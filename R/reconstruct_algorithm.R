#' reconstruct_algorithm
#'
#' @description Pattern reconstruction (internal)
#'
#' @param pattern ppp object with pattern.
#' @param n_random Integer with number of randomizations.
#' @param e_threshold Double with minimum energy to stop reconstruction.
#' @param max_runs Integer with maximum number of iterations if \code{e_threshold}
#' is not reached.
#' @param no_change Integer with number of iterations at which the reconstruction will
#' stop if the energy does not decrease.
#' @param annealing Double with probability to keep relocated point even if energy
#' did not decrease.
#' @param weights Vector with weights used to calculate energy.
#' The first number refers to Gest(r), the second number to pcf(r).
#' @param r_length Integer with number of intervals from \code{r = 0} to \code{r = rmax} for which
#' the summary functions are evaluated.
#' @param r_max Double with maximum distance used during calculation of summary functions. If \code{NULL},
#' will be estimated from data.
#' @param stoyan Coefficient for Stoyan's bandwidth selection rule.
#' @param verbose Logical if progress report is printed.
#' @param plot Logical if pcf(r) function is plotted and updated during optimization.
#'
#' @return list
#'
#' @keywords internal
reconstruct_algorithm <- function(pattern,
                                  method,
                                  n_random,
                                  e_threshold,
                                  max_runs,
                                  no_change,
                                  annealing,
                                  weights,
                                  r_length,
                                  r_max,
                                  stoyan,
                                  verbose,
                                  plot){

  # check if n_random is >= 1
  if (n_random < 1) {
    stop("n_random must be >= 1.", call. = FALSE)
  }

  # unmark pattern
  if (spatstat.geom::is.marked(pattern)) {

    pattern <- spatstat.geom::unmark(pattern)

    if (verbose) message("Unmarking provided input pattern. For marked pattern, see reconstruct_pattern_marks().")

  }

  # grab window and number of points
  n_points <- pattern$n
  window <- pattern$window

  # check if pattern is emtpy
  if (n_points == 0) stop("The observed pattern contains no points.", call. = FALSE)

  # calculate intensity
  lambda <- n_points / spatstat.geom::area(window)
  lambda2area <- (n_points * (n_points - 1)) / spatstat.geom::area(window)

  # calculate bandwidth using
  h <- stoyan / sqrt(lambda)
  bw <- h / sqrt(5)

  # calculate r from data
  if (is.null(r_max)) {

    r <- seq(from = 0, to = spatstat.explore::rmax.rule(W = window, lambda = lambda),
             length.out = r_length)

    r_max <- max(r)

    # use provided r_max
  } else {

    r <- seq(from = 0, to = r_max, length.out = r_length)

  }

  # create list with arguments fpr pcf
  denargs <- list(kernel = "epanechnikov", bw = bw, n = r_length, from = 0, to = r_max)

  # set names of randomization randomized_1 ... randomized_n
  names_randomization <- paste0("randomized_", seq_len(n_random))

  # create empty lists for results
  energy_list <- vector("list", length = n_random)
  iterations_list <- vector("list", length = n_random)
  stop_criterion_list <- as.list(rep("max_runs", times = n_random))
  result_list <- vector("list", length = n_random)

  # set names
  names(energy_list) <- names_randomization
  names(iterations_list) <- names_randomization
  names(stop_criterion_list) <- names_randomization
  names(result_list) <- names_randomization

  # calculate summary functions
  dist_observed <- get_dist_pairs(X = pattern, rmax = r_max)

  gest_observed <- calc_gest(dist = dist_observed, r = r, n_points = pattern$n)

  pcf_observed <- spatstat.explore::sewpcf(d = dist_observed[, 3], w = 1, denargs = denargs,
                                           lambda2area = lambda2area, divisor = "d")

  # create n_random recondstructed patterns
  for (i in seq_len(n_random)) {

    # simulated starting pattern for reconstruction
    if (method == "homo") {

      # create Poisson simulation data
      simulated <- spatstat.random::runifpoint(n = n_points, nsim = 1, drop = TRUE,
                                               win = window, warn = FALSE)

    } else if (method == "hetero") {

      # estimate lambda(x,y)
      lambda_xy <- spatstat.explore::density.ppp(pattern)

      # create starting pattern
      simulated <- spatstat.random::rpoint(n = n_points, f = lambda_xy,
                                           win = pattern$window)


    } else if (method == "cluster") {

      # start with fitted Thomas cluster model
      cluster_ppm <- spatstat.model::kppm.ppp(pattern, cluster = "Thomas", statistic = "pcf",
                                              statargs = list(divisor = "d", correction = "best"),
                                              improve.type = "none")

      # simulate clustered pattern
      simulated <- spatstat.model::simulate.kppm(cluster_ppm, nsim = 1, drop = TRUE,
                                                 window = window, verbose = FALSE)

      # remove points because more points in simulated
      if (n_points < simulated$n) {

        # difference between patterns
        difference <- simulated$n - n_points

        # id of points to remove
        remove_points <- sample(x = seq_len(simulated$n), size = difference)

        # remove points
        simulated <- simulated[-remove_points]

      # add points because less points in simulated
      } else if (n_points > simulated$n) {

        # difference between patterns
        difference <- n_points - simulated$n

        # create missing points
        missing_points <- spatstat.random::runifpoint(n = difference, nsim = 1, drop = TRUE,
                                                      win = pattern$window, warn = FALSE)

        # add missing points to simulated
        simulated <- spatstat.geom::superimpose.ppp(simulated, missing_points,
                                                    W = pattern$window, check = FALSE)

      }
    }

    # check if simulated is empty
    if (simulated$n == 0) stop("The simulated pattern contains no points.", call. = FALSE)

    # check if simulated has same points as observed
    if (n_points != simulated$n) stop("The simulated pattern contains the same amount as observed pattern.", call. = FALSE)

    # energy before reconstruction
    energy <- Inf

    # counter if energy changed
    energy_counter <- 0

    # df for energy
    energy_df <- data.frame(i = seq(from = 1, to = max_runs, by = 1), energy = NA)

    # create random number for annealing prob
    if (annealing != 0) {

      random_annealing <- stats::runif(n = max_runs, min = 0, max = 1)

    } else {

      random_annealing <- rep(0, max_runs)

    }

    # random ids of pattern
    rp_id <- sample(x = seq_len(simulated$n), size = max_runs, replace = TRUE)

    # create random new points
    # MH: This could use same method as simulated?
    rp_coords <- spatstat.random::runifpoint(n = max_runs, nsim = 1, drop = TRUE,
                                             win = simulated$window, warn = FALSE)

    # # MH: Use this distance to check what changed
    # dist_simulated <- get_dist_pairs(X = simulated, rmax = r_max)

    # pattern reconstruction algorithm (optimization of energy) - not longer than max_runs
    for (j in seq_len(max_runs)) {

      # data for relocation
      relocated <- simulated

      # get current point id
      id_current <- rp_id[[j]]

      # relocate point
      relocated$x[[id_current]] <- rp_coords$x[[j]]

      relocated$y[[id_current]] <- rp_coords$y[[j]]

      # calculate summary functions relocation

      dist_relocated <- get_dist_pairs(X = relocated, rmax = r_max)

      gest_relocated <- calc_gest(dist = dist_relocated, r = r, n_points = n_points)

      pcf_relocated <- spatstat.explore::sewpcf(d = dist_relocated[, 3], w = 1, denargs = denargs,
                                                lambda2area = lambda2area, divisor = "d")

      # energy after relocation
      energy_relocated <- (mean(abs(gest_observed[, 2] - gest_relocated[, 2]), na.rm = TRUE) * weights[[1]]) +
        (mean(abs(pcf_observed[, 2] - pcf_relocated[, 2]), na.rm = TRUE) * weights[[2]])

      # lower energy after relocation
      if (energy_relocated < energy || random_annealing[j] < annealing) {

        # keep relocated pattern
        simulated <- relocated

        # keep energy_relocated as energy
        energy <- energy_relocated

        # set counter since last change back to 0
        energy_counter <- 0

        # plot observed vs reconstructed
        if (plot) {

          # https://support.rstudio.com/hc/en-us/community/posts/200661917-Graph-does-not-update-until-loop-completion
          Sys.sleep(0.01)

          graphics::plot(x = pcf_observed$r, y = pcf_observed$g,
                         type = "l", col = "black", xlab = "r", ylab = "g(r)")

          graphics::abline(h = 1, lty = 2, col = "grey")

          graphics::lines(x = pcf_relocated$r, y = pcf_relocated$g, col = "red")

          graphics::legend("topright", legend = c("observed", "reconstructed"),
                           col = c("black", "red"), lty = 1, inset = 0.025)

        }

      # increase counter no change
      } else {

        energy_counter <- energy_counter + 1

      }

      # save energy in data frame
      energy_df[j, 2] <- energy

      # print progress
      if (verbose) {

        message("\r> Progress: n_random: ", i, "/", n_random,
                " || max_runs: ", floor(j / max_runs * 100), "%",
                " || energy = ", round(energy, 5), "\t\t",
                appendLF = FALSE)

      }

      # exit loop if e threshold or no_change counter max is reached
      if (energy <= e_threshold || energy_counter > no_change) {

        # set stop criterion due to energy
        stop_criterion_list[[i]] <- ifelse(test = energy <= e_threshold,
                                           yes = "e_threshold", no = "no_change")

        break

      }
    }

    # close plotting device
    if (plot) {

      grDevices::dev.off()

    }

    # remove NAs if stopped due to energy
    if (stop_criterion_list[[i]] %in% c("e_threshold", "no_change")) {

      energy_df <- energy_df[1:j, ]

    }

    # save results in lists
    energy_list[[i]] <- energy_df
    iterations_list[[i]] <- j
    result_list[[i]] <- simulated

  }

  # write result in new line if progress was printed
  if (verbose) message("\r")

  # combine to one list
  reconstruction <- list(randomized = result_list, observed = pattern,
                         method = method, energy_df = energy_list,
                         stop_criterion = stop_criterion_list,
                         iterations = iterations_list)

  return(reconstruction)
}
