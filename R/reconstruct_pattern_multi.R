#' reconstruct_pattern_multi
#'
#' @description Pattern reconstruction of a pattern marked by multiple traits.
#'
#' @param marked_pattern ppp  object with marked pattern. See Details section
#' for more information.
#' @param xr,yr Maximum extent in x and y direction of observation window.
#' @param n_repetitions Integer representing the number of simulations to be
#' performed.
#' @param max_steps Maximum number simulation steps.
#' @param no_change Integer representing the number of iterations
#' (per 1000 simulation steps) after which the reconstruction is terminated if the
#' energy does not decrease.
#' @param rcount Integer representing the number of intervals for which the
#' summary statistics are evaluated.
#' @param rmax Maximum distance [m] up to which the summary statistics are
#' evaluated.
#' @param issue Integer that determines after how many simulations steps an
#' output occurs.
#' @param divisor Choice of divisor in the estimation formula: either "r" or "d".
#' @param kernel_arg The kernel used to calculate the energy, possible kernels
#' can be: Gaussian, Epanechnikov, Rectangular, Cumulative.
#' @param timing Logical value: The computation time is measured if this is TRUE.
#' @param energy_evaluation Logical value: If this is TRUE, the procedure stores
#'  the energy shares of the total energy per simulation step.
#' @param plot Logical value: If this is TRUE, the procedure records the
#'  point pattern during optimization and updated.
#' @param Lp Distance measure for the calculation of the energy function
#' (Lp distance, 1 <= p < Inf).
#' @param bw Bandwidth [m] with which the kernels are scaled, so that this is
#' the standard deviation of the smoothing kernel.
#' @param sd This is the standard deviation [m] used in the move_coordinate action.
#' @param steps_tol After the value steps_tol it is checked whether the energy
#' change is smaller than tol.
#' @param tol Stops the procedure of energy if more than 1 - tol times no changes.
#' @param w_markcorr Vector of possible weightings of individual mcf's. (Default: all equal).
#' @param prob_of_actions Vector of probabilities for the actions performed.
#' \code{c(move_coordinate = 0.4, switch_coords = 0.1, exchange_mark_one = 0.1,
#' exchange_mark_two = 0.1, pick_mark_one = 0.2, pick_mark_two = 0.1)}.
#' @param k Vector of values k; used only if Dk is included in w_statistics.
#' @param w_statistics  vector of named weights for optional spatial statistics
#' from the \code{spatstat} package to be included in the energy calculation. This may
#' include Dk, K, Hs, pcf.
#' @param verbose Logical if progress report is printed.
#'
#' @details
#' A novel approach carries out a pattern reconstruction of marked dot patterns
#' as described by Tscheschel and Stoyan (2006) and Wiegand and Moloney (2014).
#'
#' One particular feature is the simultaneous consideration of both marks,
#' accounting for their correlation during reconstruction.
#'
#' The marked point pattern (PPP object) must is currently structured as follows:
#' X-coordinate, Y-coordinate, metric mark (e.g. diameter at breast height),
#' and nominal mark (e.g. tree species).It is calculated in the unit metre [m].
#'
#' A combination of the mark correlation function and pair correlation function
#' is used for pattern description. Additional summary statistics may be
#' considered.Two randomly selected marks are chosen in each iteration, and one
#' of various actions is performed. Changes will only be retained if the
#' difference between the observed and reconstructed pattern decreases
#' (minimizing energy).
#'
#' This method is currently only suitable for homogeneous point patterns.
#'
#' A comprehensive description of the method can be found in Wudel et al. (2023).
#'
#' @seealso
#' \code{\link{fit_point_process}} \cr
#' \code{\link{reconstruct_pattern}} \cr
#' \code{\link{reconstruct_pattern_marks}}
#'
#' @return rd_multi
#'
#' @examples
#' \dontrun{
#'
#' # Random example data set
#' xr <- 500
#' yr <- 1000
#' N <- 400
#' y <- runif(N, min = 0, max = yr)
#' x <- runif(N, min = 0, max = xr)
#'
#' species <- sample(c("A","B"), N, replace = TRUE)
#' diameter <- runif(N, 0.1, 0.4)
#'
#' random <- data.frame(x = x, y = y, dbh = diameter, species = factor(species))
#'
#' marked_pattern <- spatstat.geom::as.ppp(random, W = spatstat.geom::owin(c(0, xr), c(0, yr)))
#'
#' # Reconstruction function
#' reconstruction <- reconstruct_pattern_multi(marked_pattern, n_repetitions = 2,
#' max_steps = 10000)
#' }
#'
#' @references
#' Kirkpatrick, S., Gelatt, C.D.Jr., Vecchi, M.P., 1983. Optimization by simulated
#' annealing. Science 220, 671–680. <https://doi.org/10.1126/science.220.4598.671>
#'
#' Tscheschel, A., Stoyan, D., 2006. Statistical reconstruction of random point
#' patterns. Computational Statistics and Data Analysis 51, 859–871.
#' <https://doi.org/10.1016/j.csda.2005.09.007>
#'
#' Wiegand, T., Moloney, K.A., 2014. Handbook of spatial point-pattern analysis in
#' ecology. Chapman and Hall/CRC Press, Boca Raton. ISBN 978-1-4200-8254-8
#'
#' Wudel, C., Schlicht, R., & Berger, U. (2023). Multi-trait point pattern
#' reconstruction of plant ecosystems. Methods in Ecology and Evolution, 14, 2668–2679.
#' https://doi.org/10.1111/2041-210X.14206
#'
#' @export
reconstruct_pattern_multi <- function(marked_pattern,
                                      xr = marked_pattern$window$xrange,
                                      yr = marked_pattern$window$yrange,
                                      n_repetitions = 1,
                                      max_steps = 10000,
                                      no_change = 5,
                                      rcount = 250,
                                      rmax = 25,
                                      issue = 1000 ,
                                      divisor = "r",
                                      kernel_arg = "epanechnikov",
                                      timing = FALSE,
                                      energy_evaluation = FALSE,
                                      plot = FALSE,
                                      Lp = 1,
                                      bw = if (divisor %in% c("r","d")) 0.5 else 5,
                                      sd = "step",
                                      steps_tol = 1000,
                                      tol = 1e-4,
                                      w_markcorr = c(d_d=1, all=1, d_all=1, all_all=1, d_d0=1, all0=1, d_all0=1, all_all0=1),
                                      prob_of_actions = c(move_coordinate = 0.4, switch_coords = 0.1, exchange_mark_one = 0.1, exchange_mark_two = 0.1, pick_mark_one = 0.2, pick_mark_two = 0.1),
                                      k = 1,
                                      w_statistics= c(),
                                      verbose = TRUE) {

  # If several reconstructions are to be carried out, a list is created here in
  # which the results are then saved continuously.
  if(n_repetitions > 1) {
    names_reconstruction <- paste0("reconstruction_", seq_len(n_repetitions))
    reconstruction_list <- vector("list", length = n_repetitions)
    names(reconstruction_list) <- names_reconstruction
  }

  # Loop to perform multiple reconstructions.
  for (t in seq_len(n_repetitions)) {

    # Load the reference point pattern as a data frame with the components x, y,
    # mark, where x, y are the coordinates of the point and mark is a matrix representing the marks or their dummy values.
    p_ <- data.frame(x = marked_pattern$x, y = marked_pattern$y)

    p_$mark <- cbind(`1`= 1, diameter = marked_pattern$marks[[1]],
                     to_dummy(marked_pattern$marks[[2]]))

    marknames <- colnames(p_$mark)
    diameter  <- 2
    species   <- seq_along(marknames)[-(1:2)]

    # Check whether certain requirements are met; if not, the reconstruction is
    # aborted and an error message is displayed.
    if (is.null(marked_pattern[["marks"]])) {
      stop("'marked_pattern' must be marked", call. = FALSE)
    }

    if (!inherits(marked_pattern[["marks"]][[1]], "numeric")) {
      stop("mark one must be 'numeric', an example would be the DBH (Diameter at breast height).",
           call. = FALSE)
    }

    if (!inherits(marked_pattern[["marks"]][[2]], "factor")) {
      stop("mark two must be a 'factor', an example would be the tree species.",
           call. = FALSE)
    }

    if (n_repetitions < 1) {
      stop("n_repetitions must be at least 1 for the function to be executed.",
           call. = FALSE)
    }

    # Definition of parameters for the estimation of correlation functions.
    rmin    <- rmax / rcount
    r       <- seq(rmin, rmax, length.out = rcount)

    # Calculation of the kernels.
    sel_kernel <- select_kernel(kernel_arg, bw, rmax, divisor)
    kernel <- as.function(sel_kernel[[1]])
    rmax_bw <- as.numeric(sel_kernel[[2]])

    # get observation window of data
    obs_window <- marked_pattern$window

    # Determination of the weightings of the mark correlation functions.
    fn        <- list()
    for (i in seq_along(marknames)) for (j in seq_along(marknames)) if (i <= j) {
      fn$i <- c(fn$i,i)
      fn$j <- c(fn$j,j)
      fn$w <- c(fn$w,
        if (i     == diameter && j == diameter) w_markcorr["d_d"]
        else if(i == 1 || j      ==1) w_markcorr["all"]
        else if(i == diameter || j == diameter) w_markcorr["d_all"]
        else w_markcorr["all_all"])
      fn$w0<-c(fn$w0,
        if (i     == diameter && j == diameter) w_markcorr["d_d0"]
        else if(i == 1 || j      == 1) w_markcorr["all0"]
        else if(i == diameter || j == diameter) w_markcorr["d_all0"]
        else w_markcorr["all_all0"])
    }
    names(fn$i) <- marknames[fn$i]
    names(fn$j) <- marknames[fn$j]

    # Defines the initial state of the new ponit pattern.
    xwr <- obs_window$xrange
    ywr <- obs_window$yrange
    p <- p_[sample.int(nrow(p_),ceiling(nrow(p_)*((diff(xwr)*diff(ywr))/(diff(xr)*diff(yr)))),TRUE), ]# rpois
    p$x <- stats::runif(nrow(p), xwr[1], xwr[2])
    p$y <- stats::runif(nrow(p), ywr[1], ywr[2])
    p$mark[, diameter] <- stats::quantile(p_$mark[, diameter],
                                          probs = stats::runif(nrow(p), 0, 1), type = 4)
    p$mark[, species] <- p_$mark[, species, drop = FALSE][
    sample.int(nrow(p_), nrow(p), replace = TRUE),, drop = FALSE]

    # Calculates the functions for the reference and the new dot pattern as well as calculating the "energy" that measures their distance.
    f_         <- calc_moments_full(fn, p_, kernel, rmax_bw, r)
    f0_        <- colSums(p_$mark[, fn$i] * p_$mark[, fn$j])
    names(f0_) <- rownames(f_)
    statistics_<- compute_statistics(p_$x, p_$y, k, xr, yr, w_statistics, bw, divisor, kernel_arg, r)
    f          <- calc_moments_full(fn, p, kernel, rmax_bw, r)
    f0         <- colSums(p$mark[, fn$i] * p$mark[, fn$j])
    names(f0)  <- rownames(f)
    statistics <- compute_statistics(p$x, p$y, k, xwr, ywr, w_statistics, bw, divisor, kernel_arg, r)

    # Prepare the graphical output.
    if(plot) {
      graphics::par(mfrow = 1:2)
      num_species <- from_dummy(p_$mark[, species, drop = FALSE])
      graphics::plot(y~x, p_, pch=19, col= 2L + as.integer(num_species),
                     cex = 1.3 + 4 * mark[, diameter], xlim = xr, ylim = yr,
                     xaxs ="i", yaxs ="i", main ="Reference", xlab ="x [m]", ylab ="y [m]")
      graphics::text(p_$x, p_$y, as.integer(num_species), cex=0.7)

      graphics::plot(y~x, p, type = "n", xlim = xwr, ylim = ywr, xaxs = "i",
                     yaxs = "i", main = "Reconstructed", xlab = "x [m]", ylab = "y [m]")
      graphics::clip(xwr[1], xwr[2], ywr[1], ywr[2])
    }

    # Show warning if certain distances between pairs of trees are not present.
    energy  <- Energy_fun(f, f0, statistics, f_, f0_, statistics_, fn, p, p_, Lp, w_statistics)["energy"]

    # Prepares variables for the storage of progress.
    energy_launch            <- as.vector(energy)
    energy_course            <- data.frame(i = seq(from = 1, to = max_steps,by = 1),
                                           energy = NA)
    energy_improvement       <- 0L
    number_of_actions        <- integer(0)
    no_change_energy         <- Inf
    no_change_counter        <- 0L
    step_list                <- integer(0)
    action_list              <- character(0)
    Energy_list              <- numeric(0)
    number_of_actions_with_energy_improvement <- integer(0)

    # loop to improve the newly created dot pattern by reducing its energy.
    step <- 0L
    system.time(repeat {

      energy_course[step, 2] <- energy

      # Updating the graphical output of all "issue" steps.
      if (step %% issue  == 0) {
        if(plot) {
          graphics::rect(xwr[1], ywr[1], xwr[2], ywr[2], col="white")
          num_species <- from_dummy(p$mark[, species, drop = FALSE])

          graphics::points(y~x, p, pch = 19, col = 2L + as.integer(num_species), cex = 1.3 + 4 * mark[, diameter])
          graphics::text(p$x, p$y, as.integer(num_species), cex = 0.7)
        }

        if (verbose) {
          # Generates text output with the current calculated values (for example the energy), this is updated every "issue" simulation step.
          message("\r> Progress: ", if(n_repetitions > 1) names_reconstruction[[t]], " || iterations: ", step,
                  " || Simulation progress: ", floor(step/max_steps * 100), "%",
                  " || energy = ", round(energy, 5), " || energy improvement = ",
                  energy_improvement, "\t\t\r", appendLF = FALSE)

          Sys.sleep(0)
        }
      }

      # the next code block aborts the reconstruction if the energy decreases by less than tol in "no_change" intervals of steps_tol simulation steps.
      if (step %% steps_tol  == 0) {
        if(energy > no_change_energy * (1-tol)) {
          no_change_counter <- no_change_counter + 1L
          if(no_change_counter == no_change) {
            if (verbose) {
              message("the simulation was terminated, because the energy did not decrease in ", no_change * issue, " simulation steps.")
            }
            stop_criterion <- "no_change"
            break
          }
        } else {
          no_change_counter<-0L
          stop_criterion<-"max_steps"
        }
        no_change_energy <- energy
      }

      if (step < max_steps) step <- step + 1 else break
      action <- sample(c("move_coordinate", "switch_coords", "pick_mark_one",
                         "pick_mark_two", "exchange_mark_one", "exchange_mark_two"),
                         1, prob = prob_of_actions)
      number_of_actions[action] <- (if (is.na(number_of_actions[action])) 0L else number_of_actions[action]) + 1L

      statistics.new <- statistics

      # Switch selection for the possible changes to the reconstructed point pattern for energy minimisation (probabilities of how often an action is taken: 40%, 10%, 20%, 10%, 10%).
      switch(action,
        # Displacement of coordinates of a point in the new point pattern, is applied in xx% of the cases.
        move_coordinate = {
          i     <- sample.int(nrow(p), 1, replace = TRUE)
          s     <- if(sd=="step") nrow(p) * 1 / step else sd
          x     <- stats::rnorm(1, p$x[i], diff(xwr) * s) %% xwr[2]
          y     <- stats::rnorm(1, p$y[i], diff(ywr) * s) %% ywr[2]
          mdiff <- p$mark[i, ]
          f.new <- f - calc_moments(fn, p, i, p$x[i], p$y[i], mdiff,
                                    kernel, rmax_bw, r) +
            calc_moments(fn, p, i, x, y, mdiff, kernel, rmax_bw, r)
          f0.new <- f0
          statistics.new <- compute_statistics(replace(p$x, i, x), replace(p$y, i, y), k, xwr, ywr, w_statistics, bw, divisor, kernel_arg, r)
        },
        # Swaps the coordinates of two randomly drawn points from the new point pattern, applied in xx% of the trap.
        switch_coords = {
          i             <- sample.int(nrow(p), 2, replace = FALSE)
          mdiff         <- p$mark[i[1], ] - p$mark[i[2], ]
          f.new         <- f - calc_moments(fn, p, i, p$x[i[1]], p$y[i[1]],
                                            mdiff,
                                            kernel, rmax_bw, r) +
            calc_moments(fn, p, i, p$x[i[2]],
                         p$y[i[2]], mdiff, kernel, rmax_bw, r)
          f0.new<- f0
        },
        # Displacement of coordinates of a point in the new point pattern, applied in xx% of the cases.
        exchange_mark_one = {
          i                 <- sample.int(nrow(p), 2, replace = FALSE)
          m                 <- p$mark[i, ]
          m[, diameter]     <- m[2:1, diameter]
          mdiff             <- m[1, ] - p$mark[i[1], ]
          q                 <- p[i, ]
          q$mark[1, ]       <- m[1, ]
          f.new             <- f  + calc_moments(fn, p, i[1], p$x[i[1]],
                                                 p$y[i[1]], mdiff, kernel,
                                                 rmax_bw, r) -
            calc_moments(fn, p, i, p$x[i[2]], p$y[i[2]],
                         mdiff, kernel, rmax_bw, r) -
            calc_moments(fn, q, 2, q$x[2], q$y[2], mdiff,
                         kernel, rmax_bw, r)
          f0.new            <- f0 + m[1,fn$i] * m[1, fn$j] -
            p$mark[i[1], fn$i] * p$mark[i[1], fn$j] + m[2, fn$i] *
            m[2, fn$j] - p$mark[i[2], fn$i] *
            p$mark[i[2], fn$j]
        },
        # Swaps the type assignment of two randomly drawn points from the new point
        # pattern, applied in 10% of the trap.
        exchange_mark_two = {
          i                 <- sample.int(nrow(p), 2, replace = FALSE)
          m                 <- p$mark[i, ]
          m[, species]      <- m[2:1, species]
          mdiff             <- m[1, ] - p$mark[i[1], ]
          q                 <- p[i, ]
          q$mark            <- m

          f.new             <- f +  calc_moments(fn, p, i[1], p$x[i[1]], p$y[i[1]],
                                                mdiff, kernel, rmax_bw, r) -
                                    calc_moments(fn, p, i, p$x[i[2]], p$y[i[2]],
                                                mdiff, kernel, rmax_bw, r) -
                                    calc_moments(fn, q, 2, q$x[2], q$y[2], mdiff,
                                               kernel, rmax_bw, r)
          f0.new            <- f0 + m[1, fn$i] * m[1, fn$j] - p$mark[i[1], fn$i] *
                                    p$mark[i[1], fn$j] + m[2, fn$i] * m[2, fn$j] -
                                    p$mark[i[2], fn$i] * p$mark[i[2], fn$j]
        },
        # If the distribution (continuous function) of the diameter of the reference pattern generates a randomly drawn value for a randomly selected point in the new point pattern, the trap is applied in 20%.
        pick_mark_one = {
          i     <- sample.int(nrow(p), 1, replace = TRUE)
          m     <- p$mark[i, ]
          m[diameter] <- stats::quantile(p_$mark[,diameter],probs = stats::runif(1,0,1),
                                 type = 4)
          mdiff <- m - p$mark[i, ]
          f.new <- f + calc_moments(fn, p, i, p$x[i], p$y[i], mdiff,
                                    kernel, rmax_bw, r)
          f0.new<- f0 + m[fn$i] * m[fn$j] - p$mark[i, fn$i] *
            p$mark[i, fn$j]
        },
        # Draws a random value for a point from the new point pattern from the type distribution (discrete function) of the reference pattern, is applied in 10% of the traps.
        pick_mark_two = {
          i          <- sample.int(nrow(p), 1, replace = TRUE)
          j          <- sample.int(nrow(p_), 1, replace = TRUE)
          m          <- p$mark[i, ]
          m[species] <- p_$mark[j, species]
          mdiff      <- m - p$mark[i, ]
          f.new      <- f + calc_moments(fn, p, i, p$x[i], p$y[i], mdiff,
                                         kernel, rmax_bw, r)
          f0.new     <- f0 + m[fn$i] * m[fn$j] - p$mark[i, fn$i] *
            p$mark[i, fn$j]
        },
        # return error
        stop("undefined case")
      )

      # calculates the energy
      Energy <- Energy_fun(f.new, f0.new, statistics.new, f_, f0_, statistics_, fn, p, p_, Lp, w_statistics)

      energy.new<-Energy[["energy"]]

      # Sets the currently calculated energy as the new reference value if it is less than the previous energy.
      if(energy.new >= energy) next
      f  <- f.new
      f0 <- f0.new
      statistics <- statistics.new
      energy <- energy.new

      # Executes the previously defined actions.
      switch(action,
        # move
        move_coordinate = {
          p$x[i] <- x
          p$y[i] <- y
        },
        # switch
        switch_coords = {
          p$x[i] <- p$x[rev(i)]
          p$y[i] <- p$y[rev(i)]
        },
        pick_mark_one     =,
        pick_mark         =,
        pick_mark_two     =,
        exchange_mark_one =,
        exchange_mark_two = {
          p$mark[i, ] <- m
        },
        # no action defined
        stop("undefined case")
      )

      # Saves the intermediate results and increases running numbers.
      if (energy_evaluation == TRUE) {
        step_list <-c(step_list,step)
        action_list <-c(action_list, action)
        Energy_list <-rbind(Energy_list, Energy)
        number_of_actions_with_energy_improvement[action] <-
          (if (is.na(number_of_actions_with_energy_improvement[action]))
            0L else number_of_actions_with_energy_improvement[action]) + 1L
      }
      energy_improvement <- energy_improvement + 1L

    # End of reconstruction loop.
    }) -> process.time

    if (verbose) message("\n")

    # Saves all results Transfers them to the "reconstruction" list.
    p_$species <- from_dummy(p_$mark[, species, drop = FALSE])
    p$species  <- from_dummy(p$mark[, species, drop = FALSE])
    method            <- "Reconstruction of a homogeneous point pattern"
    Parameter_setting <- list(n_repetitions=n_repetitions, max_steps=max_steps,
                              no_change=no_change, rcount=rcount, rmax=rmax,
                              issue=issue, divisor=divisor, kernel_arg=kernel_arg,
                              timing=timing, energy_evaluation=energy_evaluation,
                              plot=plot, Lp=Lp, k=k, bw=bw, sd=sd,
                              prob_of_actions=prob_of_actions,
                              w_markcorr=w_markcorr, w_statistics=w_statistics)
    iterations        <- step
    energy_current    <- energy_course[step, 2]
    adapted_p_<- data.frame(p_$x, p_$y, p_$mark[,2], p_$species)
    colnames(adapted_p_)<-c("x", "y", "diameter", "species")
    adapted_p<- data.frame(p$x, p$y, p$mark[,2], p$species)
    colnames(adapted_p)<-c("x", "y", "diameter", "species")
    win_change<- if (xr[1] != xwr[1] | xr[2] != xwr[2] | yr[1] != ywr[1] | yr[2] != ywr[2]) {TRUE}else{FALSE}

    # Saves the results.
    reconstruction <-
      list(reference                                 = adapted_p_,
           reconstructed                             = adapted_p,
           window                                    = c(xr, yr),
           obs_window                                =
             if (win_change == TRUE) {
              c(xwr, ywr)
             },
           r                                         = r,
           f_reference                               = f_,
           f_reconstructed                           = f,
           Parameter_setting                         = Parameter_setting,
           method                                    = method,
           stop_criterion                            = stop_criterion,
           iterations                                = step,
           simulation_time                           =
             if (timing == TRUE) {
              paste(round(process.time[3], 2), "s")
             },
           energy_launch                             = energy_launch,
           energy_course                             = energy_course,
           energy_current                            = energy_current,
           energy_improvement                        = energy_improvement,
           number_of_actions                         =
             if(energy_evaluation == TRUE) {
               number_of_actions
             },
           number_of_actions_with_energy_improvement =
             if(energy_evaluation == TRUE) {
               number_of_actions_with_energy_improvement
             },
           energy_details                            =
             if(energy_evaluation == TRUE) {
               data.frame(step_list, action_list, Energy_list)
             })

    if (!(timing && energy_evaluation && win_change)) {
      reconstruction <- reconstruction[-which(sapply(reconstruction, is.null))]
    }

    # Adds the results of further reconstructions to the "reconstruction" list if several are performed.
    if (n_repetitions > 1) {
      reconstruction_list[[t]] <- reconstruction
    }
  }

  if(n_repetitions > 1) {
    class(reconstruction_list) <- "rd_multi"
    return(reconstruction_list)
  } else {
    class(reconstruction) <- "rd_multi"
    return(reconstruction)
  }
}
