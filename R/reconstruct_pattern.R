#' reconstruct_pattern
#'
#' @description Pattern reconstruction
#'
#' @param pattern ppp.
#' @param n_random Number of randomizations.
#' @param e_threshold Minimum energy to stop reconstruction.
#' @param max_runs Maximum number of iterations of e_threshold is not reached.
#' @param fitting It true, the pattern reconstruction starts with a fitting of a Thomas process.
#' @param comp_fast Should summary functions be estimated in an computational fast way.
#' @param return_input The original input data is returned as last list entry
#' @param simplify If n_random = 1 and return_input = FALSE only pattern will be returned.
#' @param verbose Print progress report.
#' @param plot Plot pcf function during optimization.
#'
#' @details
#' The functions randomizes the observed pattern by using pattern reconstruction
#' as described in Tscheschel & Stoyan (2006) and Wiegand & Moloney (2014). The
#' algorithm starts with a random reconstructed pattern, shifts a point to a new location and
#' keeps the change only, if the deviation between the observed and the reconstructed
#' pattern decreases. The pair correlation function and the nearest neighbour
#' distance function are used to describe the patterns. For large patterns
#' `comp_fast = TRUE` decreases the computational demand because no edge
#' correction is used and the pair correlation function is estimated based on Ripley's
#' K-function. For more information see \code{\link{estimate_pcf_fast}}.
#'
#' @seealso
#' \code{\link{calculate_energy}} \cr
#' \code{\link{plot_randomized_pattern}}
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' pattern_recon <- reconstruct_pattern(species_b, n_random = 19, max_runs = 1000)
#' }
#'
#' @aliases reconstruct_pattern
#' @rdname reconstruct_pattern
#'
#' @references
#' Tscheschel, A., & Stoyan, D. (2006). Statistical reconstruction of random point
#' patterns. Computational Statistics and Data Analysis, 51(2), 859–871.
#'
#' Wiegand, T., & Moloney, K. A. (2014). Handbook of spatial point-pattern analysis
#' in ecology. Boca Raton: Chapman and Hall/CRC Press.
#'
#' @export
reconstruct_pattern <- function(pattern, n_random = 19,
                                e_threshold = 0.01, max_runs = 10000,
                                fitting = FALSE, comp_fast = FALSE,
                                return_input = TRUE,
                                simplify = FALSE,
                                verbose = TRUE,
                                plot = FALSE){

  # check if n_random is >= 1
  if(!n_random >= 1) {
    stop("n_random must be >= 1.", call. = FALSE)
  }

  pattern <- spatstat::unmark(pattern) # only spatial points

  # start with fitted pattern
  if(fitting){

    # fit Thomas process
    fitted_process <- spatstat::kppm(pattern, cluster = "Thomas",
                                     statistic = "pcf",
                                     statargs = list(divisor = "d",
                                                     correction = "best"),
                                     method = "mincon",
                                     improve.type = "none")
  }

  # create n_random recondstructed patterns
  result <- lapply(seq_len(n_random), function(current_pattern){

    # fit a Thomas process to the data
    if(fitting){

      # simulte clustered pattern
      simulated <- spatstat::simulate.kppm(fitted_process,
                                           nsim = 1, drop = TRUE,
                                           window = pattern$window)

      # remove points because more points in simulated
      if(pattern$n < simulated$n) {

        # difference between patterns
        difference <- simulated$n - pattern$n

        # id of points to remove
        remove_points <- sample(seq_len(simulated$n), size = difference)

        # remove points
        simulated <- simulated[-remove_points]
      }

      # add points because less points in simulated
      if(pattern$n > simulated$n) {

        # difference between patterns
        difference <- pattern$n - simulated$n

        # create missing points
        missing_points <- spatstat::runifpoint(n = difference,
                                               nsim = 1, drop = TRUE,
                                               win = pattern$window)

        # add missing points to simulated
        simulated <- spatstat::superimpose(simulated, missing_points,
                                           W = pattern$window)
      }
    }

    # create Poisson simulation data
    else {
      simulated <- spatstat::runifpoint(n = pattern$n,
                                        nsim = 1, drop = TRUE,
                                        win = pattern$window)
    }

    # fast computation of summary functions
    if(comp_fast) {

      gest_observed <- spatstat::Gest(pattern, correction = "none")

      gest_simulated <- spatstat::Gest(simulated, correction = "none")

      pcf_observed <- shar::estimate_pcf_fast(pattern,
                                              correction = "none",
                                              method = "c",
                                              spar = 0.5)

      pcf_simulated <- shar::estimate_pcf_fast(simulated,
                                               correction = "none",
                                               method = "c",
                                               spar = 0.5)
    }

    # normal computation of summary functions
    else {

      gest_observed <- spatstat::Gest(X = pattern, correction = "han")

      gest_simulated <- spatstat::Gest(X = simulated, correction = "han")

      pcf_observed <- spatstat::pcf(X = pattern, correction = "best", divisor = "d")

      pcf_simulated <- spatstat::pcf(X = simulated, correction = "best", divisor = "d")
    }

    # energy before reconstruction
    e0 <- mean(abs(gest_observed[[3]] - gest_simulated[[3]]), na.rm = TRUE) +
      mean(abs(pcf_observed[[3]] - pcf_simulated[[3]]), na.rm = TRUE)

    # random ids of pattern
    rp_id <- sample(x = seq_len(pattern$n),
                    size = max_runs, replace = TRUE)

    # create random new points
    rp_coords <- spatstat::runifpoint(n = max_runs,
                                      nsim = 1, drop = TRUE,
                                      win = pattern$window)

    # pattern reconstruction algorithm (optimaztion of e0) - not longer than max_runs
    for(i in seq_len(max_runs)){

      relocated <- simulated # data for relocation

      rp_id_current <- rp_id[[i]] # get current point id

      # relocate point
      relocated$x[[rp_id_current]] <- rp_coords$x[[i]]

      relocated$y[[rp_id_current]] <- rp_coords$y[[i]]

      # calculate summary functions after relocation
      if(comp_fast) {

        gest_relocated <- spatstat::Gest(relocated, correction = "none")

        pcf_relocated <- shar::estimate_pcf_fast(relocated,
                                                 correction = "none",
                                                 method = "c",
                                                 spar = 0.5)
      }

      else {

        gest_relocated <- spatstat::Gest(X = relocated, correction = "han")

        pcf_relocated <- spatstat::pcf(X = relocated, correction = "best", divisor = "d")
      }

      # energy after relocation
      e_relocated <- mean(abs(gest_observed[[3]] - gest_relocated[[3]]), na.rm = TRUE) +
        mean(abs(pcf_observed[[3]] - pcf_relocated[[3]]), na.rm = TRUE)

      # lower energy after relocation
      if(e_relocated < e0){

        simulated <- relocated # keep relocated pattern

        e0 <- e_relocated # keep e_relocated as e0

        # plot observed vs reconstructed
        if(plot) {

          Sys.sleep(0.1) # https://support.rstudio.com/hc/en-us/community/posts/200661917-Graph-does-not-update-until-loop-completion

          graphics::plot(x = pcf_observed[[1]], y = pcf_observed[[3]],
                         type = "l", col = "black",
                         xlab = "r", ylab = "g(r)")

          graphics::lines(x = pcf_relocated[[1]], y = pcf_relocated[[3]], col = "red")

          graphics::legend("topright",
                           legend = c("observed", "reconstructed"),
                           col = c("black", "red"),
                           lty = 1, inset = 0.025)
        }
      }

      # print progress
      if(verbose) {
        message("\r> Progress: n_random: ", current_pattern, "/", n_random,
                " || max_runs: ", i, "/", max_runs,
                " || e0 = ", round(e0, 5), appendLF = FALSE)
      }

      # exit loop if e threshold is reached
      if(e0 <= e_threshold){
        break
      }
    }

    return(simulated)
  })

  # add input pattern to randomizations
  if(return_input){

    if(verbose & simplify){
      message("\n")
      warning("'simplify = TRUE' not possible for 'return_input = TRUE'.", call. = FALSE)
    }

    result[[n_random + 1]] <- pattern # add input pattern as last list entry

    names(result) <-  c(paste0("randomized_", seq_len(n_random)), "observed") # set names
  }

  else{

    if(simplify) {

      if(verbose & n_random > 1) {
        message("\n")
        warning("'simplify = TRUE' not possible for 'n_random > 1'.", call. = FALSE)
      }

      else {
        result <- result[[1]]
      }
    }

    else{
      names(result) <- paste0("randomized_", seq_len(n_random)) # set names
    }
  }

  # write result in new line if progress was printed
  if(verbose) {
    message("\r")
  }

  return(result)
}
