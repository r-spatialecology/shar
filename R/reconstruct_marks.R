#' reconstruct_marks
#'
#' @description Pattern reconstruction of marks
#'
#' @param pattern ppp.
#' @param n_random Number of randomizations.
#' @param e_threshold Minimum energy to stop reconstruction.
#' @param max_runs Maximum number of iterations of e_threshold is not reached.
#' @param return_input The original input data is returned as last list entry
#' @param verbose Print progress report.
#' @param plot Plot mark function during optimization
#'
#' @details
#'
#'
#' @seealso
#' \code{\link{reconstruct_pattern}} \cr
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' species_a
#' species_b
#'
#' pattern_recon <- reconstruct_marks(species_b, n_random = 39, max_runs = 1000)
#' }
#'
#' @aliases reconstruct_marks
#' @rdname reconstruct_marks
#'
#' @references
#' Tscheschel, A., & Stoyan, D. (2006). Statistical reconstruction of random point
#' patterns. Computational Statistics and Data Analysis, 51(2), 859–871.
#'
#' Wiegand, T., & Moloney, K. A. (2014). Handbook of spatial point-pattern analysis
#' in ecology. Boca Raton: Chapman and Hall/CRC Press.
#'
#' @export
reconstruct_marks <- function(pattern,
                              marked_pattern,
                              mark,
                              n_random = 19,
                              e_threshold = 0.01, max_runs = 10000,
                              return_input = TRUE,
                              verbose = FALSE,
                              plot = FALSE){

  # check if n_random is >= 1
  if(!n_random >= 1) {
    stop("n_random must be >= 1.", call. = FALSE)
  }

  #
  if(any(pattern$window$xrange != marked_pattern$window$xrange) ||
     any(pattern$window$yrange != marked_pattern$window$yrange) ||
     pattern$n != marked_pattern$n ||
     spatstat::is.marked(pattern)) {
    stop("'pattern' and 'pattern' must have same window and n & 'pattern' must be unmarked",
         call. = FALSE)
  }

  # check if marks are data frame
  if(spatstat::markformat(marked_pattern) == "dataframe") {

    # only use selected mark
    marked_pattern$marks <- marked_pattern$marks[[mark]]
  }

  if(class(marked_pattern$marks) == "numeric") {

    summary_function <- get("markcorr", mode = "function")
  }

  else if(class(marked_pattern$marks) == "factor") {
    summary_function <- get("markcon", mode = "function")

  }

  else{

    stop("marks must bei either 'numeric' or 'factor'",
         call. = FALSE)
  }



  # create n_random recondstructed patterns
  result <- lapply(1:n_random, function(current_pattern){

    # create random simulation data (shuffle marks)
    # spatstat::rlabel()

    # calculate summary functions

    # energy before reconstruction
    # e0 <- mean(abs(gest_observed[[3]] - gest_simulated[[3]]), na.rm = TRUE) +
    #   mean(abs(pcf_observed[[3]] - pcf_simulated[[3]]), na.rm = TRUE)

    # pattern reconstruction algorithm (optimaztion of e0) - not longer than max_runs
    for(i in 1:max_runs){

      relocated <- simulated # data for relocation

      rp <- sample(x = 1:relocated$n, size = 1) # random point of pattern

      # switch marks
      # relocated$x[rp] <- stats::runif(n = 1, min = xrange[1], max = xrange[2])
      #
      # relocated$y[rp] <- stats::runif(n = 1, min = yrange[1], max = yrange[2])

      # calculate summary functions after relocation

      # energy after relocation
      # e_relocated <- mean(abs(gest_observed[[3]] - gest_relocated[[3]]), na.rm = TRUE) +
      #   mean(abs(pcf_observed[[3]] - pcf_relocated[[3]]), na.rm = TRUE)

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
        cat(paste0("\rProgress: n_random: ", current_pattern, "/", n_random,
                   " || max_runs: ", i, "/", max_runs,
                   " || e0 = ", round(e0, 5)))
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

    result[[n_random + 1]] <- pattern # add input pattern as last list entry

    names(result) <-  c(paste0("randomized_", 1:n_random), "observed") # set names
  }

  else{

    names(result) <- paste0("randomized_", 1:n_random) # set names
  }

  return(result)
}
