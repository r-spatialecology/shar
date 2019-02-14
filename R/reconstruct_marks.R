#' reconstruct_marks
#'
#' @description Pattern reconstruction of marks
#'
#' @param pattern ppp.
#' @param marked_pattern ppp (marked; see details).
#' @param n_random Number of randomizations.
#' @param e_threshold Minimum energy to stop reconstruction.
#' @param max_runs Maximum number of iterations of e_threshold is not reached.
#' @param no_change Reconstrucction will stop if energy does not decrease for this number of iterations.
#' @param return_input The original input data is returned as last list entry
#' @param simplify If n_random = 1 and return_input = FALSE only pattern will be returned.
#' @param verbose Print progress report.
#' @param plot Plot kmmr function during optimization.
#'
#' @details
#' The function randomizes the numeric marks of a point pattern using pattern reconstruction
#' as described in Tscheschel & Stoyan (2006) and Wiegand & Moloney (2014). Therefore,
#' an unmarked as well as a marked pattern must be provided. The unmarked pattern must have
#' the spatial characteristics but the same observation window and number of points
#' as the marked one (see `reconstruct_pattern` or `fit_point_process`). Marks must be
#' numeric because the mark-correlation function is used as summary function. Two
#' randomly chosen marks are switch each iterations and changes only kept if the
#' deviation between the observed and the reconstructed pattern decreases.
#'
#' @seealso
#' \code{\link{reconstruct_pattern}} \cr
#' \code{\link{fit_point_process}} \cr
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' pattern_recon <- reconstruct_pattern(species_a, n_random = 1, max_runs = 1000)[[1]]
#' marks_sub <- spatstat::subset.ppp(species_a, select = dbh)
#' marks_recon <- reconstruct_marks(pattern_recon, marks_sub, n_random = 19, max_runs = 1000)
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
                              n_random = 1,
                              e_threshold = 0.01,
                              max_runs = 10000,
                              no_change = Inf,
                              return_input = TRUE,
                              simplify = FALSE,
                              verbose = TRUE,
                              plot = FALSE){

  # check if n_random is >= 1
  if(!n_random >= 1) {
    stop("n_random must be >= 1.", call. = FALSE)
  }

  # check if pattern is marked
  if(spatstat::is.marked(pattern) || !spatstat::is.marked(marked_pattern)) {
    stop("'pattern' must be unmarked and 'marked_pattern' marked", call. = FALSE)
  }

  if(any(pattern$window$xrange != marked_pattern$window$xrange) ||
     any(pattern$window$yrange != marked_pattern$window$yrange) ||
     pattern$n != marked_pattern$n) {
    stop("'pattern' and 'pattern' must have same window and number of points",
         call. = FALSE)
  }

  # check if marks are numeric
  if(class(marked_pattern$marks) != "numeric") {
    stop("marks must be 'numeric'", call. = FALSE)
  }

  # counter if energy changed
  energy_counter <- 0

  # assign shuffeld marks to pattern
  spatstat::marks(pattern) <- rcpp_sample(x = marked_pattern$marks, n = marked_pattern$n)

  # calculate summary functions
  kmmr_observed <- spatstat::markcorr(marked_pattern,
                                      correction = "Ripley")

  kmmr_simulated <- spatstat::markcorr(pattern,
                                       correction = "Ripley")

  # energy before reconstruction
  energy <- mean(abs(kmmr_observed[[3]] - kmmr_simulated[[3]]), na.rm = TRUE)

  # create n_random recondstructed patterns
  result <- lapply(seq_len(n_random), function(current_pattern) {

    # get two random points to switch marks
    rp_a <- rcpp_sample(x = seq_len(pattern$n), n = max_runs, replace = TRUE)

    rp_b <- rcpp_sample(x = seq_len(pattern$n), n = max_runs, replace = TRUE)

    # pattern reconstruction algorithm (optimaztion of energy) - not longer than max_runs
    for(i in seq_len(max_runs)) {

      relocated <- pattern # data for relocation

      # current random points
      rp_a_current <- rp_a[[i]]

      rp_b_current <- rp_b[[i]]

      # get marks of the two random points
      mark_a <- relocated$marks[[rp_a_current]]

      mark_b <- relocated$marks[[rp_b_current]]

      # switch the marks of the two points
      relocated$marks[[rp_a_current]] <- mark_b

      relocated$marks[[rp_b_current]] <- mark_a

      # calculate summary functions after relocation
      kmmr_relocated <- spatstat::markcorr(relocated,
                                           correction = "Ripley")

      # energy after relocation
      e_relocated <- mean(abs(kmmr_observed[[3]] - kmmr_relocated[[3]]), na.rm = TRUE)

      # lower energy after relocation
      if(e_relocated < energy){

        # keep relocated pattern
        pattern <- relocated

        # keep e_relocated as energy
        energy <- e_relocated

        # plot observed vs reconstructed
        if(plot) {

          # https://support.rstudio.com/hc/en-us/community/posts/200661917-Graph-does-not-update-until-loop-completion
          Sys.sleep(0.1)

          graphics::plot(x = kmmr_observed[[1]], y = kmmr_observed[[3]],
                         type = "l", col = "black",
                         xlab = "r", ylab = "kmm(r)")

          graphics::lines(x = kmmr_relocated[[1]], y = kmmr_relocated[[3]], col = "red")

          graphics::legend("topright",
                           legend = c("observed", "reconstructed"),
                           col = c("black", "red"),
                           lty = 1, inset = 0.025)
        }
      }

      # increase counter no change
      else {
        energy_counter <- energy_counter + 1
      }

      # print progress
      if(verbose) {
        message("\r> Progress: n_random: ", current_pattern, "/", n_random,
                " || max_runs: ", i, "/", max_runs,
                " || energy = ", round(energy, 5), appendLF = FALSE)
      }

      # exit loop if e threshold or no_change counter max is reached
      if(energy <= e_threshold|| energy_counter > no_change){
        break
      }
    }

    return(pattern)
  })

  # add input pattern to randomizations
  if(return_input){

    # simplify not possible if return pattern should be included
    if(simplify){
      cat("\n")
      warning("'simplify = TRUE' not possible for 'return_input = TRUE'", call. = FALSE)
    }

    # add input pattern as last list entry
    result[[n_random + 1]] <- marked_pattern

    # set names
    names(result) <-  c(paste0("randomized_", seq_len(n_random)), "observed")
  }

  # do not include input pattern
  else{

    # only return pattern as ppp (and not as list)
    if(simplify) {

      # not possible if more than one pattern is present
      if(n_random > 1) {
        cat("\n")
        warning("'simplify = TRUE' not possible for 'n_random > 1'", call. = FALSE)
      }

      # only randomized pattern
      else {
        result <- result[[1]]
      }
    }

    # return as list
    else{

      # set names
      names(result) <- paste0("randomized_", seq_len(n_random))
    }
  }

  # write result in new line if progress was printed
  if(verbose) {
    message("\r")
  }

  return(result)
}
