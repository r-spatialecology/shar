#' plot_randomized_pattern
#'
#' @description Plot randomized pattern
#'
#' @param pattern List with reconstructed patterns.
#' @param size Line size.
#' @param base_size base font size.
#' @param comp_fast Should summary functions be estimated in an computational fast way.
#'
#' @details
#' The function plots the pair correlation function and the nearest neighbour function
#' the observed pattern and the reconstructed patterns (as "simulation envelopes".).
#' For large patterns `comp_fast = TRUE` decreases the computational demand because no edge
#' correction is used and the pair correlation function is estimated based on Ripley's
#' K-function. For more information see \code{\link{estimate_pcf_fast}}.
#'
#' @return ggplot
#'
#' @examples
#' \dontrun{
#' pattern_random <- spatstat::runifpoint(n = 50)
#' pattern_recon <- SHAR::reconstruct_pattern(, n_random = 9, max_runs = 1000)
#' plot_randomized_pattern(pattern_recon)
#' }
#'
#' @aliases plot_randomized_pattern
#' @rdname plot_randomized_pattern
#'
#' @export
plot_randomized_pattern <- function(pattern,
                                    size = 0.5, base_size = 15,
                                    comp_fast = FALSE){

  name_unit <- spatstat::unitname(pattern$observed)[[1]]

  result_list <- lapply(pattern, function(current_pattern){

    gest_result <- spatstat::Gest(current_pattern)


    if(isTRUE(comp_fast)) {
      gest_result <- spatstat::Gest(current_pattern, correction = "none")

      pcf_result <- SHAR::estimate_pcf_fast(current_pattern,
                                            correction = "none",
                                            method = "c",
                                            spar = 0.5)
    }

    else{
      gest_result <- spatstat::Gest(current_pattern, correction = "han")

      pcf_result <- spatstat::pcf(current_pattern, divisor = "d", correction = "best")
    }

    gest_df <- as.data.frame(gest_result)
    names(gest_df)[3] <- "x_r"
    gest_df$summary_function <- "Nearest Neighbour Distance Function G(r)"

    pcf_df <- as.data.frame(pcf_result)
    names(pcf_df)[3] <- "x_r"
    pcf_df$summary_function <- "Pair Correlation Function g(r)"

    result <- dplyr::bind_rows(gest_df, pcf_df)

    return(result)
  })

  names(result_list) <- names(pattern)

  result_df <- dplyr::bind_rows(result_list, .id = "pattern")

  result_df <- dplyr::mutate(result_df, type = dplyr::case_when(pattern == "observed" ~ "observed",
                                                                TRUE ~ "randomized"))

  result_df_grouped <- dplyr::group_by(result_df, summary_function, r, type)

  result_df_summarised <- dplyr::summarise(result_df_grouped,
                                           lo = stats::quantile(x_r, probs = 0.025),
                                           hi = stats::quantile(x_r, probs = 0.975),
                                           x_r = mean(x_r),
                                           theo = mean(theo))

  plot <-  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = dplyr::filter(result_df_summarised, type == "randomized"),
                         ggplot2::aes(x = r, ymin = lo, ymax = hi, fill = "pattern reconstruction"),
                         size = size) +
    ggplot2::geom_line(data = dplyr::filter(result_df_summarised, type == "observed"),
                       ggplot2::aes(x = r, y = theo, col = "CSR"),
                       linetype = 2, size = size) +
    ggplot2::geom_line(data = dplyr::filter(result_df_summarised, type == "observed"),
                       ggplot2::aes(x = r, y = x_r, col = "observed"),
                       size = size) +
    ggplot2::facet_wrap(. ~ summary_function, scales = "free") +
    ggplot2::scale_fill_manual(values = c("pattern reconstruction" = "gray"),
                               name = "") +
    ggplot2::scale_color_manual(values = c("CSR" = "red",
                                           "observed" = "black"),
                                name = "") +
    ggplot2::labs(x = paste0("r [",name_unit, "]"), y = "f(r)") +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(legend.position="bottom")

  return(plot)
}

