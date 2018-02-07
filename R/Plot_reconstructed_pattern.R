#' Plot list of reconstructed patterns
#'
#' Function to plot all reconstructed patterns created of the pattern list with reconstructed patterns
#' @param pattern [\code{list(1)}] List with reconstructed and observed point pattern created with SHAR::Pattern.Reconstruction()
#' @param only_spatial [\code{logical(1)}] TRUE for univariate point pattern
#'
#' @return ggplot object of the ggplot2 package

#' @export
Plot.Reconstructed.Pattern <- function(pattern, only_spatial=T){

  if(only_spatial==T){
    if(pattern[[length(pattern)]]$n>=1000){result <- lapply(pattern, FUN=SHAR::Pcf.Fast)}
    else{result <- lapply(pattern, FUN=spatstat::pcf, divisor="d", correction="Ripley")}
  }
  else{result <- lapply(pattern, FUN=SHAR::Pcf.Multi)}

  result_aggregated <- result %>%
    reshape2::melt(id.vars=c("r"), value.name="g", variable.name="Type") %>%
    tibble::as.tibble() %>%
    dplyr::filter(Type!="theo") %>%
    dplyr::mutate(Type=dplyr::case_when(L1=="Observed" ~ "Observed",
                                            TRUE ~ "Randomized")) %>%
    dplyr::select(-L1) %>%
    dplyr::group_by(Type, r) %>%
    dplyr::summarise(Lo=stats::quantile(g,probs=0.025),
                     Hi=stats::quantile(g,probs=0.975),
                     gr=mean(g))

  plot_pcf <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data=dplyr::filter(result_aggregated, Type=="Randomized"),
                         ggplot2::aes(x=r, ymin=Lo, ymax=Hi), fill="grey") +
    ggplot2::geom_line(data=dplyr::filter(result_aggregated, Type=="Observed"),
                       ggplot2::aes(x=r, y=gr), col="black", size=1) +
    ggplot2::geom_hline(yintercept=1, linetype=2) +
    ggplot2::labs(x="r [m]", y="g(r)") +
    ggplot2::theme_bw(base_size=15)

  return(plot_pcf)
}

