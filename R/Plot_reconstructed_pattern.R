#' Plot list of randomized patterns
#'
#' Function to plot all randomized patterns created with either pattern reconstruction of the Gamma test
#' @param pattern [\code{list(1)}] List with reconstructed and observed point pattern created with SHAR::Pattern.Reconstruction()
#' @param only_spatial [\code{logical(1)}] TRUE for univariate point pattern
#' @param title [\code{string(1)}] Title of plot

#'
#' @return ggplot object of the ggplot2 package

#' @export
Plot.Randomized.Pattern <- function(pattern, only_spatial=T, title=NULL){

  if(only_spatial==T){
    if(pattern[[length(pattern)]]$n>=1000){result <- purrr::map(pattern, SHAR::Pcf.Fast)}
    else{result <- purrr::map(pattern, spatstat::pcf, divisor="d", correction="Ripley")}
  }

  else{
    result <- pattern %>%
      purrr::map(function(x) {SHAR::Pcf.Multi(x) %>%
          dplyr::mutate(theo = 1) %>%
          dplyr::select(r, theo, Mean)})
  }

  # result_aggregated <- result %>%
  #   reshape2::melt(id.vars=c("r"), value.name="g", variable.name="Type") %>%
  #   tibble::as.tibble() %>%
  #   dplyr::filter(Type!="theo") %>%
  #   dplyr::mutate(Type=dplyr::case_when(L1=="Observed" ~ "Observed",
  #                                           TRUE ~ "Randomized")) %>%
  #   dplyr::select(-L1) %>%
  #   dplyr::group_by(Type, r) %>%
  #   dplyr::summarise(Lo=stats::quantile(g,probs=0.025),
  #                    Hi=stats::quantile(g,probs=0.975),
  #                    gr=mean(g))

  result_aggregated <- result %>%
    purrr::map_dfr(tibble::as.tibble, .id = 'Type') %>%
    setNames(c('Type', 'r', 'theo', 'pcf')) %>%
    dplyr::mutate(Type=dplyr::case_when(Type=="Observed" ~ "Observed",
                                        TRUE ~ "Randomized")) %>%
    dplyr::group_by(Type, r) %>%
    dplyr::summarise(lo=stats::quantile(pcf, probs=0.025),
                     hi=stats::quantile(pcf, probs=0.975),
                     gr=mean(pcf))


  plot_pcf <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data=dplyr::filter(result_aggregated, Type=="Randomized"),
                         ggplot2::aes(x=r, ymin=lo, ymax=hi), fill="grey") +
    ggplot2::geom_line(data=dplyr::filter(result_aggregated, Type=="Observed"),
                       ggplot2::aes(x=r, y=gr), col="black", size=1) +
    ggplot2::geom_hline(yintercept=1, linetype=2) +
    ggplot2::labs(x="r [m]", y="g(r)", title=title) +
    ggplot2::theme_bw(base_size=15)

  return(plot_pcf)
}

