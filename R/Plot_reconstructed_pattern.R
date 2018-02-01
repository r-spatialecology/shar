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

    df_pcf <- data.frame()
    for(i in 1:length(pattern)){
      if(pattern[[i]]$n>=500){
        pcf_i <- SHAR::Pcf.Fast(pattern[[i]])
      }
      else{
        pcf_i <- spatstat::pcf(pattern[[i]], correction="best", divisor="d")
      }

      df_pcf <- rbind(df_pcf, cbind(Pattern=i, r=pcf_i[[1]], g=pcf_i[[3]]))
    }

    df_pcf$Type <- "Reconstructed"
    df_pcf$Type[df_pcf$Pattern==length(pattern)] <- "Observed"

    df_reconstructed <- df_pcf[df_pcf$Type=="Reconstructed",2:3]
    df_observed <- df_pcf[df_pcf$Type=="Observed",2:3]

    df_reconstructed_aggregated <- plyr::ddply(df_reconstructed, c("r"),
                                     plyr::summarise,
                                     Lo=stats::quantile(g, probs=0.025),
                                     Hi=stats::quantile(g, probs=0.975))

    plot_pcf <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(data=df_reconstructed_aggregated, ggplot2::aes(x=r, ymin=Lo, ymax=Hi, fill="Reconstructed"),
                           alpha=1, size=1) +
      ggplot2::geom_line(data=df_observed, ggplot2::aes(x=r, y=1), linetype=2) +
      ggplot2::geom_line(data=df_observed, ggplot2::aes(x=r, y=g, col="Observed"), size=1) +
      ggplot2::scale_color_manual(name="", values=c(Observed="black")) +
      ggplot2::scale_fill_manual(name="", values=c(Reconstructed="gray")) +
      ggplot2::labs(x="r [m]", y="g(r)") +
      ggplot2::theme_bw(base_size=15)
  }

  else{
    df_pcf <- data.frame()

    for(i in 1:length(pattern)){
      pcf_i <- SHAR::Pcf.Multi(pattern[[i]], r_max=8)
      df_pcf <- rbind(df_pcf, cbind(Pattern=i, r=pcf_i$r, g=pcf_i$Mean))
    }

    df_pcf$Type <- "Reconstructed"
    df_pcf$Type[df_pcf$Pattern==length(pattern)] <- "Observed"

    df_reconstructed <- df_pcf[df_pcf$Type=="Reconstructed",2:3]
    df_observed <- df_pcf[df_pcf$Type=="Observed",2:3]

    df_reconstructed_aggregated <- plyr::ddply(df_reconstructed, c("r"),
                                               plyr::summarise,
                                               Lo=quantile(g, probs=0.025),
                                               Hi=quantile(g, probs=0.975))

    plot_pcf <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(data=df_reconstructed_aggregated, ggplot2::aes(x=r, ymin=Lo, ymax=Hi, fill="Reconstructed"),
                          alpha=1, size=1) +
      ggplot2::geom_line(data=df_observed, ggplot2::aes(x=r, y=1), linetype=2, size=1.25) +
      ggplot2::geom_line(data=df_observed, ggplot2::aes(x=r, y=g, col="Observed"), size=1) +
      ggplot2::scale_color_manual(name="", values=c(Observed="black")) +
      ggplot2::scale_fill_manual(name="", values=c(Reconstructed="gray")) +
      ggplot2::labs(x="r [m]", y="gi(r)") +
      ggplot2::theme_bw(base_size=15)
  }
  return(plot_pcf)
}

