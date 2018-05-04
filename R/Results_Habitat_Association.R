#' Results habitat association
#'
#' Results of the species-habitat association test using the simulated vs. the observed data
#' @param pattern [\code{ppp(1)}]\cr ppp object of the spatstat package. If method="random_pattern", list with simulated patterns
#' @param raster [\code{raster(1)}]\cr Raster object of the raster package. If method="random_raster, list with simulated habitats
#' @param method [\code{string(1)}]\cr Form of input data. Either "random_pattern " or "random_raster"
#' @param threshold [\code{numeric(2)}]\cr Threshold for significance. See ?quantile() for more details
#' @param only_spatial [\code{logical(1)}]\cr Boolean if pattern contains only one species
#' @return List or data frame with results of species-habitat association

#' @export
Results.Habitat.Association <- function(pattern, raster, method, threshold=c(0.025, 0.975), only_spatial=F){

  result_list <- list()

  if(method=="random_raster"){

    if(only_spatial==T){
      points <- pattern %>%
        spatstat::coords() %>%
        sp::SpatialPoints()

      habitat_counts <- purrr::map_dfr(raster, .id = 'Type',
                                       function(x) SHAR::Extract.Points(raster=x,
                                                                        points=points,
                                                                        method=method))

      habitat_counts_randomized <- habitat_counts %>%
        dplyr::filter(Type!="Observed") %>%
        dplyr::group_by(Habitat) %>%
        dplyr::summarise(Lo=stats::quantile(Count,probs=threshold[[1]]),
                         Hi=stats::quantile(Count,probs=threshold[[2]]))

      habitat_counts_observed <- habitat_counts %>%
        dplyr::filter(Type=="Observed") %>%
        dplyr::select(-Type)

      result_list <- dplyr::full_join(habitat_counts_observed, habitat_counts_randomized,
                                      by = "Habitat") %>%
        dplyr::mutate(Significance=factor(dplyr::case_when(Count<Lo ~ "negative",
                                                           Count>Hi ~ "positive",
                                                           Count>=Lo & Count<=Hi ~ "N.S.")))
    }

    else{
      points <- pattern %>%
        spatstat::coords() %>%
        sp::SpatialPointsDataFrame(data=data.frame(pattern$marks$Species))
      names(points) <- "Species"

      species_list <- points$Species %>%
        unique(drop=T)

      for(i in 1:length(species_list)){
        points_spec <- points %>%
          subset(Species==species_list[[i]])

        habitat_counts <- purrr::map_dfr(raster, .id = 'Type',
                                         function(x) SHAR::Extract.Points(raster=x,
                                                                    points=points_spec,
                                                                    method=method))

        habitat_counts_randomized <- habitat_counts %>%
          dplyr::filter(Type!="Observed") %>%
          dplyr::group_by(Habitat) %>%
          dplyr::summarise(Lo=stats::quantile(Count,probs=threshold[[1]]),
                           Hi=stats::quantile(Count,probs=threshold[[2]]))

        habitat_counts_observed <- habitat_counts %>%
          dplyr::filter(Type=="Observed") %>%
          dplyr::select(-Type)

        result_list[[paste(species_list[[i]])]] <- dplyr::full_join(habitat_counts_observed, habitat_counts_randomized,
                                                                    by = "Habitat") %>%
          dplyr::mutate(Significance=factor(dplyr::case_when(Count<Lo ~ "negative",
                                                             Count>Hi ~ "positive",
                                                             Count>=Lo & Count<=Hi ~ "N.S.")))
      }
    }
  }

  else if(method=="random_pattern"){

    if(only_spatial==T){
      habitat_counts <- purrr::map_dfr(pattern, .id = 'Type',
                                       function(x) SHAR::Extract.Points(raster=raster,
                                                                        points=x,
                                                                        method=method))

      habitat_counts_randomized <- habitat_counts %>%
        dplyr::filter(Type!="Observed") %>%
        dplyr::group_by(Habitat) %>%
        dplyr::summarise(Lo=stats::quantile(Count,probs=threshold[[1]]),
                         Hi=stats::quantile(Count,probs=threshold[[2]]))

      habitat_counts_observed <- habitat_counts %>%
        dplyr::filter(Type=="Observed") %>%
        dplyr::select(-Type)

      result_list <- dplyr::full_join(habitat_counts_observed, habitat_counts_randomized, by = "Habitat") %>%
        dplyr::mutate(Significance=factor(dplyr::case_when(Count<Lo ~ "negative",
                                                           Count>Hi ~ "positive",
                                                           Count>=Lo & Count<=Hi ~ "N.S.")))
    }

    else{
      print("Method 'random_pattern' not implemented for multiple species yet")
      result_list <- NA
    }
  }

  else{
    print("Please select either 'random_pattern' or 'random_raster' as method")
    result_list <- NA
  }

  return(result_list)
}
