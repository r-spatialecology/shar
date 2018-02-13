#' Aggregate result list
#'
#' Aggregates the result of FUNCTION-NAME into the long format and calculates some statistics (e.g. mean, SE, ...)
#' @param result_list [\code{list(1)}]\cr Result list create with Result.Habitat.Association()
#' @param id [\code{string(1)}]\cr Name of the ID-variable
#' @param complex [\code{logical(1)}] \cr TRUE if result_list is based on complex simulation pattern
#'
#' @return Long data frame with aggregated results

#' @export
Aggregate.Result.List <- function(result_list, id,  neutral=F){

  if(neutral==F){
    result_list_aggregated <- result_list %>%
      dplyr::group_by(Alpha, Species) %>%
      dplyr::summarise(Correct_mean=mean(Correct),
                       Correct_hi = mean(Correct) + (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                       Correct_lo = mean(Correct) - (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                       False_mean=mean(False),
                       False_hi = mean(False) + (stats::sd(False, na.rm=T)/sqrt(length(False))),
                       False_lo = mean(False) - (stats::sd(False, na.rm=T)/sqrt(length(False)))) %>%
      dplyr::mutate(Species_type=factor(dplyr::case_when(Species==1 ~ "Poisson process (positive)",
                                                  Species==2 ~ "Thomas process (positive)",
                                                  Species==3 ~ "Poisson procces (negative)",
                                                  Species==4 ~ "Thomas process (negative)")))
    }

  else{
    result_list_aggregated <- result_list %>%
      dplyr::group_by(Alpha, Species) %>%
      dplyr::summarise(Correct_mean=mean(Correct),
                       Correct_hi = mean(Correct) + (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                       Correct_lo = mean(Correct) - (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                       False_mean=mean(False),
                       False_hi = mean(False) + (stats::sd(False, na.rm=T)/sqrt(length(False))),
                       False_lo = mean(False) - (stats::sd(False, na.rm=T)/sqrt(length(False)))) %>%
      dplyr::mutate(Species_type=factor(dplyr::case_when(Species==1 ~ "Poisson process",
                                                         Species==2 ~ "Thomas process")))
    }

#   else{
#     result_list_aggregated <- reshape2::melt(result_list, id=id,
#                                              value.name="Percentage", variable.name="Measure") %>%
#       setNames(c(id, "Measure", "Percentage", "Species")) %>%
#       dplyr::mutate(Type=factor(dplyr::case_when(Species=="Species_1" ~ "Neutral",
#                                                  Species=="Species_2" ~ "Positive associations",
#                                                  Species=="Species_3" ~ "Negative associations"))) %>%
#       dplyr::group_by(Association, Measure, Type) %>%
#       dplyr::summarise(Mean=mean(Percentage, na.rm=T),
#                        SE = stats::sd(Percentage, na.rm=T)/sqrt(length(Percentage)),
#                        HI = stats::quantile(Percentage, probs=0.95)[[1]],
#                        LO = stats::quantile(Percentage, probs=0.05)[[1]],
#                        CI = SE * 1.96)
#   }

  return(result_list_aggregated)
}
