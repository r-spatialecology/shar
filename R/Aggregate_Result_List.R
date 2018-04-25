#' Aggregate result list
#'
#' Aggregates the result of FUNCTION-NAME into the long format and calculates some statistics (e.g. mean, SE, ...)
#' @param result_list [\code{list(1)}]\cr Result list create with Result.Habitat.Association()
#' @param id [\code{string(1)}]\cr Name of the ID-variable
#' @param neutral [\code{logical(1)}] \cr TRUE if result_list is based on neutral species
#'
#' @return Long data frame with aggregated results

#' @export
Aggregate.Result.List <- function(result_list, id,  neutral=F){

  if(neutral==F){
    result_list_aggregated <- result_list %>%
      dplyr::group_by_(.dots=c(id, "Species")) %>%
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
      dplyr::group_by_(.dots=c(id, "Species")) %>%
      dplyr::summarise(Correct_mean=mean(Correct),
                       Correct_hi = mean(Correct) + (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                       Correct_lo = mean(Correct) - (stats::sd(Correct, na.rm=T)/sqrt(length(Correct))),
                       False_mean=mean(False),
                       False_hi = mean(False) + (stats::sd(False, na.rm=T)/sqrt(length(False))),
                       False_lo = mean(False) - (stats::sd(False, na.rm=T)/sqrt(length(False)))) %>%
      dplyr::mutate(Species_type=factor(dplyr::case_when(Species==1 ~ "Poisson process",
                                                         Species==2 ~ "Thomas process")))
  }

  return(result_list_aggregated)
}
