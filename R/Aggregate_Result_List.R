#' Aggregate result list
#'
#' Aggregates the result of FUNCTION-NAME into the long format and calculates some statistics (e.g. mean, SE, ...)
#' @param result_list [\code{list(1)}]\cr Result list create with Result.Habitat.Association()
#' @param id [\code{string(1)}]\cr Name of the ID-variable
#' @param complex [\code{logical(1)}] \cr TRUE if result_list is based on complex simulation pattern
#'
#' @return Long data frame with aggregated results

#' @export
Aggregate.Result.List <- function(result_list, id, complex=F){
  data_long <- reshape2::melt(result_list, id=id, value.name="Percentage", variable.name="Measure")
  names(data_long)[4] <- "Species"

  if(complex==F){
    data_long$Type[data_long$Species=="Species_1"] <- "Poisson process (neutral)"
    data_long$Type[data_long$Species=="Species_2"] <- "Poisson process (positive)"
    data_long$Type[data_long$Species=="Species_3"] <- "Thomas process (positive)"
    data_long$Type[data_long$Species=="Species_4"] <- "Poisson process (negative)"
    data_long$Type[data_long$Species=="Species_5"] <- "Thomas process (negative)"
    data_long$Type[data_long$Species=="Species_6"] <- "Thomas process (neutral)"
  }

  else{
    data_long$Type[data_long$Species=="Species_1"] <- "Neutral"
    data_long$Type[data_long$Species=="Species_2"] <- "Positive association"
    data_long$Type[data_long$Species=="Species_3"] <- "Negative association"
  }

  data_long_aggregated <- plyr::ddply(data_long, c(id, "Measure", "Type"),
                                      plyr::summarise, Mean=mean(Percentage, na.rm=T),
                                      SE = sd(Percentage, na.rm=T)/sqrt(length(Percentage)),
                                      HI = stats::quantile(Percentage, probs=0.95)[[1]],
                                      LO = stats::quantile(Percentage, probs=0.05)[[1]])
  data_long_aggregated$CI <- data_long_aggregated$SE * 1.96

  return(data_long_aggregated)
}
