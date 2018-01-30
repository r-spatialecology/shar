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
      points<- sp::SpatialPoints(spatstat::coords(pattern))
      df_counts <- data.frame() # create empty dataframe

      for(j in 1:length(raster)){ # loop for habitat maps
        e <- factor(raster::extract(raster[[j]], points, factor=T), levels=raster::unique(raster[[j]]))
        df_counts <- rbind(df_counts,  table(e)) # count trees within each habitat
      }

      names(df_counts)[1:5] <- c("Habitat_1", "Habitat_2", "Habitat_3", "Habitat_4", "Habitat_5") # rename df
      df_counts$Type <- c(rep("Randomized", length(raster)-1), "Observed") # assign type to values

      df_counts_long <- reshape2::melt(df_counts, id="Type", variable.name="Habitat", value.name="Count")
      df_counts_long$Type <- as.factor(df_counts_long$Type) # Type as factor
      df_counts_long$Habitat <- as.factor(df_counts_long$Habitat) # Habitat as factor

      df_observed <- df_counts_long[df_counts_long$Type=="Observed",2:3]
      df_randomized <- df_counts_long[df_counts_long$Type=="Randomized",2:3]

      randomized_aggregated <- plyr::ddply(df_randomized, "Habitat",
                                             plyr::here(plyr::summarise),
                                             Lo=stats::quantile(Count, probs=threshold[[1]]),
                                             Hi=stats::quantile(Count, probs=threshold[[2]]))

      df_total <- merge(df_observed, randomized_aggregated)
      df_total$Significance <- "N.S."
      df_total$Significance[df_total$Count < df_total$Lo] <- "Negative"
      df_total$Significance[df_total$Count > df_total$Hi] <- "Positive"

      result_list <- df_total
    }

    else{
      points <- sp::SpatialPointsDataFrame(spatstat::coords(pattern), data=data.frame(pattern$marks$Species))
      names(points) <- "Species"
      species_list <- unique(points$Species, drop=T)

      for(i in 1:length(species_list)){
        points_spec <- subset(points, Species==species_list[[i]])
        df_counts <- data.frame() # create empty dataframe

        for(j in 1:length(raster)){ # loop for habitat maps
          e <- factor(raster::extract(raster[[j]], points_spec, factor=T), levels=raster::unique(raster[[j]]))
          df_counts <- rbind(df_counts,  table(e)) # count trees within each habitat
        }

        names(df_counts)[1:5] <- c("Habitat_1", "Habitat_2", "Habitat_3", "Habitat_4", "Habitat_5") # rename df
        df_counts$Type <- c(rep("Randomized", length(raster)-1), "Observed") # assign type to values

        df_counts_long <- reshape2::melt(df_counts, id="Type", variable.name="Habitat", value.name="Count")
        df_counts_long$Type <- as.factor(df_counts_long$Type) # Type as factor
        df_counts_long$Habitat <- as.factor(df_counts_long$Habitat) # Habitat as factor

        df_observed <- df_counts_long[df_counts_long$Type=="Observed",2:3]
        df_randomized <- df_counts_long[df_counts_long$Type=="Randomized",2:3]

        randomized_aggregated <- plyr::ddply(df_randomized, "Habitat",
                                             plyr::here(plyr::summarise),
                                             Lo=stats::quantile(Count, probs=threshold[[1]]),
                                             Hi=stats::quantile(Count, probs=threshold[[2]]))

        df_total <- merge(df_observed, randomized_aggregated)
        df_total$Significance <- "N.S."
        df_total$Significance[df_total$Count < df_total$Lo] <- "Negative"
        df_total$Significance[df_total$Count > df_total$Hi] <- "Positive"

        result_list[[paste(species_list[[i]])]] <- df_total
      }
    }
  }

  else if(method=="random_pattern"){
    if(only_spatial==T){
      df_counts <- data.frame()
      for(i in 1:length(pattern)){
        points <- sp::SpatialPoints(spatstat::coords(pattern[[i]]))
        e <- factor(raster::extract(raster, points, factor=T), levels=raster::unique(raster))
        df_counts <- rbind(df_counts,  table(e)) # count trees within each habitat
      }

      names(df_counts)[1:5] <- c("Habitat_1", "Habitat_2", "Habitat_3", "Habitat_4", "Habitat_5") # rename df
      df_counts$Type <- c(rep("Randomized", length(pattern)-1), "Observed") # assign type to values

      df_counts_long <- reshape2::melt(df_counts, id="Type", variable.name="Habitat", value.name="Count")
      df_counts_long$Type <- as.factor(df_counts_long$Type) # Type as factor
      df_counts_long$Habitat <- as.factor(df_counts_long$Habitat) # Habitat as factor

      df_observed <- df_counts_long[df_counts_long$Type=="Observed",2:3]
      df_randomized <- df_counts_long[df_counts_long$Type=="Randomized",2:3]

      randomized_aggregated <- plyr::ddply(df_randomized, "Habitat",
                                           plyr::here(plyr::summarise),
                                           Lo=stats::quantile(Count, probs=threshold[[1]]),
                                           Hi=stats::quantile(Count, probs=threshold[[2]]))

      df_total <- merge(df_observed, randomized_aggregated)
      df_total$Significance <- "N.S."
      df_total$Significance[df_total$Count < df_total$Lo] <- "Negative"
      df_total$Significance[df_total$Count > df_total$Hi] <- "Positive"

      result_list <- df_total
    }

    else{
      species_list <- levels(pattern[[length(pattern)]]$marks$Species)

      for(i in 1:length(species_list)){
        df_counts <- data.frame()

        for(j in 1:length(pattern)){
          if(length(names(pattern[[j]]$marks))>0){pattern[[j]] <- spatstat::subset.ppp(pattern[[j]], select=Species)}

          pattern_species <- spatstat::subset.ppp(pattern[[j]], marks==species_list[[i]])
          points_species <- sp::SpatialPoints(spatstat::coords(pattern_species))

          e <- factor(raster::extract(raster, points_species, factor=T), levels=raster::unique(raster))
          df_counts <- rbind(df_counts,  table(e)) # count trees within each habitat
        }

        names(df_counts)[1:5] <- c("Habitat_1", "Habitat_2", "Habitat_3", "Habitat_4", "Habitat_5") # rename df
        df_counts$Type <- c(rep("Randomized", length(pattern)-1), "Observed") # assign type to values

        df_counts_long <- reshape2::melt(df_counts, id="Type", variable.name="Habitat", value.name="Count")
        df_counts_long$Type <- as.factor(df_counts_long$Type) # Type as factor
        df_counts_long$Habitat <- as.factor(df_counts_long$Habitat) # Habitat as factor

        df_observed <- df_counts_long[df_counts_long$Type=="Observed",2:3]
        df_randomized <- df_counts_long[df_counts_long$Type=="Randomized",2:3]

        randomized_aggregated <- plyr::ddply(df_randomized, "Habitat",
                                             plyr::here(plyr::summarise),
                                             Lo=stats::quantile(Count, probs=threshold[[1]]),
                                             Hi=stats::quantile(Count, probs=threshold[[2]]))

        df_total <- merge(df_observed, randomized_aggregated)
        df_total$Significance <- "N.S."
        df_total$Significance[df_total$Count < df_total$Lo] <- "Negative"
        df_total$Significance[df_total$Count > df_total$Hi] <- "Positive"

        result_list[[paste(species_list[[i]])]] <- df_total
      }
    }
  }

  else{
    print("Please select either 'random_pattern' or 'random_raster' as method")
    result_list <- NA
  }

  return(result_list)
}
