#' Plot species thermal niche
#'
#' The function plots the thermal distribution of species
#' occurrences. 
#'
#' \code{temperature_distr} takes plotting arguments and 
#' a data set of geolocated species occurrences with their 
#' accompanying temperature and taxonomy data.  
#' 
#' @param fgroup is a subsetting argument for the function. If non null
#' the function subsets the data to only retain occurrences for a particular
#' functional group.
#' @param species is a subsetting argument for the function. If non null
#' the function subsets the data to only retain occurrences for a particular
#' species.
#' @param xtop is the upper limit for the x axis in the thermal distribution graph  
#' @param xbottom is the lower limit for the x axis in the thermal distribution graph  
#'   
#' @examples
#' manuela <- manueladb_to_table("~/manuela")
#' class(manuela)
#' 
temperature_distr <- function(fgroup = NULL, species = NULL, xtop = 1000, xbottom = 0){
  if(is.null(fgroup) & is.null(species)){
    print("The function requires a functional group or species name!")
    stop()
  }
  if(!is.null(fgroup) & !is.null(species)){
    print("Both functional group and species provided. Species superseeds")
    fgroup <- NULL
  }
  
  if(!is.null(fgroup)){
    temp_plot <- NA
    temp_plot <- eval(parse(text = paste("subset(obis_dat, functional_group == \"", fgroup, "\")", sep = "")))
    temp_plot <- temp_plot %>%
      ggplot(data = .) + geom_density(aes(surface_temp), colour = "black", fill = "blue", alpha = 0.1)+
      theme(legend.position = "none")+
      xlim(xbottom, xtop)
  }
  
  if(!is.null(species)){
    temp_plot <- NA
    temp_plot <- eval(parse(text = paste("subset(obis_dat, scientificName == \"", species, "\")", sep = "")))
    temp_plot <- temp_plot %>%
       ggplot(data = .) + geom_density(aes(surface_temp), colour = "black", fill = "blue", alpha = 0.1)+
      theme(legend.position = "none")+
      labs(x = "Surface temperature") +
      xlim(xbottom, xtop) 
  }
  return(list(plot = temp_plot))
}