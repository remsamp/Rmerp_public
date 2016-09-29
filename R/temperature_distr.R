#' Plot species thermal niche
#'
#' The function takes the meiofauna database MANUELA
#' and use dplyr to merge its tables into a data.frame and/or csv file.
#' There are 14 tables in total. the first 4 concern abiotic measurements 
#' 5 and 6 have biotic data, with names, count, length, width and biomass

#' do I need to disconnect the link between R and the database?

#' \code{manueladb_to_table} merges 4 of the tables together.
#' The database itself is on Strathcloud in module1/data
#' and needs to be downloaded on the user's computer.
#' 
#' @param path_to_manuela is the path to the local
#' copy of the database on the user's computer.  
#' 
#' @examples
#' manuela <- manueladb_to_table("~/manuela")
#' class(manuela)
temperature_distr <- function(fgroup = NULL, species = NULL, returndata = F, bin_min = -10, bin_max = 40, bwidth = 1, xtop = 1000, xbottom = 0){
  # species = "Calanus finmarchicus"
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
    # temp_plot <- temp_plot %>%
    #   ggplot(data = .) + geom_histogram(aes(surface_temp), breaks = seq(bin_min, bin_max, by = bwidth), colour = "black", fill = "grey") + 
    #   theme_grey() +
    #   labs(x = "Surface temperature", y = "Records") +
    #   ylim(c(ybottom,ytop))
    temp_plot <- temp_plot %>%
      ggplot(data = .) + geom_density(aes(surface_temp), colour = "black", fill = "blue", alpha = 0.1)+
      theme(legend.position = "none")+
      xlim(xbottom, xtop)
  }
  
  if(!is.null(species)){
    temp_plot <- NA
    temp_plot <- eval(parse(text = paste("subset(obis_dat, scientificName == \"", species, "\")", sep = "")))
    # temp_plot <- temp_plot %>%
    #   ggplot(data = .) + geom_histogram(aes(surface_temp), breaks = seq(bin_min, bin_max, by = bwidth), colour = "black", fill = "grey") + 
    #   theme_grey() +
    #   labs(x = "Surface temperature", y = "Records") +
    #   ylim(c(ybottom,ytop))
    temp_plot <- temp_plot %>%
       ggplot(data = .) + geom_density(aes(surface_temp), colour = "black", fill = "blue", alpha = 0.1)+
      theme(legend.position = "none")+
      xlim(xbottom, xtop) 
  }
  return(list(plot = temp_plot))
}