temperature_distr_obis <- function(fgroup = NULL, species = NULL, returndata = F, bin_min = -10, bin_max = 40, bwidth = 1){
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
    temp_plot <- temp_plot %>%
      ggplot(data = .) + geom_histogram(aes(surface_temp), breaks = seq(bin_min, bin_max, by = bwidth), colour = "black", fill = "grey") + 
      theme_grey() +
      labs(x = "Surface temperature", y = "Records")
  }
  
  if(!is.null(species)){
    temp_plot <- NA
    temp_plot <- eval(parse(text = paste("subset(obis_dat, scientificName == \"", species, "\")", sep = "")))
    temp_plot <- temp_plot %>%
      ggplot(data = .) + geom_histogram(aes(surface_temp), breaks = seq(bin_min, bin_max, by = bwidth), colour = "black", fill = "grey") + 
      theme_grey() +
      labs(x = "Surface temperature", y = "Records")
  }
  return(list(plot = temp_plot))
}