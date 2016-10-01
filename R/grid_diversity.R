#' Function to grid geolocated, taxonomically-defined observations on a map
#'
#' Data portals such as OBIS typically return tables of geolocated observations upon requests.
#' When combined with packages such as taxizesoap the taxonomy of all occurring species
#' can be added. From there a common task is to map the number of unique species/genus/family (or
#' other taxonomic group) on a lattice.
#' 
#' @return \code{grid_diversity} returns a data frame containing the number of unique 
#' taxonomic group per cell. The taxonomic resolution is defined by the user.
#' 
#' 
#' @param mydata is an R data.frame. It contains two columns that must be named lat and lon, lon corresponding to each observations.
#' mydata must have the right column names for the function to work. See example on how to do that.
#' @param myresolution is the size of the cells the data is to be aggregated over
#' @param myzoom is the zoom to be applied to plot the gridded data on ggmap map
#' @param taxonomic_level is the taxonomic resolution for gridding. Possible choices are
#' kingdom, phylum, class, order, family, genus and species
#' @param lon_centre is the user-defined longitude the map will be centred on. Default to mean 
#' longitude of observations.
#' @param lat_centre is the user-defined latitude the map will be centred on. Default to mean 
#' latitude of observations.
#' 
#' 
grid_diversity <- function(mydata, taxonomic_level, myresolution = 0.5, myzoom = 7, lat_centre = NULL, lon_centre = NULL){

  breakx <- seq(min(floor(mydata$lon)), max(ceiling(mydata$lon)), by = myresolution)
  breaky <- seq(min(floor(mydata$lat)), max(ceiling(mydata$lat)), by = myresolution)
  mydata$cellx <- cut(mydata$lon, breaks = breakx, labels = breakx[1:(length(breakx)-1)])
  mydata$celly <- cut(mydata$lat, breaks = breaky, labels = breaky[1:(length(breaky)-1)])
  eval(parse(text = paste("mydata <- mydata[!is.na(mydata$",taxonomic_level,"), ]", sep = "")))
  eval(parse(text = paste("inter <- mydata$",taxonomic_level, sep = "")))
  mydata$ref <- paste(mydata$cellx, mydata$celly, inter, sep = "_")
  mydata <- mydata[!duplicated(mydata$ref), ]
  
  mycoord <- paste(mydata$cellx, mydata$celly, sep = "_")
  Records <- table(mycoord)
  
  inter <- expand.grid(breakx, breaky)
  dat <- data.frame(x0 = inter[, 1], x1 = inter[, 1] + 1, y0 = inter[, 2], y1 = inter[, 2] + 1)
  dat$mycoord <- paste(dat$x0, dat$y0, sep = "_")
  dat$Records <- rep(NA, nrow(dat))
  
  # match mycoord with dat$mycoord
  idx <- match(dat$mycoord, names(Records))
  dat$Records <- as.numeric(Records)[idx]
  # dat$taxa_number[is.na(dat$taxa_number)] <- NULL
  dat$id <- c(1: nrow(dat))
  # dat$alpha <- rep(0, nrow(dat))
  # dat$alpha[dat$Records != 0] <- 0.25
  dat$myresolution <- rep(myresolution, nrow(dat))
  
  if(is.null(lat_centre)){
     mymap <- get_map(location=c(mean(mydata$lon),mean(mydata$lat)),"satellite",zoom=myzoom,scale="auto")
   }
  else{
     mymap <- get_map(location=c(lon_centre,lat_centre),"satellite",zoom=myzoom,scale="auto")
  }
  p <- ggmap(mymap)
  p <- p + geom_tile(data = dat, aes(x = (x0 + x1) / 2, y = (y0 + y1) / 2, width = myresolution, height = myresolution, group = id, fill = Records)) +
    scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18), plot.margin = unit(c(t = 1,b = 10,r = 10,l = 10), unit = "mm")) +
    labs(x= "Longitude", y = "Latitude")
  return(list(gridded_data = dat, myplot = p))
}