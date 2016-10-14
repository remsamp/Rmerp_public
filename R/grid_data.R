#' Function to grid geolocated observations on a map
#'
#' Data portals such as OBIS typically return tables of geolocated observations upon requests.
#' Gridded data is often used to represent in a synthetic way this type of observations, 
#' but although R packages ggplot2 and ggmap are good at representing it, 
#' there is no built-in function to go from raw to aggregated data. 
#' 
#' @return \code{grid_data} returns a data frame containing the number of observations per cell. 
#' 
#' @param lat is a numerical vector of latitudes for the set of observations.
#' @param lon is a numerical vector of longitudes for the set of observations.
#' @param myresolution is the size of the cells the data is to be aggregated over
#' @param myzoom is the zoom to be applied to plot the gridded data on ggmap map
#' @param lon_centre is the user-defined longitude the map will be centred on. Default to mean 
#' longitude of observations.
#' @param lat_centre is the user-defined latitude the map will be centred on. Default to mean 
#' latitude of observations.
#' 
#' @examples
#' library(robis)
#' obs <- occurrence("Gadus morhua", year = 2002)
#' justchecking <- grid_data(lat = obs$decimalLatitude, lon = obs$decimalLongitude, myresolution = 0.5, myzoom = 4)
#' # examine the data
#' head(justchecking$gridded_data)
#' # plot the data
#' justchecking$myplot

grid_data <- function(lat, lon, myresolution = 0.5, myzoom = 7, lat_centre = NULL, lon_centre = NULL){
  breakx <- seq(min(floor(lon)), max(ceiling(lon)), by = myresolution)
  breaky <- seq(min(floor(lat)), max(ceiling(lat)), by = myresolution)
  cellx <- cut(lon, breaks = breakx, labels = breakx[1:(length(breakx)-1)])
  celly <- cut(lat, breaks = breaky, labels = breaky[1:(length(breaky)-1)])
  mycoord <- paste(cellx, celly, sep = "_")
  Records <- table(mycoord)
  
  inter <- expand.grid(breakx, breaky)
  dat <- data.frame(x0 = inter[, 1], x1 = inter[, 1] + 1, y0 = inter[, 2], y1 = inter[, 2] + 1)
  dat$mycoord <- paste(dat$x0, dat$y0, sep = "_")
  
  dat$Records <- rep(NA, nrow(dat))
  
  idx <- match(dat$mycoord, names(Records))
  dat$Records <- as.numeric(Records)[idx]
  dat$id <- c(1: nrow(dat))
  dat$myresolution <- rep(myresolution, nrow(dat))
  
  if(is.null(lat_centre)){
    mymap <- get_map(location=c(mean(lon),mean(lat)),"satellite",zoom=myzoom,scale="auto")
  }
  else{
    mymap <- get_map(location=c(lon_centre,lat_centre),"satellite",zoom=myzoom,scale="auto")
  }
  
  p <- ggmap(mymap)
  p <- p + geom_tile(data = dat, aes(x = (x0 + x1) / 2, y = (y0 + y1) / 2, width = myresolution, height = myresolution, fill = Records, group = id)) +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18) , plot.margin = unit(c(t = 1,b = 10,r = 10,l = 10), unit = "mm")) +
    labs(x= "Longitude", y = "Latitude")
  return(list(gridded_data = dat, myplot = p))
}


