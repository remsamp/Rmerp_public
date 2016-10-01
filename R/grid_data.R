#' Function to grid and plot geolocated observations on a map
#'
#' Data portals such as OBIS typically return tables of geolocated observations upon requests.
#' Gridded data is often used to represent in a synthetic way this type of observations, 
#' but although R packages ggplot2 and ggmap are good at representing it, 
#' there is no built-in function to go from raw to aggregated data. 
#' 
#' @return \code{grid_data} returns a data frame containing the number of observations per cell. 
#' 
#' @param mydata is an R data.frame. It contains two columns that must be named lat and lon, lon corresponding to each observations.
#' mydata must have the right column names for the function to work. See example on how to do that.
#' @param myresolution is the size of the cells the data is to be aggregated over
#' @param myzoom is the zoom to be applied to plot the gridded data on ggmap map
#' 
#' 
#' @examples
#' library(robis)
#' obs <- occurrence("Gadus morhua", year = 2002)
#' myobs <- data.frame(obs$decimalLongitude, obs$decimalLatitude)
#' names(myobs) <- c("lon", "lat")
#' justchecking <- grid_data(myobs, myresolution = 0.5, myzoom = 4)
#' # examine the data
#' head(justchecking$gridded_data)
#' # plot the data
#' justchecking$myplot

# function to grid obis data extracted using occurrence function
grid_data <- function(mydata, myresolution = 0.5, myzoom = 7, lat_centre = NULL, lon_centre = NULL){
  breakx <- seq(min(floor(mydata$lon)), max(ceiling(mydata$lon)), by = myresolution)
  breaky <- seq(min(floor(mydata$lat)), max(ceiling(mydata$lat)), by = myresolution)
  cellx <- cut(mydata$lon, breaks = breakx, labels = breakx[1:(length(breakx)-1)])
  celly <- cut(mydata$lat, breaks = breaky, labels = breaky[1:(length(breaky)-1)])
  mycoord <- paste(cellx, celly, sep = "_")
  Records <- table(mycoord)
  
  inter <- expand.grid(breakx, breaky)
  dat <- data.frame(x0 = inter[, 1], x1 = inter[, 1] + 1, y0 = inter[, 2], y1 = inter[, 2] + 1)
  dat$mycoord <- paste(dat$x0, dat$y0, sep = "_")
  
  dat$Records <- rep(NA, nrow(dat))
  
  # match mycoord with dat$mycoord
  idx <- match(dat$mycoord, names(Records))
  dat$Records <- as.numeric(Records)[idx]
  # dat$Records[is.na(dat$Records)] <- 0
  dat$id <- c(1: nrow(dat))
  dat$myresolution <- rep(myresolution, nrow(dat))
  
  if(is.null(lat_centre)){
    mymap <- get_map(location=c(mean(mydata$lon),mean(mydata$lat)),"satellite",zoom=myzoom,scale="auto")
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


