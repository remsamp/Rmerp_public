#' Map diversity on lattice
#'
#' This functions maps the diversity of 
#' 
#'  
#' \code{grid_diversity} merges 4 of the tables together.
#' The database itself is on Strathcloud in module1/data
#' and needs to be downloaded on the user's computer.
#' 
#' @param path_to_manuela is the path to the local
#' @param path_to_manuela is the path to the local
#' @param path_to_manuela is the path to the local
#' @param path_to_manuela is the path to the local
#' @param path_to_manuela is the path to the local
#' 
#' @examples
#' somedata <- manueladb_to_table("~/manuela")
#' somedata

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
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18), plot.margin = unit(c(t = 1,b = 10,r = 10,l = 10), "mm")) +
    labs(x= "Longitude", y = "Latitude")
  return(list(gridded_data = dat, myplot = p))
}