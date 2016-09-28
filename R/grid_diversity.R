grid_diversity <- function(mydata, taxonomic_level, myresolution, myzoom, lat_centre = NULL, lon_centre = NULL){
  # mydata <- allthedata
  # myresolution <- 0.5
  # taxonomic_level <- "order"
  
  breakx <- seq(min(floor(mydata$lon)), max(ceiling(mydata$lon)), by = myresolution)
  breaky <- seq(min(floor(mydata$lat)), max(ceiling(mydata$lat)), by = myresolution)
  mydata$cellx <- cut(mydata$lon, breaks = breakx, labels = breakx[1:(length(breakx)-1)])
  mydata$celly <- cut(mydata$lat, breaks = breaky, labels = breaky[1:(length(breaky)-1)])
  eval(parse(text = paste("mydata <- mydata[!is.na(mydata$",taxonomic_level,"), ]", sep = "")))
  eval(parse(text = paste("inter <- mydata$",taxonomic_level, sep = "")))
  mydata$ref <- paste(mydata$cellx, mydata$celly, inter, sep = "_")
  mydata <- mydata[!duplicated(mydata$ref), ]
  
  mycoord <- paste(mydata$cellx, mydata$celly, sep = "_")
  mygeocounts <- table(mycoord)
  
  inter <- expand.grid(breakx, breaky)
  dat <- data.frame(x0 = inter[, 1], x1 = inter[, 1] + 1, y0 = inter[, 2], y1 = inter[, 2] + 1)
  dat$mycoord <- paste(dat$x0, dat$y0, sep = "_")
  dat$mygeocounts <- rep(NA, nrow(dat))
  
  # match mycoord with dat$mycoord
  idx <- match(dat$mycoord, names(mygeocounts))
  dat$mygeocounts <- as.numeric(mygeocounts)[idx]
  # dat$taxa_number[is.na(dat$taxa_number)] <- NULL
  dat$id <- c(1: nrow(dat))
  # dat$alpha <- rep(0, nrow(dat))
  # dat$alpha[dat$mygeocounts != 0] <- 0.25
  dat$myresolution <- rep(myresolution, nrow(dat))
  
  if(is.null(lat_centre)){
     mymap <- get_map(location=c(mean(mydata$lon),mean(mydata$lat)),"satellite",zoom=myzoom,scale="auto")
   }
  else{
     mymap <- get_map(location=c(lon_centre,lat_centre),"satellite",zoom=myzoom,scale="auto")
  }
  p <- ggmap(mymap)
  p <- p + geom_tile(data = dat, aes(x = (x0 + x1) / 2, y = (y0 + y1) / 2, width = myresolution, height = myresolution, group = id, fill = mygeocounts)) +
    scale_fill_gradient(low = "yellow", high = "red", na.value = NA)
  return(list(gridded_data = dat, myplot = p))
}

# I can have a wrapper for this that would allow subsetting for a certain taxa
# for instance, all genus within superclass Pisces. 