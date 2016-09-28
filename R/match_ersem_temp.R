#' Match ERSEM temperature to biotic observations
#'
#' This functions uses ERSEM sea surface temperature
#' and matches it to a user-provided data set of
#' geolocated observations. The ERSEM data set is not included
#' in the package but can be downloaded freely from opec data.
#' A copy is also included in Strathcloud for ease of use. 
#' 
#' This is montly data between jan 1991 and november 2013
#' on a lattice covering a domain 
#' between -19.83333  13.00000  and 40.11111 64.88889
#' latitude.
#' 
#'  
#' \code{grid_diversity} merges 4 of the tables together.
#' The database itself is on Strathcloud in module1/data
#' and needs to be downloaded on the user's computer.
#' 
#' @param lon is the path to the local
#' @param lat is the path to the local
#' @param year is the path to the local
#' @param month is the path to the local
#' 
#' @examples
#' library(netcdf4)
#' 
#' 
# > range(dat_temp$var$T$dim[[1]]$vals). 198 steps so a point every 0.167
# [1] -19.83333  13.00000
# > range(dat_temp$var$T$dim[[2]]$vals). 224 steps so a point every 0.11
# [1] 40.11111 64.88889
# jan 1991 to nov 2013 (month 1 to 275)
# lon <- -7
# lat <- 49.5
# year <- 1997
# month <- 8
match_ersem_temp <- function(ersem = NULL, lon, lat, year, month){
  if(is.null(ersem)){
    print("The ERSEM dataset is missing! Download it at xxx")
    stop()
  }
  step <- 0.2
  nbmonth <- ((year - 1990 - 1) * 12 + month)
  coord_sub <- ersem_coord[ersem_coord$lat > (lat - step) & ersem_coord$lat < (lat + step) & ersem_coord$lon > (lon - step) & ersem_coord$lon < (lon + step), ]
  alldist <- sapply(c(1:nrow(coord_sub)), function(i) dist(rbind(coord_sub[i, 1:2], c(lon, lat))))
  res <- as.numeric(coord_sub[which.min(alldist), 1:2])
  mylon <- match(res[1], ersem_lon)
  mylat <- match(res[2], ersem_lat)
  ersem[[nbmonth]][mylon, mylat]
}