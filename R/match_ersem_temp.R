#' Match ERSEM temperature to biotic observations
#'
#' This functions uses ERSEM sea surface temperature
#' and matches it to a user-provided data set of
#' geolocated observations. The ERSEM data set is not included
#' in the package but can be downloaded freely from opec website
#' at https://portal.marineopec.eu/ (see example on how to 
#' load the data into R). This is montly data between 
#' jan 1991 and november 2013 on a lattice covering a domain 
#' between longitudes -19.83333 and 13.00000 and 
#' latitudes 40.11111 and 64.88889.
#' 
#' @param ersem is the data set containing the ersem temperature data
#' @param lon is the longitude for the observation to match
#' @param lat is the latitude for the observation to match
#' @param year is the year the observation was recorded
#' @param month is the month the observation was recorded
#' 
#' @examples
#' # Not run: download ersem netcdf4 file and split it 
#' # by months (275 between jan 1991 and nov 2013)
#' #library(netcdf4)
#' #netc_ersem <- nc_open("path_to_ersem_netcdf4_file.nc")
#' #myersem <- vector("list", length = dim(netc_ersem)[1])
#' #for(i in 1:length(myersem)) myersem[[i]] <- netc_ersem[i, , ] 
#' #match_ersem_temp(ersem = myersem, lon = -7, lat = 50, year = 2010, month = 8)
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