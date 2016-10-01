#' Match biotic and abiotic data in space
#'
#' The function takes two sets of geolocated data (typically one biotic
#' and one abiotic) and match their locations to combine the two.

#' \code{match_env} returns the value of the environmental variable
#' for the location in the abiotic set of coordinates that is closest to the 
#' location for the biotic record. If the vector of values for 
#' the abiotic variable is not provided, the function simply returns the
#' index of the closest location in the vector of coordinates. 
#' 
#' @param env_lon is a vector of longitudes for the abiotic variable
#' @param env_lat is a vector of latitude for the abiotic variable
#' @param bio_lon is the longitude of the biotic variable
#' @param bio_lat is the latitude of the biotic variable
#' @param env_variable is an optional vector containing the value of the abiotic variable
#' 
#' 
match_env <- function(env_lon, env_lat, bio_lon, bio_lat, env_variable = NULL){
  alldist <- sapply(c(1:length(env_lon)), function(i) dist(rbind(c(env_lon[i], env_lat[i]), c(bio_lon, bio_lat))))
  if(is.null(env_variable)) return(which.min(alldist))
  else{return(env_variable[which.min(alldist)])}
}
