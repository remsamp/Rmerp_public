#' Match biotic and abiotic data in space
#'
#' The function takes the meiofauna database MANUELA
#' and use dplyr to merge its tables into a data.frame and/or csv file.
#' There are 14 tables in total. the first 4 concern abiotic measurements 
#' 5 and 6 have biotic data, with names, count, length, width and biomass

#' do I need to disconnect the link between R and the database?

#' \code{match_env} merges 4 of the tables together.
#' The database itself is on Strathcloud in module1/data
#' and needs to be downloaded on the user's computer.
#' 
#' @param env_lon is the longitude of the abiotic variable
#' @param env_lat is the latitude of the abiotic variable
#' @param bio_lon is the longitude of the biotic variable
#' @param bio_lat is the latitude of the biotic variable
#' @param env_variable is the value of the abiotic variable
#' 
#' @examples
#' data(datras_surveys)

match_env <- function(env_lon, env_lat, bio_lon, bio_lat, env_variable = NULL){
  alldist <- sapply(c(1:length(env_lon)), function(i) dist(rbind(c(env_lon[i], env_lat[i]), c(bio_lon, bio_lat))))
  if(is.null(env_variable)) return(which.min(alldist))
  else{return(env_variable[which.min(alldist)])}
}
