# this is to match in space and time environmental data with biological records
# this is a way to match 

# mydata <- data.frame(lon = runif(1000, min = -10, max = 10), lat = runif(1000, min = 40, max = 60), temperature = runif(1000, min = 10, max = 20))
match_env <- function(env_lon, env_lat, bio_lon, bio_lat, env_variable = NULL){
  alldist <- sapply(c(1:length(env_lon)), function(i) dist(rbind(c(env_lon[i], env_lat[i]), c(bio_lon, bio_lat))))
  if(is.null(env_variable)) return(which.min(alldist))
  else{return(env_variable[which.min(alldist)])}
}
# match_env(mydata$lon, mydata$lat, -7, 49.5)
# match_env(mydata$lon, mydata$lat, -7, 49.5, mydata$temperature)
