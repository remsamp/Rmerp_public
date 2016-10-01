#' Convert MANUELA to table
#'
#' The function takes the meiofauna database MANUELA
#' and uses dplyr to merge its tables into a data.frame and/or csv file.
#' There are 14 tables in total, some of them containing 
#' biotic variables and others abiotic variables.
#' 
#' \code{manueladb_to_table} currently merges 4 of the tables together.
#' The result is a data frame containing all biotic observations
#' as well as their accompanying spatial and temporal information.
#' The database itself is on Strathcloud in module1/data
#' and needs to be downloaded on the user's computer. WE HAVE BEEN 
#' ASKED BY THE DATA PROVIDER TO NOT SHARE IT BEYOND THE MERP COMMUNITY.
#' 
#' @param path_to_manuela is the path to the local
#' copy of the database on the user's computer.  
#' 
#' @examples
#' manuela <- manueladb_to_table("~/manuela")
#' class(manuela)

manueladb_to_table <- function(path_to_manuela){
  con <- dbConnect(RSQLite::SQLite(), path_to_manuela)
  list_tables <- dbListTables(con)
  list_data <- vector("list", length(list_tables))
  for(i in 1:length(list_tables)){
    statement <- paste("SELECT * from ",list_tables[i], sep = "") # check what is in the database, simplest possible statement
    list_data[[i]] <- dbGetQuery(con,statement)
  }
  names(list_data) <- list_tables

  manuela <- left_join(list_data$slices, list_data$samples, by = c("sample_id" = "id"))
  manuela <- left_join(manuela, list_data$stations, by = c("station_id" = "id"))
  manuela <- left_join(list_data$counts, manuela, by = c("slice_id" = "id"))
  
  return(manuela)
}