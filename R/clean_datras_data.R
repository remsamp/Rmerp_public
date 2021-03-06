#' Function to clean DATRAS survey data
#'
#' This function cleans up datras data uploaded into R, for example using the get_datras_data function.
#' DATRAS data is typically messy and gathers information collected using different sampling
#' protocols (gear type for instance). Analysing data across surveys without this cleaning step
#' is therefore unadvisable. \code{clean_data_datras} runs a basic clean 
#' of the uploaded data (see Details).
#' 
#' The majority of surveys on DATRAS are IBTS surveys. There are consistent in sampling protocol
#' from 1980 onwards. Among the other surveys only the English Beam Trawl survey have a consistent
#' sampling protocol through time and other sources of information are excluded.
#' Other cleaning rules include: gear consistency (GOV if IBTS survey or 4-meter beam trawl if
#' English Trawl survey); haul validity; haul duration (hauls are normally 30 minutes long but
#' can vary, we exclude overly short or long hauls).
#' 
#' @param survey is an R data.frame 
#' @param beam is a boolean argument. Default value is FALSE. If TRUE, the data comes from 
#' a beam trawl survey, rather than from the Internation Bottom Trawl Survey (IBTS).
#' 
#' @examples
#' data(datras_surveys)
#' mysurvey <- datras_surveys$survey[1]
#' myear <- datras_surveys$year[1]
#' myquarter <- datras_surveys$quarter[1]
#' mysurvey <- get_datras_data("HH", mysurvey, myear, myquarter)
#' mycleansurvey <- clean_datras_data(mysurvey, beam = F)

# some cleaning steps - optional but recommanded
clean_datras_data <- function(survey, beam = F){
  if(beam == F) condition1 <- survey$Gear == "GOV"
  else {condition1 <- survey$Gear == "BT4A" & survey$Country == "ENG"}
  # check for duration
  survey$HaulDur <- as.numeric(as.character(survey$HaulDur))
  condition2 <- survey$HaulDur > 27 & survey$HaulDur < 33  
  # check for validity
  condition3 <- survey$HaulVal == "V"
  survey[condition1 & condition2 & condition3, ]
}