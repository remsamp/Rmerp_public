#' Function to upload DATRAS survey data into R
#'
#' This function uploads DATRAS data from one quarter and one year at a time, for a given survey.
#' Several other tools are available to the R user to perform this task, including
#' the DATRAS R package. A more sophisticated approach has been developed recently by Scott Large
#' and is available at http://www.ices.dk/marine-data/Documents/DATRAS/DATRAS_R.pdf.
#' Currently this code only exists as a pdf and would require a complete
#' re-write as an R script.
#' 
#' \code{get_datras_data} achieves the same result as Scott Large's code,
#' but leaves the task of aggregating surveys (if multiple surveys are uploaded) to the user. 
#' Uploading the data is a slow process and partly depends on internet speed. Parallelising 
#' the code as done by Scott Large will potentially speed things up, although available
#' bandwith will always be a problem. Note that a list of available surveys, years and quarters is stored 
#' in the data.frame "datras_surveys", which can be looked up using data(datras_surveys)
#' 
#' @param whatinfo is a character variable "HH", "HL" or "CA".
#' "HH" is haul information, "HL" is haul contents by size, and "CA" is haul contents by age
#' @param survey is a character variable matching the survey code wanted
#' @param year is a character variable for the year wanted
#' @param quarter is a character variable for the quarter wanted
#' 
#' 
#' @examples
#' data(datras_surveys)
#' mysurvey <- datras_surveys$survey[1]
#' myear <- datras_surveys$year[1]
#' myquarter <- datras_surveys$quarter[1]
#' get_datras_data("HH", mysurvey, myear, myquarter)

get_datras_data <- function(whatinfo, survey, year, quarter){
  whichAPI <- switch(whatinfo,HH = "getHHdata",HL = "getHLdata",CA = "getCAdata")
  myDATRASurl <- paste("https://datras.ices.dk/WebServices/DATRASWebService.asmx/",whichAPI,"?survey=",survey,"&year=",year,"&quarter=",quarter,sep="")
  myxml <- xmlTreeParse(getURL(myDATRASurl))
  xmltop = xmlRoot(myxml)
  data.frame(t(xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))),row.names=NULL)
}