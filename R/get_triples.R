#' Access RDF triples at nerc or bodc sparql endpoints
#'
#' The function makes use of one of two sparql endpoints that can be used together to access BODC data. The first endpoint is the nerc vocab.
#' It contains the list of appropriate terms needed to query the bodc endpoint.  
#' the function returns a 3-column table with names s, p and o for subject, predicate and object
#' this is the typical "triple" format of RDFs, the building block of linked data
#'
#' @param mylimit is the number of triples to extract. 
#' It can be string or integer (the function lets you use numeric and converts it internally). 
#' Note that this operation can take a while if mylimit is high (>1e4). We recommend you do not try 
#' to visualize all triples at once
#' @param nerc is boolean TRUE/FALSE with default TRUE. If TRUE (the default), access triples from nerc vocab endpoint. If FALSE, triples at BODC enpoint are queried. 
#' @param named_graph is boolean TRUE/FALSE with default FALSE. Named graphs are sets of triples, rather than individual triples
#'
#' 

get_triples <- function(mylimit, nerc = FALSE, named_graph = FALSE){
  endpoint <- ifelse(nerc == TRUE, "http://vocab.nerc.ac.uk/sparql/sparql", "http://linked.bodc.ac.uk/sparql/")
  
  if(!is.integer(mylimit)) mylimit <- as.integer(mylimit)
  if(!is.character(mylimit)) mylimit <- as.character(mylimit)
  if(named_graph == FALSE) {
  myquery <- paste("SELECT ?s ?p ?o 
  WHERE{
	?s ?p ?o
  }
  limit ", mylimit, sep = "")}
  else {
    myquery <- paste("SELECT ?g ?s ?p ?o  
  WHERE{
	{?s ?p ?o}
	UNION
	{GRAPH ?g {?s ?p ?o} }
  }
  limit ", mylimit, sep = "")}
  
  qd <- SPARQL(url = endpoint, query = myquery)
  qd$results
}
# pb with at least one entry (triple 518) because of character strings not in standard unambiguous format in the object part of the triple

# try this
# mysparql <- function(url = "http://localhost/", query = "", update = "", ns = NULL, param = "", extra = NULL, format = "xml", curl_args = NULL, parser_args = NULL) 
# {
    # extrastr <- ""
    # tf <- tempfile()
    # if (query != "") {
        # if (param == "") {
            # param <- "query"
        # }
        # if (format == "xml") {
            # tf <- append(list(url = paste(url, "?", param, "=", gsub("\\+", "%2B", URLencode(query, reserved = TRUE)), extrastr, sep = ""), httpheader = c(Accept = "application/sparql-results+xml")), curl_args)
        # }
        # list(url.r = tf$url)
    # }
# }


# endpoint <- "http://linked.bodc.ac.uk/sparql/"
# query <- "SELECT * WHERE { ?s ?p ?o .} LIMIT 518"
# rem.r <- xmlTreeParse(getURL(paste(mysparql(endpoint, query),"&output=xml",sep=""), HTTPHeader = c(Accept = "application/sparql-results+xml")) , useInternalNodes = TRUE)
# res <- xmlToList(rem.r)[["results"]]
# # each element of res has 3 bindings corresponfing to s, p and o

# s.vect <- character(length = length(res))
# p.vect <- character(length = length(res))
# o.vect <- character(length = length(res))
# for(i in 1:length(res)){
  # s.vect[i] <- ifelse(!is.null(res[[i]][1]$binding$uri), res[[i]][1]$binding$uri, res[[i]][1]$binding$literal)
  # p.vect[i] <- ifelse(!is.null(res[[i]][2]$binding$uri), res[[i]][2]$binding$uri, res[[i]][2]$binding$literal)
  # o.vect[i] <- ifelse(!is.null(res[[i]][3]$binding$uri), res[[i]][3]$binding$uri, res[[i]][3]$binding$literal)}
# mydata <- data.frame(s = unlist(s.vect), p = unlist(p.vect), o = unlist(o.vect))

# there is lots of cookbook recipes in the learn SPARQL book that can be easily implemented
# they will allow exploring the bodc endpoint, but not accessing the data at the moment

