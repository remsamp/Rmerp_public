Rmerp
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
##### Functions to access, visualise and combine marine data

Installation
------------

Currently the Rmerp package is in a private Github repository. Installation in R requires a github account and an authenticating key. Please see document on Strathcloud for details.

``` r
# library(devtools)
# devtools::install_github(repo = "remsamp/Rmerp", auth_token = mykey)
# library(Rmerp)
```

The Rmerp package is a set of functions that facilitate the collation of marine data from multiple sources for MERP users. This is still very much a development version, a draft that will be improved upon from interacting with users as they run into bugs or need additional functionalities. Any requests or problems can be reported on github by clicking on the issues tab.

Below is the list of available functions:
+ grid\_data
+ grid\_diversity
+ match\_env
+ match\_ersem\_temp
+ temperature\_distr
+ get\_datras\_data
+ clean\_datras\_data
+ manuela\_to\_table
+ emodnet\_getallparam
+ emodnet\_getallplatform
+ emodnet\_getallplatformroos
+ bodc\_classes
+ bodc\_search\_string
+ bodc\_get\_triples

Each function has a help file which can be accessed in R by typing ? followed by the name of the function.

The Rmerp package is very much designed to be used alongside existing packages, notably robis, taxizesoap and mregions (for data enrichment) and ggplot2, ggmap and ggvis (for mapping and visualisation).

Examples of workflows will be included after the Annual Science Meeting after discussing with interested members.
