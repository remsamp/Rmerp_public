# list.files <- "~/Google Drive/not_to_share/my_backup/myDocuments/postdoc/MERP_postdoc/temperature_data/ERSEM_SST_hindcast.nc"
# load one data set to get coordinates (they are the same for all years after that)
# library(ncdf4)
# dat_temp <- nc_open(list.files[1])

# to get the actual data we need to use the function ncvar_get(dat, varid = "BST")
# makes more sense to get BST here since it is demersal communities
# ersem_coord <- expand.grid(dat_temp$var$T$dim[[1]]$vals, dat_temp$var$T$dim[[2]]$vals)
# ersem_lon <- dat_temp$var$T$dim[[1]]$vals
# ersem_lat <- dat_temp$var$T$dim[[2]]$vals

# dim(ncvar_get(dat_temp, varid = "T"))
# ersem_temp_list <- vector("list", length = 275)
# for(i in 1:275){
#   ersem_temp_list[[i]] <- ncvar_get(dat_temp, varid = "T")[, , i]
#   print(i)
# }
# save(ersem_temp_list, file = "~/Google Drive/not_to_share/my_backup/myDocuments/postdoc/MERP_postdoc/temperature_data/ersem_monthly_sst.Rdata")
# that is still a 31.9mb object
# have this as internal data using devtools::use_data(x, mtcars, internal = TRUE)

# > range(dat_temp$var$T$dim[[1]]$vals). 198 steps so a point every 0.167
# [1] -19.83333  13.00000
# > range(dat_temp$var$T$dim[[2]]$vals). 224 steps so a point every 0.11
# [1] 40.11111 64.88889
# jan 1991 to nov 2013 (month 1 to 275)
# lon <- -7
# lat <- 49.5
# year <- 1997
# month <- 8
match_ersem_temp <- function(lon, lat, year, month){
  step <- 0.2
  nbmonth <- ((year - 1990 - 1) * 12 + month)
  coord_sub <- ersem_coord[ersem_coord[, 2] > (lat - step) & ersem_coord[, 2] < (lat + step) & ersem_coord[, 1] > (lon - step) & ersem_coord[, 1] < (lon + step), ]
  alldist <- sapply(c(1:nrow(coord_sub)), function(i) dist(rbind(coord_sub[i, 1:2], c(lon, lat))))
  res <- as.numeric(coord_sub[which.min(alldist), 1:2])
  mylon <- match(res[1], ersem_lon)
  mylat <- match(res[2], ersem_lat)
  ersem_temp_list[[nbmonth]][mylon, mylat]
}



# library(lubridate)
# unicorn$date <- as.character(unicorn$date)
# unicorn$month <- sapply(c(1:nrow(unicorn)), function (x) strsplit(unicorn$date[x], split = "/", fixed = T)[[1]][2])
# unicorn$year <- as.numeric(sapply(c(1:nrow(unicorn)), function (x) strsplit(unicorn$date[x], split = "/", fixed = T)[[1]][3]))
# unicorn$year[unicorn$year > 90] <- 1900 + unicorn$year[unicorn$year > 90]
# unicorn$year[unicorn$year < 50] <- 2000 + unicorn$year[unicorn$year < 50]
# unicorn$surface_temp <- rep(NA, nrow(unicorn))
# coord <- expand.grid(dat$var$SBT$dim[[1]]$vals, dat$var$SBT$dim[[2]]$vals)

# massive amount of data in obis_dat but not so many unique lat, lon and dates combination
# unicorn$ref <- paste(unicorn$year, unicorn$month, unicorn$whichlat, unicorn$whichlon, sep = "_")
# list_ref <- levels(factor(unicorn$ref))
# # bottom_temp <- rep(NA, length(list_ref))
# surface_temp <- rep(NA, length(list_ref))
# 
# checking <- as.numeric(sapply(c(1:length(list_ref)), function (x) strsplit(list_ref[x], split = "_")[[1]][1]))
# idx <- which(checking >= 1991 & checking <= 2013)


# for(u in 1:length(list_ref))
# {
#   #   # choose year and get the corresponding data
#   # u <- 1
#   elements <- strsplit(list_ref[u], split = "_")[[1]]
#   myear <- as.numeric(elements[1])
#   mymonth <- as.numeric(elements[2])
#   if(myear >= 1991 & myear <= 2013){
#     myear1 <- myear - 1990
#     # dat <- nc_open(list.files[myear1])
#     
#     # find the appropriate day
#     mynbdays <- as.numeric(elements[2])
#     temp_r <- ncvar_get(dat_temp, varid = "T")[, , mynbdmonths]
#     
#     # find the appropriate locations
#     mylat <- as.numeric(elements[3])
#     mylon <- as.numeric(elements[4])
#     # bottom_temp[idx[u]] <- temp_r[mylon, mylat]
#     surface_temp[idx[u]] <- temp_r_surface[mylon, mylat]
#   }
#   print(u)
# }