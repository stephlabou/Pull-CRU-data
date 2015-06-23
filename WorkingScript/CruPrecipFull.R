##################################################################
############### Pull CRU values by lat and long ##################
############### sum precip values by sample date #################
##################################################################

setwd("C:/RPackages/netCDF")

##################################################################
#################### Kara updated code ###########################
##################################################################

library("ncdf")
library("plyr")
library("dplyr")

## Updated get_cru function
get_cru <- function(latlong, variable, folder = "."){ #made one variable latlong which will be a matrix
  CRU <- file.path(folder, sprintf('cru_ts3.22.1901.2013.pre.dat.nc', variable))
  nc  <-  open.ncdf(con = CRU)
  lat.vals  <-    get.var.ncdf(nc,varid="lat")
  lon.vals    <-    get.var.ncdf(nc,varid="lon")
  days.since <- get.var.ncdf(nc,varid="time") #still not sure what "time" is
  start.time = 1
  
  lat.i  <-  which.min(abs(lat.vals-latlong[1]))[1] #of the latlong matrix, choose lat
  lon.i  <-  which.min(abs(lon.vals-latlong[2]))[1] #of the latlong matrix, choose long
  
  vals = get.var.ncdf(nc=nc, varid=variable,start=c(lon.i,lat.i,start.time),count=c(1,1,length(days.since)))
  close.ncdf(nc)
  
  days.since = as.Date('1900-01-01')+days.since
  df <- data.frame("DateTime" = days.since, "vals" = vals,
                   "lat" = latlong[1], "long" = latlong[2])
  names(df)[2] <- variable
  return(df)
}

## Load data
full <- read.csv("full_under_ice_data_20150613.csv", stringsAsFactors = FALSE)

## Take all the sets of coordinates and split them into a list where each list
## element is a vector of length two where the first element is the latitude and
## the second is the longitude.
coords <- full %>%
  select(stationlat, stationlong) %>%
  unique() %>% # keep each unique set of coordinates
  filter(!is.na(stationlat) | !is.na(stationlong)) %>% # remove NAs
  as.matrix() %>% # convert to matrix
  split(row(.)) # split each row into a list item 

## Extract precipitation data for each pair of coordinates in the list, then
## combine into one big data frame.
precip <- lapply(coords, get_cru, variable = "pre")
precipdf <- do.call(rbind, precip)


############## provide unique ID to full data set #################

full$uniqueID <- 1:nrow(full)

#check

head(full)

summary(full$uniqueID)


################# subset precipdf to get correct years ###################

#do don't need anything from 1900-1939

#first sample from 1940 is 1940-01-16 (prior sample is 1939-12-16)

precip.subset <- subset(precipdf, DateTime > "1939-12-16")

#check that this worked

head(precip.subset)

str(precip.subset)

unique(precip.subset$DateTime)


############## convert abbrev months to num months #######################

#add as new column 

full$start.month.num <- match(full$startmonth, month.abb)

#do the same for endmonth

full$end.month.num <- match(full$endmonth, month.abb)

#check

head(full)


############################## kicking months forward ##########################

#to capture full month that sample occured in

full_adj <- full %>% 
  mutate(end.year.adj = ifelse(end.month.num >= 12, endyear + 1, endyear), #if December, goes to Jan of following year
         end.month.adj = ifelse(end.month.num >= 12, 1, end.month.num+1)) # if Dec, make it Jan, otherwise if not Dec, kicks forward one month


################# collapse sample yr/mm to one column ######################

#month is now an integer (separate column)

#year is also integer

library(zoo)

full_adj$start.ymm <- as.Date(as.yearmon(with(full_adj, paste(startyear, start.month.num, sep="-"))))

full_adj$end.ymm <- as.Date(as.yearmon(with(full_adj, paste(end.year.adj, end.month.adj, sep="-")))) #end date incorporates if need to kick forward to get whole period

#check

head(full_adj) 
                              
                              
###################### subset by CRU years available ############################
                              
# filtered to only include sample years during CRU period (not 2014 or 2015)
                              
fullfilt <- filter(full_adj, year < 2014)

## Keep only columns of interest
fullfilt.subset <- fullfilt[, c("uniqueID", "lakename", "season", "stationlat",
                                "stationlong", "start.ymm", "end.ymm")]
                              
                              
################ add a new filtered unique ID (subsetted years) #####################

uniqueID2 <- 1:nrow(fullfilt.subset)
                            
fullfilt.subset$filteredID <- uniqueID2
                              
#so there's the unique ID from the beginning
#took out all rows from 2014 and 2015
#left with 2237 rows, all of which have a unique "filtered ID"
 
#check

head(fullfilt.subset)
                              
################ round lat values (deals with potential later mismatch) ###################
                              
#round lat to 5 decimal places
                              
precip.subset$lat <- round(precip.subset$lat, 5)
                              
#need to also round for fulldf
                              
fullfilt.subset$stationlat <- round(fullfilt.subset$stationlat, 5)
                              
                              
###################### rename so merge will work ##############################

## Use dplyr's rename() function to rename "stationlat" and "stationlong" to
## "lat" and "long"
fullfilt.subset <- rename(fullfilt.subset, lat = stationlat, long = stationlong)                              
                              
########################## merge ###############################
                              
mergeddf <- merge(fullfilt.subset, precip.subset, by=c("lat", "long"))

#check

head(mergeddf)
str(mergeddf)


########### check to make sure all filtered uniqiue IDs are present #############

missing_cutoff <- uniqueID2[which(!uniqueID2 %in% mergeddf$filteredID)]

fullfilt.subset[fullfilt.subset$filteredID %in% missing_cutoff, ]

#only two values are missing - these are the two stations with NA for lat (Lake Simcoe)

#lat/long not included due to privacy concerns

                              
############## using the sample year/month as the cut off ####################
                              
#need to put all precip.subset$DateTime values that are between full.start.date and full.end.date
                              
#put all these in a new dataframe
                              
#can subset each row based on where DateTime falls relative to start and end date or samples
                              
merged.subset.bymonth <- subset(mergeddf, DateTime <= end.ymm & DateTime >= start.ymm) 
                              
#limited to when datetime is between start and end dates BY ACTUAL DATE

                              
##################### check filtered unique IDs again ########################   

missing_cutoff2 <- uniqueID2[which(!uniqueID2 %in% merged.subset.bymonth$filteredID)]

fullfilt.subset[fullfilt.subset$filteredID %in% missing_cutoff2, ]

#only ones missing are two NAs for Lake Simcoe (no lat/long included due to privacy concerns)

                       
################# sum precip data over period #######################
                              
sum_precip_bymonth <- merged.subset.bymonth %>% 
                          group_by(uniqueID, filteredID, lakename, lat, long, start.ymm, season) %>% 
                          dplyr::summarize(precip = sum(pre, na.rm = TRUE))

#plyr conflicts with dplyr so be sure to use dplyr summarize
                              
#this gives sum of precip for sample period (by actual sample date cut off)


######### merge with full data set ##########

#merge precip sums back into full dat set (using unique ID)

precip_full_merge <- merge(full, sum_precip_bymonth, by="uniqueID")

#this does lead to two full.start.date columns - will clean for final export

#rename column to be clear this is total precip
                            
names(precip_full_merge)[names(precip_full_merge) == "precip"] <- "total.CRU.precip"

                              
##############################################################
##################### extra code #############################
##############################################################
                              
                              
#################### open ncdf file #########################
                              
setwd("C:/RPackages/netCDF")

ncname <- "cru_ts3.22.1901.2013.pre.dat"
ncfname <- paste(ncname, ".nc", sep = "")
dname <- "tmpy"
                              
opencru <- open.ncdf(ncfname)
                              
                              
################### some code for QA/ QC #########################
                              
#check if unique ID values are missing
                              
missing_cutoff <- uniqueID2[which(!uniqueID2 %in% mergeddf$filteredID)]
                              
fullfilt.subset[fullfilt.subset$filteredID %in% missing_cutoff, ]
                              
                              
#check if values are matching up exactly 
                              
                           
preciplat <- precip.subset %>% filter(lat > 49 & lat < 49.67) %>% summarize(unique(lat))
                              
fulldflat <- fullfilt.subset %>% filter(lat > 49 & lat < 49.67) %>% summarize(unique(lat))
                              
                              
all.equal(fulldflat[1, 1], preciplat[1, 1]) 
                              
identical(fulldflat[1, 1], preciplat[1, 1]) 
                              
                              
sprintf("%.54f", fulldflat[1, 1])
                              
sprintf("%.54f", preciplat[1, 1])
                              
                              
                              
                              