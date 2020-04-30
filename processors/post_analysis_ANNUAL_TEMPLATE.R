### RENTAL LISTINGS POST-PROCESSING SKELETON CODE -- ANNUAL ###
# Date: April 23, 2020

# STEP 0: CODE PREP (PACKAGES, WORKING DIRECTORIES, SETTINGS)
# STEP 1: READ IN MOST UPDATED PARCEL DATA
# STEP 2: READ IN RENTAL LISTINGS DATA <<<--------- UPDATE WITH MOST RECENT RENTAL LISTINGS QUARTERLY DATA CSV!
# STEP 3: LINK RENTAL LISTINGS DATA TO PARCEL ID
# STEP 4: LINK RENTAL LISTINGS DATA TO ZIP CODE
# STEP 5: CLEAN UP YEAR BUILT VARIABLE BASED ON PARCEL DATA
# STEP 6: FINAL DATASET WRANGLING BEFORE ANALYSIS
# STEP 7: QUARTERLY SUMMARY STATISTICS
# STEP 8: WRITE TABLES

###################################### STEP 0: CODE PREP (PACKAGES, WORKING DIRECTORIES, SETTINGS) ######################################

# clear workspace
rm(list=ls())

options(scipen=999)
options(max.print=10000)

# Load packages
#install.packages("pacman") # pacman package allows for easy installing and loading of multiple packages in a single function
library(pacman)
pacman::p_load(plyr, dplyr, tidyverse, ggplot2, stringr, stringdist, ngram, sp, rgdal, raster, foreign, gtools)

# define working directories
outFilePath <- "K:/DataServices/Projects/Current_Projects/rental_listings_research/data/output/output_2018_clean"
spatialSrcPath <- "K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial"

today <- Sys.Date()
year<- format(today, "%Y")
month <- format(today, "%m")
day <- format(today, "%d")


###################################### STEP 1: READ IN MOST UPDATED PARCEL DATA ######################################
### READ IN PARCEL DATA ###
# parcel data (most recent) -- TAKES ABOUT ONE HOUR
setwd(spatialSrcPath)
parcels_mapc = readOGR(dsn = ".", layer = "ma_parcels_mapc", stringsAsFactors = FALSE)

#project the shapefiles to NAD83 
CRS.new <- CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000
                +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

proj4string(parcels_mapc) <- CRS.new

new_CRS <- "+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# the parcels file is a LARGE file (~3.6 GB). Therefore, let's break it up to make the spTransform step manageable for the computer
parcels_mapc_a = parcels_mapc[1:200000,]
parcels_mapc_b = parcels_mapc[200001:400000,]
parcels_mapc_c = parcels_mapc[400001:600000,]
parcels_mapc_d = parcels_mapc[600001:850388,]

rm(parcels_mapc)

# Isolate parcels from the five Rental Listings Consortium municipalities: Arlington, Boston, Cambrige, Quincy, and Somerville
# Why? The MAPC parcel full dataset is too large for my current RAM to handle, even with partitioning into four parts
parcels_consortium_a = parcels_mapc_a[parcels_mapc_a$muni %in% c("Arlington", "Boston", "Cambridge", "Quincy", "Somerville"),]
parcels_consortium_b = parcels_mapc_b[parcels_mapc_b$muni %in% c("Arlington", "Boston", "Cambridge", "Quincy", "Somerville"),]
parcels_consortium_c = parcels_mapc_c[parcels_mapc_c$muni %in% c("Arlington", "Boston", "Cambridge", "Quincy", "Somerville"),]
parcels_consortium_d = parcels_mapc_d[parcels_mapc_d$muni %in% c("Arlington", "Boston", "Cambridge", "Quincy", "Somerville"),]

rm(parcels_mapc_a, parcels_mapc_b, parcels_mapc_c, parcels_mapc_d)

# now perform spTransform on each partition of the consortium parcel data
parcels_consortium_a = spTransform(parcels_consortium_a, CRS(new_CRS))
parcels_consortium_b = spTransform(parcels_consortium_b, CRS(new_CRS))
parcels_consortium_c = spTransform(parcels_consortium_c, CRS(new_CRS))
parcels_consortium_d = spTransform(parcels_consortium_d, CRS(new_CRS))

# now recombine the parcel partitions to make the full, spTransform-ed MAPC parcel dataframe
parcels_consortium_abcd = spRbind(spRbind(spRbind(parcels_consortium_a, parcels_consortium_b), 
                                          parcels_consortium_c),parcels_consortium_d)

###################################### STEP 2: READ IN RENTAL LISTINGS DATA ######################################
##### EXAMPLE: 2018 DATA
#####2018 Q1
listings_unique_2018_q1 <- read.csv("K:/DataServices/Projects/Current_Projects/rental_listings_research/outputs/2018q1/rental-listings-2018Q1/geolocator/1524787772.721995_processed_listings.csv", stringsAsFactors = FALSE)
#####2018 Q2
listings_unique_2018_q2 <- read.csv("K:/DataServices/Projects/Current_Projects/rental_listings_research/outputs/2018q2/rental-listings-Q2/geolocator/1530904764.083565_processed_listings.csv", stringsAsFactors = FALSE)
#####2018 Q3
listings_unique_2018_q3 <- read.csv("K:/DataServices/Projects/Current_Projects/rental_listings_research/outputs/2018q3/rental-listings-Q3/geolocator/1540598923_processed_listings.csv", stringsAsFactors = FALSE)
#####2018 Q4
listings_unique_2018_q4 <- read.csv("K:/DataServices/Projects/Current_Projects/rental_listings_research/outputs/2018q4/rental-listings-2018Q4/geolocator/1549300121.139742_processed_listings.csv", stringsAsFactors = FALSE)


listings_unique_temp <- rbind.fill(listings_unique_2018_q1, 
                              listings_unique_2018_q2,
                              listings_unique_2018_q3,
                              listings_unique_2018_q4) %>%
  mutate(eight_bedroom = ifelse(numRooms == 8, eight_bedroom == 1, 0)) %>%
  dplyr::select(-1) # remove X variable (the first column) because it becomes redundant after combining the four dataframes
  


#categorize Quarter
colnames(listings_unique_temp)[which(names(listings_unique_temp) == "Year")] <- "year"
colnames(listings_unique_temp)[which(names(listings_unique_temp) == "Month")] <- "month"
listings_unique_temp$quarter [listings_unique_temp$year == 2018 & listings_unique_temp$month <= 3] = "2018 Q1"
listings_unique_temp$quarter [listings_unique_temp$year == 2018 & (listings_unique_temp$month > 3 & listings_unique_temp$month <= 6)] = "2018 Q2"
listings_unique_temp$quarter [listings_unique_temp$year == 2018 & (listings_unique_temp$month > 6 & listings_unique_temp$month <= 9)] = "2018 Q3"
listings_unique_temp$quarter [listings_unique_temp$year == 2018 & (listings_unique_temp$month > 9 & listings_unique_temp$month <= 12)] = "2018 Q4"


### LAG IN POSTED TO UPDATED TIME ###
# # calculate updated lag 
listings_unique_temp$update_lag <- as.Date(listings_unique_temp$updated_date) - as.Date(listings_unique_temp$post_date)

#replace lat long attributes with GEOLOCATED lat long 
listings_unique_temp$latitude_merge[is.na(listings_unique_temp$latitude_merge)] <- 0
listings_unique_temp$latitude_original <- listings_unique_temp$latitude

# source ID 1 = craigslist
# source ID 2 = padmapper
# only padmapper data should update their original longitude/latitude using the longitude_merge/latitude_merge variables
# craigslist data should keep its original longitude/latitude

listings_unique_temp$latitude <- ifelse(listings_unique_temp$latitude_merge < 0 & listings_unique_temp$source_id == 2,  
                                        listings_unique_temp$latitude_merge,
                                        listings_unique_temp$latitude)

listings_unique_temp$longitude_merge[is.na(listings_unique_temp$longitude_merge)] <- 0
listings_unique_temp$longitude_original <- listings_unique_temp$longitude

listings_unique_temp$longitude <- ifelse(listings_unique_temp$longitude_merge < 0 & listings_unique_temp$source_id == 2,  
                                         listings_unique_temp$longitude_merge,
                                         listings_unique_temp$longitude)

###################################### STEP 3: LINK RENTAL LISTINGS DATA TO PARCEL ID ######################################
# read lat long attributes of the listing records and create event points of WGS1984 geographic projection 
WCS1984_CRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
event.Points <- SpatialPoints(data.frame(latitude = listings_unique_temp$latitude, longitude = listings_unique_temp$longitude),
                              proj4string = CRS(WCS1984_CRS)) 

#reproject the points to NAD83
event.Points <- spTransform(event.Points, CRS(new_CRS))

#overlay any of the boundaries with the listing records
pnt_parcels_consortium <- over(event.Points, parcels_consortium_abcd)

#extract the names from each overlay
try (
  if("parloc_id" %in% colnames(pnt_parcels_consortium)) 
    parloc_id <- pnt_parcels_consortium$parloc_id
  else 
    stop("Error! No town attribute detected in town shapefile!")
)

try (
  if("yr_built" %in% colnames(pnt_parcels_consortium)) 
    yr_built <- pnt_parcels_consortium$yr_built
  else 
    stop("Error! No town attribute detected in town shapefile!")
)

try (
  if("luc_adj_1" %in% colnames(pnt_parcels_consortium)) 
    luc_adj_1 <- pnt_parcels_consortium$luc_adj_1
  else 
    stop("Error! No town attribute detected in town shapefile!")
)

try (
  if("luc_adj_2" %in% colnames(pnt_parcels_consortium)) 
    luc_adj_2 <- pnt_parcels_consortium$luc_adj_2
  else 
    stop("Error! No town attribute detected in town shapefile!")
)  

#add the parcel attributes to the listings
listings_unique_w_parcel <- cbind(listings_unique_temp, parloc_id, luc_adj_1, luc_adj_2, yr_built)


###################################### STEP 4: LINK RENTAL LISTINGS DATA TO ZIP CODE ######################################

# check is the files exist in the directory of dsn throw
# error if not available with releavant message and exit the method
list.files(spatialSrcPath, pattern='\\.shp$')

try(if (file.exists(paste0(spatialSrcPath,'/zip_MA.shp')) == FALSE) 
  stop("ZIP code file not available!"))

# read shape files of ZIP code boundaries
zip.shape <- readOGR(dsn=path.expand(spatialSrcPath), layer ="zip_MA")

# project the shapefiles to NAD83 
proj4string(zip.shape) <- CRS.new

zip.shape <- spTransform(zip.shape, CRS(new_CRS))

# overlay any of the boundaries with the listing records
pnt_zip.shape <- over(event.Points,zip.shape)

# add the parcel attributes to the listings
listings_unique <- cbind(listings_unique_w_parcel, pnt_zip.shape$postcode, pnt_zip.shape$pc_name)


###################################### STEP 5: CLEAN UP YEAR BUILT VARIABLE BASED ON PARCEL DATA ######################################
listings_unique$yr_built[is.na(listings_unique$yr_built)]  <- 0
listings_unique$yr_built <- as.numeric(levels(listings_unique$yr_built))[listings_unique$yr_built]

#### CAMBRIDGE HOUSING STARTS PERMITS ####
parcels_cambridge = parcels_consortium_abcd[parcels_consortium_abcd$muni == "Cambridge",]
camb_housingstarts = read.csv("K:/DataServices/Projects/Current_Projects/rental_listings_research/data/partner cities/Cambridge/cambridge_housingstarts_updated2019.csv")
camb_housingstarts_update = camb_housingstarts %>% separate(Location, c("Longitude", "Latitude"), sep = ",") 
camb_housingstarts_update$Longitude = as.numeric(gsub("\\(", "", camb_housingstarts_update$Longitude))
camb_housingstarts_update$Latitude = as.numeric(gsub("\\)", "", camb_housingstarts_update$Latitude))

camb_housingstarts.Points <- SpatialPoints(data.frame(latitude = camb_housingstarts_update$Latitude, longitude= camb_housingstarts_update$Longitude),
                                           proj4string = CRS(WCS1984_CRS)) 

#reproject the points to NAD83
camb_housingstarts.Points <- spTransform(camb_housingstarts.Points, CRS(new_CRS))

pnt_camb_housingstarts.shape <- over(camb_housingstarts.Points,parcels_cambridge)

camb_housingstarts_temp = cbind(camb_housingstarts_update, Muni = "Cambridge", State = "MA", parloc_id = pnt_camb_housingstarts.shape$parloc_id)

# there are many listings for some parcels -- either they've had several housing starts in the same year on the same parcel, or housing starts in different years
# here, we filter on the most recent housing start for each parcel, and if multiple housing starts happened in the same year, we filter on the Order
# Order = Unique identifier that orders cases by year, by fiscal year (if applicable), by street number, and by house number
# We select the highest order for a given parcel
camb_housingstarts_final = camb_housingstarts_temp %>% 
  rename(camb_yr_built = Year.Permitted) %>%
  group_by(parloc_id) %>%
  filter(camb_yr_built == (max(camb_yr_built)),
         Order == max(Order))

# Now we no longer have duplicate parloc_id observations, and each parloc_id is associated with the most recent year housing starts occurred on that parcel.

listings_unique_w_camb = left_join(listings_unique, 
                                   camb_housingstarts_final[, c("parloc_id","camb_yr_built")], 
                                   by = "parloc_id",
                                   na_matches = 'never') # require NAs and NaN to NOT be matched -- if you match NAs, then a parloc_id NA from Cambridge with a given yr_built value can be assigned to any other municipality's parloc_NA, giving their yr_built value a camb_yr_built value! We don't want that!

listings_unique_w_camb$camb_yr_built[is.na(listings_unique_w_camb$camb_yr_built)]  <- 0
listings_unique_w_camb$yr_built[listings_unique_w_camb$camb_yr_built >0 ] = listings_unique_w_camb$camb_yr_built[listings_unique_w_camb$camb_yr_built >0 ]

#### BOSTON HOUSING COMPLETIONS #### -- HOLD FOR NOW
bos_comp_older = read.csv("K:/DataServices/Projects/Current_Projects/rental_listings_research/data/partner cities/Boston/Completions_3-16-18_FOR MAPC_jointoParcels.csv")
bos_comp_update = read.csv("K:/DataServices/Projects/Current_Projects/rental_listings_research/data/partner cities/Boston/Boston Housing Completions 1.1.19 thru 9.27.19.csv")

bos_comp_older = bos_comp_older %>%
  dplyr::select(-parloc_id) %>%
  rename(parloc_id = "Ward...Parcel")

bos_comp_update$parloc_id = as.factor(bos_comp_update$Ward...Parcel)

# rbind the bos_comp_older and bos_comp_update datasets on matching column names
l <- list(bos_comp_older,
          bos_comp_update)

u = do.call(smartbind,l) 

bos_comp_final = u %>%
  dplyr::select(colnames(bos_comp_older))

bos_comp_final$bos_yr_built = as.Date(bos_comp_final$Complete.Date, "%m/%d/%Y")
bos_comp_final$bos_yr_built = format(bos_comp_final$bos_yr_built, "%Y")

listings_unique = left_join(listings_unique_w_camb,
                            bos_comp_final[, c("parloc_id","bos_yr_built")], 
                            by = "parloc_id",
                            na_matches = 'never')
listings_unique$bos_yr_built[is.na(listings_unique$bos_yr_built)]  <- 0
listings_unique$yr_built[listings_unique$bos_yr_built >0 ] = listings_unique$bos_yr_built[listings_unique$bos_yr_built >0 ]

#### YEAR BUILT CATEGORIZATION ####
listings_unique$periodblt [listings_unique$yr_built >= 2011] = "b_2011 or later"
listings_unique$periodblt [listings_unique$yr_built < 2011] = "a_Before 2011"
listings_unique$periodblt [listings_unique$yr_built == 0] = "d_NA"  

###################################### STEP 6: FINAL DATASET WRANGLING BEFORE ANALYSIS ######################################
### rooms for rent ###
#create keyWord list for studio and 1-10 bedrooms by calling com function

  listings_unique$roomrent <- grepl(" ROOM RENT | ROOMMATE | ROOMIE | ONE ROOM IN TWO | ONE BEDROOM IN TWO | ONE ROOM IN THREE | ONE BEDROOM IN THREE | ONE ROOM IN FOUR | ONE BEDROOM IN FOUR | ONE ROOM IN FIVE | ONE BEDROOM IN FIVE | ONE ROOM IN SIX | ONE BEDROOM IN SIX | ONE ROOM IN SEVEN | ONE BEDROOM IN SEVEN | ONE ROOM IN EIGHT | ONE BEDROOM IN EIGHT | ONE ROOM IN NINE | ONE BEDROOM IN NINE | ONE ROOM IN HOUSE | ONE BEDROOM IN HOUSE | ONE ROOM IN CONDO | ONE BEDROOM IN CONDO | ONE ROOM IN APARTMENT | ONE BEDROOM IN APARTMENT | ROOM AVAILABLE IN TWO | BEDROOM AVAILABLE IN TWO | ROOM AVAILABLE IN THREE | BEDROOM AVAILABLE IN THREE | ROOM AVAILABLE IN FOUR | BEDROOM AVAILABLE IN FOUR | ROOM AVAILABLE IN FIVE | BEDROOM AVAILABLE IN FIVE | ROOM AVAILABLE IN SIX | BEDROOM AVAILABLE IN SIX | ROOM AVAILABLE IN SEVEN | BEDROOM AVAILABLE IN SEVEN | ROOM AVAILABLE IN EIGHT | BEDROOM AVAILABLE IN EIGHT | ROOM AVAILABLE IN NINE | BEDROOM AVAILABLE IN NINE | ROOM AVAILABLE IN HOUSE | BEDROOM AVAILABLE IN HOUSE | ROOM AVAILABLE IN APARTMENT | BEDROOM AVAILABLE IN APARTMENT | ROOM AVAILABLE IN CONDO | BEDROOM AVAILABLE IN CONDO ", listings_unique$title)
  listings_unique$sublet <- grepl(" SUBLET ", listings_unique$title)
  listings_unique$shortterm <- grepl(" SHORT TERM ", listings_unique$title)
  listings_unique$shared <- grepl(" SHARED | SHARE ", listings_unique$title)

  # Reorder by column name
  listings_unique_mod <- listings_unique %>%
    dplyr::select("id",
                  "ask", 
                  "bedrooms", 
                  "numRooms", 
                  "title", 
                  "original_title",
                  "source_id", 
                  "survey_id",
                  "post_date",
                  "created_date",
                  "updated_date",
                  "update_lag",
                  "month",
                  "year",
                  "quarter",
                  "latitude",
                  "longitude",
                  "repeated_location",                         #2016
                  "ct10_id",
                  "muni_ID",
                  "muni",
                  "zip_code" = 'pnt_zip.shape$postcode',
                  "zip_muni" = 'pnt_zip.shape$pc_name',
                  "comm_type",
                  "neighborhood_01",
                  "neighborhood_02",
                  "neighborhood_03",
                  "studio",
                  "studio._not_in_range",
                  "one_bedroom",
                  "one_bedroom._not_in_range",
                  "two_bedroom",
                  "two_bedroom._not_in_range",
                  "three_bedroom",
                  "three_bedroom._not_in_range",
                  "four_bedroom",
                  "four_bedroom._not_in_range",
                  "five_bedroom",
                  "five_bedroom._not_in_range",
                  "six_bedroom","six_bedroom._not_in_range",
                  "seven_bedroom",
                  "seven_bedroom._not_in_range",
                  "eight_bedroom",                              #2016
                  "eight_bedroom._not_in_range",                #2016
                  #"nine_bedroom",                               #2016
                  #"nine_bedroom._not_in_range",                 #2016
                  "periodblt",
                  "roomrent",
                  "sublet",
                  "shortterm",
                  "shared",
                  "fwd_geolocated",
                  "rev_geolocated",
                  "mapzen_geolocated",
                  "mapzen_confidence",
                  "tract10",
                  "tract10_fwd_geocode",        #2016/?
                  "latitude_original",
                  "longitude_original",
                  "latitude_merge",
                  "longitude_merge",
                  "census_tract",               #2016/?
                  "ADDR_NUM_merge",             #2016/?
                  "BASE_merge",                 #2016/?
                  "COMMUNITY_Merge",            #2016/?
                  "joint_addresses_merge",      #2016/?
                  "post_at",
                  "created_at",
                  "updated_at",
                  "uniqueid",
                  "parloc_id",
                  "pid_long_15",
                  "pid_long_16",
                  "luc_adj_1",
                  "luc_adj_2",
                  "parloc_id",
                  "pid_long_15",
                  "pid_long_16",
                  "luc_adj_1",
                  "luc_adj_2",
                  "yr_built",
                  "camb_yr_built",
                  "bos_yr_built") %>%
    mutate(zip_code = as.character(zip_code))
  
  listings_unique_units <- filter(listings_unique_mod, 
                                  roomrent == "FALSE" & 
                                    sublet == "FALSE" & 
                                    shortterm == "FALSE" & 
                                    shared == "FALSE" & 
                                    update_lag<=30)
  
  # remove units from zip code 02108 that are below $700 ask and in two locations with high rates of probable scam listings 
  # (1 Acorn and 10 Willow)
  listings_unique_units_clean <- listings_unique_units %>%
    filter(!(zip_code == '02108' & (BASE_merge == 'Acorn' | BASE_merge == 'Willow') & ask < 700) & numRooms != -1) %>% # also filters out all listings with numRooms == -1
    mutate(neighborhood_01 = ifelse(muni == "ARLINGTON", ct10_id,  neighborhood_01)) # use census tract 2010 ID for Arlington neighborhood variable since we don't have neighborhood names yet
  
  listings_summary_counts <- ddply(listings_unique_units_clean,c("year", "month", "source_id"), summarise,
                                   rentcount = length(numRooms),
                                   medrent = round(median(ask), 0))
  
  rm(bos_comp, camb_housingstarts, CRS.new, event.Points, listings_unique, listings_unique_mod, 
     pnt_zip.shape, zip.shape)
  gc() # garbage collection -- tells us how much space we have remaining in memory
  
  ### POINT-LEVEL LISTINGS FULL UNITS (NO ROOMMATES ETC) ###
  
  listings_unique_units_boston <- filter(listings_unique_units_clean, muni == "BOSTON")
  listings_unique_units_cambridge <- filter(listings_unique_units_clean, muni == "CAMBRIDGE")
  listings_unique_units_quincy <- filter(listings_unique_units_clean, muni == "QUINCY")
  listings_unique_units_somerville <- filter(listings_unique_units_clean, muni == "SOMERVILLE")
  listings_unique_units_arlington <- filter(listings_unique_units_clean, muni == "ARLINGTON")
  
  
  ###################################### STEP 7: ANNUAL SUMMARY STATISTICS ######################################

   listings_summary_full_annual <- ddply(listings_unique_units_clean,c("numRooms", "year"), summarise,
                                    rentcount = length(numRooms),
                                    meanrent = round(mean(ask),0),
                                    medrent = round(median(ask), 0))
  
   listings_summary_muni_annual <- ddply(listings_unique_units_clean,c("muni","numRooms", "year"), summarise,
                                  rentcount = length(numRooms),
                                  meanrent = round(mean(ask),0),
                                  medrent = round(median(ask), 0))
   
   listings_summary_ct_annual <- ddply(listings_unique_units_clean,c("ct10_id","numRooms", "year"), summarise,
                                         rentcount = length(numRooms),
                                         meanrent = round(mean(ask),0),
                                         medrent = round(median(ask), 0))
  
   listings_summary_nhood_annual <- ddply(listings_unique_units_clean,c("neighborhood_01","muni","numRooms", "year"), summarise,
                                         rentcount = length(numRooms),
                                         meanrent = round(mean(ask),0),
                                         medrent = round(median(ask), 0))
  
   listings_summary_zip_annual <- ddply(listings_unique_units_clean,c("zip_code","muni","numRooms", "year"), summarise,
                                          rentcount = length(numRooms),
                                          meanrent = round(mean(ask),0),
                                          medrent = round(median(ask), 0))
   
   listings_summary_boston_annual <- filter(listings_summary_muni_annual, muni == "BOSTON")
   listings_summary_cambridge_annual <- filter(listings_summary_muni_annual, muni == "CAMBRIDGE")
   listings_summary_quincy_annual <- filter(listings_summary_muni_annual, muni == "QUINCY")
   listings_summary_somerville_annual <- filter(listings_summary_muni_annual, muni == "SOMERVILLE")
   listings_summary_arlington_annual <- filter(listings_summary_muni_annual, muni == "ARLINGTON")
   
   listings_summary_boston_nhood_annual <- filter(listings_summary_nhood_annual, muni == "BOSTON")
   listings_summary_cambridge_nhood_annual <- filter(listings_summary_nhood_annual, muni == "CAMBRIDGE")
   listings_summary_quincy_nhood_annual <- filter(listings_summary_nhood_annual, muni == "QUINCY")
   listings_summary_somerville_nhood_annual <- filter(listings_summary_nhood_annual, muni == "SOMERVILLE")
   listings_summary_arlington_nhood_annual <- filter(listings_summary_nhood_annual, muni == "ARLINGTON")
   
   listings_summary_full_annual_age <- ddply(listings_unique_units_clean,
                                                c("source_id","numRooms","year","periodblt"), summarise,
                                                rentcount = length(numRooms),
                                                meanrent = round(mean(ask),0),
                                                medrent = round(median(ask), 0))
  
   listings_summary_muni_annual_age <- ddply(listings_unique_units_clean,c("source_id","muni","numRooms","year","periodblt"), summarise,
                                                rentcount = length(numRooms),
                                                meanrent = round(mean(ask),0),
                                                medrent = round(median(ask), 0))
  
   listings_summary_muni_annual_age <- ddply(listings_unique_units_clean,c("source_id","ct10_id","numRooms","year","periodblt"), summarise,
                                             rentcount = length(numRooms),
                                             meanrent = round(mean(ask),0),
                                             medrent = round(median(ask), 0))
   
   listings_summary_nhood_annual_age <- ddply(listings_unique_units_clean,c("source_id","neighborhood_01","muni","numRooms","year","periodblt"), summarise,
                                                 rentcount = length(numRooms),
                                                 meanrent = round(mean(ask),0),
                                                 medrent = round(median(ask), 0))
   
   listings_summary_zip_annual_age <- ddply(listings_unique_units_clean,c("source_id","zip_code","muni","numRooms","year","periodblt"), summarise,
                                              rentcount = length(numRooms),
                                              meanrent = round(mean(ask),0),
                                              medrent = round(median(ask), 0))
  
  
   listings_summary_boston_annual_age <- filter(listings_summary_muni_annual_age, muni == "BOSTON")
   listings_summary_cambridge_annual_age <- filter(listings_summary_muni_annual_age, muni == "CAMBRIDGE")
   listings_summary_quincy_annual_age <- filter(listings_summary_muni_annual_age, muni == "QUINCY")
   listings_summary_somerville_annual_age <- filter(listings_summary_muni_annual_age, muni == "SOMERVILLE")
   listings_summary_arlington_annual_age <- filter(listings_summary_muni_annual_age, muni == "ARLINGTON")
   
  
   listings_summary_boston_nhood_annual_age <- filter(listings_summary_nhood_annual_age, muni == "BOSTON")
   listings_summary_cambridge_nhood_annual_age <- filter(listings_summary_nhood_annual_age, muni == "CAMBRIDGE")
   listings_summary_quincy_nhood_annual_age <- filter(listings_summary_nhood_annual_age, muni == "QUINCY")
   listings_summary_somerville_nhood_annual_age <- filter(listings_summary_nhood_annual_age, muni == "SOMERVILLE")
   listings_summary_arlington_nhood_annual_age <- filter(listings_summary_nhood_annual_age, muni == "ARLINGTON")
   
   listings_unique_units_fwd <- filter(listings_unique_units_clean, fwd_geolocated=="TRUE")
   listings_summary_full_annual_age_fwd <- ddply(listings_unique_units_fwd,
                                             c("numRooms","year","periodblt"), summarise,
                                             rentcount = length(numRooms),
                                             meanrent = round(mean(ask),0),
                                             medrent = round(median(ask), 0))

  
  ### QUARTERLY SUMMARIES ###
  listings_summary_full_quarterly <- ddply(listings_unique_units_clean,
                                           c("numRooms","quarter"), summarise,
                                           rentcount = length(numRooms),
                                           meanrent = round(mean(ask),0),
                                           medrent = round(median(ask), 0))
  
  listings_summary_muni_quarterly <- ddply(listings_unique_units_clean,c("muni","numRooms","quarter"), summarise,
                                 rentcount = length(numRooms),
                                 meanrent = round(mean(ask),0),
                                 medrent = round(median(ask), 0))
  
  
  listings_summary_nhood_quarterly <- ddply(listings_unique_units_clean,c("neighborhood_01","muni","numRooms","quarter"), summarise,
                                         rentcount = length(numRooms),
                                         meanrent = round(mean(ask),0),
                                         medrent = round(median(ask), 0))
  
  listings_summary_ct_quarterly <- ddply(listings_unique_units_clean,c("ct10_id","muni","numRooms","quarter"), summarise,
                                            rentcount = length(numRooms),
                                            meanrent = round(mean(ask),0),
                                            medrent = round(median(ask), 0))
  
  listings_summary_zip_quarterly <- ddply(listings_unique_units_clean,c("zip_code","muni","numRooms","quarter"), summarise,
                                         rentcount = length(numRooms),
                                         meanrent = round(mean(ask),0),
                                         medrent = round(median(ask), 0))

  listings_summary_boston_quarterly <- filter(listings_summary_muni_quarterly, muni == "BOSTON")
  listings_summary_cambridge_quarterly <- filter(listings_summary_muni_quarterly, muni == "CAMBRIDGE")
  listings_summary_quincy_quarterly <- filter(listings_summary_muni_quarterly, muni == "QUINCY")
  listings_summary_somerville_quarterly <- filter(listings_summary_muni_quarterly, muni == "SOMERVILLE")
  listings_summary_arlington_quarterly <- filter(listings_summary_muni_quarterly, muni == "ARLINGTON")
  
  listings_summary_boston_nhood_quarterly <- filter(listings_summary_nhood_quarterly, muni == "BOSTON")
  listings_summary_cambridge_nhood_quarterly <- filter(listings_summary_nhood_quarterly, muni == "CAMBRIDGE")
  listings_summary_quincy_nhood_quarterly <- filter(listings_summary_nhood_quarterly, muni == "QUINCY")
  listings_summary_somerville_nhood_quarterly <- filter(listings_summary_nhood_quarterly, muni == "SOMERVILLE")
  listings_summary_arlington_nhood_quarterly <- filter(listings_summary_nhood_quarterly, muni == "ARLINGTON")
  
  listings_summary_full_quarterly_age <- ddply(listings_unique_units_clean,
                                             c("source_id","numRooms","quarter","periodblt"), summarise,
                                             rentcount = length(numRooms),
                                             meanrent = round(mean(ask),0),
                                             medrent = round(median(ask), 0))
  
  listings_summary_muni_quarterly_age <- ddply(listings_unique_units_clean,c("source_id","muni","numRooms","quarter","periodblt"), summarise,
                                           rentcount = length(numRooms),
                                           meanrent = round(mean(ask),0),
                                           medrent = round(median(ask), 0))
  
  listings_summary_nhood_quarterly_age <- ddply(listings_unique_units_clean,c("source_id","neighborhood_01","muni","numRooms","quarter","periodblt"), summarise,
                                            rentcount = length(numRooms),
                                            meanrent = round(mean(ask),0),
                                            medrent = round(median(ask), 0))
  
  listings_summary_ct_quarterly_age <- ddply(listings_unique_units_clean,c("source_id","ct10_id","numRooms","quarter","periodblt"), summarise,
                                                rentcount = length(numRooms),
                                                meanrent = round(mean(ask),0),
                                                medrent = round(median(ask), 0))

  listings_summary_zip_quarterly_age <- ddply(listings_unique_units_clean,c("source_id","zip_code","numRooms","quarter","periodblt"), summarise,
                                             rentcount = length(numRooms),
                                             meanrent = round(mean(ask),0),
                                             medrent = round(median(ask), 0))
  
  
  listings_summary_boston_quarterly_age <- filter(listings_summary_muni_quarterly_age, muni == "BOSTON")
  listings_summary_cambridge_quarterly_age <- filter(listings_summary_muni_quarterly_age, muni == "CAMBRIDGE")
  listings_summary_quincy_quarterly_age <- filter(listings_summary_muni_quarterly_age, muni == "QUINCY")
  listings_summary_somerville_quarterly_age <- filter(listings_summary_muni_quarterly_age, muni == "SOMERVILLE")
  listings_summary_arlington_quarterly_age <- filter(listings_summary_muni_quarterly_age, muni == "ARLINGTON")
  
  listings_summary_boston_nhood_quarterly_age <- filter(listings_summary_nhood_quarterly_age, muni == "BOSTON")
  listings_summary_cambridge_nhood_quarterly_age <- filter(listings_summary_nhood_quarterly_age, muni == "CAMBRIDGE")
  listings_summary_quincy_nhood_quarterly_age <- filter(listings_summary_nhood_quarterly_age, muni == "QUINCY")
  listings_summary_somerville_nhood_quarterly_age <- filter(listings_summary_nhood_quarterly_age, muni == "SOMERVILLE")
  listings_summary_arlington_nhood_quarterly_age <- filter(listings_summary_nhood_quarterly_age, muni == "ARLINGTON")
  
  ########## WRITE TABLES ########################
  setwd(outFilePath)
  
  write.csv(listings_summary_counts, paste("listings_summary_counts_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  
  write.csv(listings_unique_units, paste("listings_unique_full_units_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  
  write.csv(listings_unique_units_clean, paste("listings_unique_clean_full_units_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_unique_units_boston, paste("listings_boston_unique_clean_full_units_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_unique_units_cambridge, paste("listings_cambridge_unique_clean_full_units_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_unique_units_quincy, paste("listings_quincy_unique_clean_full_units_","2018_",year,month,day,".csv", sep=""))
  write.csv(listings_unique_units_somerville, paste("listings_somerville_unique_clean_full_units_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_unique_units_arlington, paste("listings_arlington_unique_clean_full_units_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  
  # Annual tables
  write.csv(listings_summary_full_annual, paste("listings_summary_full_annual_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_muni_annual, paste("listings_summary_muni_annual_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_ct_annual, paste("listings_summary_ct_annual_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_nhood_annual, paste("listings_summary_nhood_annual_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_zip_annual, paste("listings_summary_zip_annual_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  
  write.csv(listings_summary_boston_annual, paste("listings_summary_boston_annual_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_cambridge_annual, paste("listings_summary_cambridge_annual_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_quincy_annual, paste("listings_summary_quincy_annual_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_somerville_annual, paste("listings_summary_somerville_annual_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_arlington_annual, paste("listings_summary_arlington_annual_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  
  write.csv(listings_summary_boston_nhood_annual, paste("listings_summary_boston_nhood_annual_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_cambridge_nhood_annual, paste("listings_summary_cambridge_nhood_annual_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_quincy_nhood_annual, paste("listings_summary_quincy_nhood_annual_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_somerville_nhood_annual, paste("listings_summary_somerville_nhood_annual_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_arlington_nhood_annual, paste("listings_summary_arlington_nhood_annual_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  
  # Annual tables with age
  write.csv(listings_summary_full_annual_age, paste("listings_summary_full_annual_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_muni_annual_age, paste("listings_summary_muni_annual_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_ct_annual_age, paste("listings_summary_ct_annual_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_nhood_annual_age, paste("listings_summary_nhood_annual_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_zip_annual_age, paste("listings_summary_zip_annual_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  
  write.csv(listings_summary_boston_annual_age, paste("listings_summary_boston_annual_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_cambridge_annual_age, paste("listings_summary_cambridge_annual_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_quincy_annual_age, paste("listings_summary_quincy_annual_age_","2018_",year,month,day,".csv", sep=""))
  write.csv(listings_summary_somerville_annual_age, paste("listings_summary_somerville_annual_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_arlington_annual_age, paste("listings_summary_arlington_annual_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  
  write.csv(listings_summary_boston_nhood_annual_age, paste("listings_summary_boston_nhood_annual_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_cambridge_nhood_annual_age, paste("listings_summary_cambridge_nhood_annual_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_quincy_nhood_annual_age, paste("listings_summary_quincy_nhood_annual_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_somerville_nhood_annual_age, paste("listings_summary_somerville_nhood_annual_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_arlington_nhood_annual_age, paste("listings_summary_arlington_nhood_annual_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  
  ###
  write.csv(listings_summary_full_quarterly,paste("listings_full_summary_quarterly_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_muni_quarterly, paste("listings_muni_summary_quarterly_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_ct_quarterly, paste("listings_ct_summary_quarterly_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_nhood_quarterly, paste("listings_nhood_summary_quarterly_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_zip_quarterly, paste("listings_zip_summary_quarterly_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  
  write.csv(listings_summary_boston_quarterly, paste("listings_boston_summary_quarterly_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_cambridge_quarterly, paste("listings_cambridge_summary_quarterly_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_quincy_quarterly, paste("listings_quincy_summary_quarterly_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_somerville_quarterly, paste("listings_somerville_summary_quarterly_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_arlington_quarterly, paste("listings_somerville_summary_quarterly_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  
  write.csv(listings_summary_boston_nhood_quarterly, paste("listings_boston_summary_nhood_quarterly_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_cambridge_nhood_quarterly, paste("listings_cambridge_summary_nhood_quarterly_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_quincy_nhood_quarterly, paste("listings_quincy_summary_nhood_quarterly_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_somerville_nhood_quarterly, paste("listings_somerville_summary_nhood_quarterly_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_arlington_nhood_quarterly, paste("listings_arlington_summary_nhood_quarterly_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  
  ###
  write.csv(listings_summary_full_quarterly_age, paste("listings_full_summary_quarterly_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_muni_quarterly_age, paste("listings_muni_summary_quarterly_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_ct_quarterly_age, paste("listings_ct_summary_quarterly_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_nhood_quarterly_age, paste("listings_nhood_summary_quarterly_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_zip_quarterly_age, paste("listings_zip_summary_quarterly_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  
  write.csv(listings_summary_boston_quarterly_age, paste("listings_boston_summary_quarterly_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_cambridge_quarterly_age, paste("listings_cambridge_summary_quarterly_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_quincy_quarterly_age, paste("listings_quincy_summary_quarterly_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_somerville_quarterly_age, paste("listings_somerville_summary_quarterly_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_arlington_quarterly_age, paste("listings_arlington_summary_quarterly_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  
  write.csv(listings_summary_boston_nhood_quarterly_age, paste("listings_boston_summary_nhood_quarterly_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_cambridge_nhood_quarterly_age, paste("listings_cambridge_summary_nhood_quarterly_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_quincy_nhood_quarterly_age, paste("listings_quincy_summary_nhood_quarterly_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_somerville_nhood_quarterly_age, paste("listings_somerville_summary_nhood_quarterly_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  write.csv(listings_summary_arlington_nhood_quarterly_age, paste("listings_arlington_summary_nhood_quarterly_age_","2018_",year,month,day,".csv", sep=""),row.names = FALSE)
  