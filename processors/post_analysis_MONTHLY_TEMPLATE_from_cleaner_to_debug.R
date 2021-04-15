 #!/usr/bin/env Rscript
### RENTAL LISTINGS POST-PROCESSING SKELETON CODE -- QUARTERLY ###
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
options(scipen=999)

# Load packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(stringdist)
library(ngram)
library(sp)
library(rgdal)
library(raster)
library(foreign)
library(gtools)
library(here)
library(rgeos)
library(maptools)
library(sf)
library(lubridate)

today <- Sys.Date()
year<- format(today, "%Y")
month <- format(today, "%m")
day <- format(today, "%d")

municipalities <- c("BOSTON","CAMBRIDGE","QUINCY","SOMERVILLE","ARLINGTON")

opt <- vector()

opt$year <- lubridate::year(Sys.Date())
opt$month <- lubridate::month(Sys.Date())-1

file <- "K:/DataServices/Projects/Current_Projects/rental_listings_research/r_scripts/analysis/rental-listings-data-analysis/data/rental-listings_3-2021/rental-listings_3-2021/cleaner/1617807323.35616_listings_unique.csv"

###################################### STEP 1: READ IN MOST UPDATED PARCEL DATA ######################################
load(file = here("data", "spatial", "parcels_consortium_abcd.RData"))

epsg_26986_crs_but_slightly_wrong <- CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
CRS.new <- CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
epsg_26986_crs <- 
CRS("+proj=lcc 
+lat_1=42.68333333333333 
+lat_2=41.71666666666667 
+lat_0=41 
+lon_0=-71.5 
+x_0=200000 
+y_0=750000 
    +ellps=GRS80 
    +datum=NAD83 
    +units=m 
    +no_defs")


parcels_consortium_abcd <- 
  parcels_consortium_abcd %>% 
  st_as_sf() %>% 
  st_set_crs(.,
             "+proj=lcc
             +lat_0=41
             +lon_0=-71.5
             +lat_1=41.7166666666667
             +lat_2=42.6833333333333
             +x_0=200000
             +y_0=750000
             +ellps=GRS80
             +towgs84=0,0,0,0,0,0,0
             +units=m
             +no_defs") %>%
  st_transform(.,
               st_crs(26986))

  

###################################### STEP 2: READ IN RENTAL LISTINGS DATA ######################################
if( file.access(file) == -1) {
    stop(sprintf("Specified file ( %s ) does not exist", file))
} else {
    listings_unique <- read.csv(file, stringsAsFactors = FALSE)  %>%
      mutate(eight_bedroom = ifelse(numRooms == 8, eight_bedroom == 1, 0)) %>%
      dplyr::select(-1) # remove X variable (the first column) because it becomes redundant after combining the four dataframes
}

#categorize Quarter
colnames(listings_unique)[which(names(listings_unique) == "Year")] <- "year"
colnames(listings_unique)[which(names(listings_unique) == "Month")] <- "month"

### LAG IN POSTED TO UPDATED TIME ###
# # calculate updated lag
listings_unique$update_lag <- as.Date(listings_unique$updated_date) - as.Date(listings_unique$post_date)

###################################### STEP 3: LINK RENTAL LISTINGS DATA TO PARCEL ID ######################################
# read lat long attributes of the listing records and create event points of WGS1984 geographic projection

event.Points <-
  listings_unique %>% 
  as_tibble() %>% 
  st_as_sf(coords = c("longitude","latitude"),
           crs = 4326) %>% 
  st_transform(.,
               crs = 26986)

# ggplot()+
#   geom_sf(data = parcels_consortium_abcd,
#           aes(geometry = geometry))+
#   geom_sf(data = event.Points,
#           aes(geometry = geometry))+
# coord_sf(xlim = c(224474.9, 252005.6), 
#          ylim = c(883835.7, 909577.2), expand = TRUE)

listings_unique_w_parcel <- 
  st_join(event.Points %>% 
            dplyr::select(!c(luc_adj_2, luc_adj_1, yr_built, parloc_id)),
          parcels_consortium_abcd %>%
            dplyr::select(luc_adj_2, luc_adj_1, yr_built, parloc_id),
          left = TRUE)

###################################### STEP 4: LINK RENTAL LISTINGS DATA TO ZIP CODE ######################################


zip.shape <- read_sf("data/spatial/zip_MA.shp")

listings_unique <- 
  st_join(listings_unique_w_parcel %>% 
            dplyr::select(!c(`pnt_zip.shape$postcode`, `pnt_zip.shape$pc_name`)),
          zip.shape %>%
            dplyr::select(postcode, pc_name),
          left = TRUE)


###################################### STEP 5: CLEAN UP YEAR BUILT VARIABLE BASED ON PARCEL DATA ######################################

listings_unique <- 
  listings_unique %>% 
  mutate(yr_built = as.numeric(yr_built),
         yr_built = replace_na(yr_built, 0))

#### CAMBRIDGE HOUSING STARTS PERMITS ####

parcels_cambridge <- 
  parcels_consortium_abcd %>% 
  filter(muni == "Cambridge")

camb_housingstarts <- read_csv("data/partners/Cambridge/cambridge_housingstarts_updated2019.csv")
  separate(Location, c("latitude", "longitude"), sep = ",") %>% 
  mutate(longitude = as.numeric(gsub("\\)", "", longitude)),
         latitude = as.numeric(gsub("\\(", "", latitude)))


#####
camb_housingstarts.Points <-
  
camb_housingstarts %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(.,
               crs = 26986)



  camb_housingstarts_final <- 
    st_join(camb_housingstarts.Points,
            parcels_cambridge) %>% 
    mutate(Muni = "Cambridge", 
           State = "MA") %>%
    rename(camb_yr_built = `Year Permitted`) %>% 
    group_by(parloc_id) %>%
    filter(camb_yr_built == (max(camb_yr_built)),
           Order == max(Order)) %>% 
    ungroup()

# there are many listings for some parcels -- either they've had several housing starts in the same year on the same parcel, or housing starts in different years
# here, we filter on the most recent housing start for each parcel, and if multiple housing starts happened in the same year, we filter on the Order
# Order = Unique identifier that orders cases by year, by fiscal year (if applicable), by street number, and by house number
# We select the highest order for a given parcel
 


# Now we no longer have duplicate parloc_id observations, and each parloc_id is associated with the most recent year housing starts occurred on that parcel.

listings_unique_w_camb <- left_join(listings_unique,
                                      camb_housingstarts_final %>% 
                                        dplyr::select(parloc_id, camb_yr_built) %>% 
                                        st_drop_geometry(),
                                      by = "parloc_id",
                                      na_matches = 'never') # require NAs and NaN to NOT be matched -- if you match NAs, then a parloc_id NA from Cambridge with a given yr_built value can be assigned to any other municipality's parloc_NA, giving their yr_built value a camb_yr_built value! We don't want that!
listings_unique_w_camb <-
listings_unique_w_camb %>% 
mutate(camb_yr_built = replace_na(camb_yr_built, 0))

listings_unique_w_camb$yr_built[listings_unique_w_camb$camb_yr_built >0 ] = listings_unique_w_camb$camb_yr_built[listings_unique_w_camb$camb_yr_built >0 ]

#### BOSTON HOUSING COMPLETIONS #### -- HOLD FOR NOW

bos_comp_older <- read_csv("data/partners/Boston/Completions_3-16-18_FOR MAPC_jointoParcels.csv",
                           col_names = TRUE,
                           cols(Project = col_character(),
                                `Street Number` = col_character(),
                                `Street Name` = col_character(),
                                `Total New Units` = col_character(),
                                `Complete Date` = col_character(),
                                `COO Number` = col_character(),
                                `Permit #` = col_character(),
                                Agency = col_character(),
                                `Planning District` = col_character(),
                                `Market Area` = col_character(),
                                `TOD 5 Min Walk` = col_character(),
                                `TOD 1/2 Mile` = col_character(),
                                `Ward & Parcel` = col_character(),
                                `parloc_id` = col_character(),
                                `Census Tract` = col_character(),
                                `Council District` = col_character(),
                                LAT = col_character(),
                                LONG = col_character(),
                                DESCRIPTION = col_character()),
                           locale(encoding = "latin1")) %>% 
  janitor::clean_names()

bos_comp_update <- read_csv("data/partners/Boston/Boston Housing Completions 1.1.19 thru 9.27.19.csv",
                            col_names = TRUE,
                            cols(Project = col_character(),
                                 `St Number` = col_character(),
                                 `Street Name` = col_character(),
                                 Agency = col_character(),
                                 `Planning District` = col_character(),
                                 `Market Area` = col_character(),
                                 `TOD 5 Min Walk` = col_double(),	
                                 `TOD 1/2 Mile` = col_double(),
                                 `Ward & Parcel` = col_character(),
                                 `Census Tract` = col_character(),
                                 `Council District` = col_double(),
                                 LAT = col_character(),
                                 LONG = col_character(),
                                 `Permit #` = col_character(),
                                 `Complete Date` = col_character(),	
                                 `COO Number` = col_character(),
                                 `Total New Units` = col_character(),
                                 `New Owner Units` = col_character(),
                                 `New Rental Units` = col_character(),
                                 `Afford New Units` = col_character(),
                                 `Afford New Owner Units` = col_character(),
                                 `Afford New Rental Units` = col_character(),
                                 DESCRIPTION = col_character()),
                            locale(encoding = "latin1")) %>% 
  janitor::clean_names()

bos_comp_older <-
  bos_comp_older %>%
  mutate(bos_yr_built = year(mdy(complete_date))) %>%
  dplyr::select(parloc_id, bos_yr_built)

listings_unique <- left_join(listings_unique_w_camb,
                             bos_comp_older %>% dplyr::select(parloc_id, bos_yr_built),
                            by = "parloc_id",
                            na_matches = 'never') %>% 
  mutate(bos_yr_built = replace_na(bos_yr_built, 0))

listings_unique$yr_built[listings_unique$bos_yr_built >0 ] = listings_unique$bos_yr_built[listings_unique$bos_yr_built >0 ]

#### YEAR BUILT CATEGORIZATION ####
listings_unique <- 
  listings_unique %>% 
  mutate(periodblt = case_when(yr_built >= 2011 ~ "b_2011 or later",
                               yr_built < 2011 ~ "a_Before 2011",
                               yr_built == 0 ~ "d_NA"))

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
                #"latitude",
                #"longitude",
                "ct10_id",
                "muni_ID",
                "muni",
                "zip_code" = 'postcode',
                "zip_muni" = 'pc_name',
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
                "periodblt",
                "roomrent",
                "sublet",
                "shortterm",
                "shared",
                "post_at",
                "created_at",
                "updated_at",
                "uniqueid",
                "parloc_id",
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
listings_unique_units_clean <- 
  listings_unique_units %>%
  st_transform(crs = 4326) %>% 
  mutate(longitude =  sf::st_coordinates(.)[,1],
         latitude = sf::st_coordinates(.)[,2]) %>%
  filter(!(zip_code == '02108' & ask < 700) & numRooms != -1) %>% # also filters out all listings with numRooms == -1
  mutate(neighborhood_01 = ifelse(muni == "ARLINGTON", ct10_id,  neighborhood_01)) %>% # use census tract 2010 ID for Arlington neighborhood variable since we don't have neighborhood names yet
st_drop_geometry()

listings_unique_units_clean_shp <- 
  listings_unique_units %>%
  st_transform(crs = 4326) %>% 
  mutate(longitude =  sf::st_coordinates(.)[,1],
         latitude = sf::st_coordinates(.)[,2]) %>%
  filter(!(zip_code == '02108' & ask < 700) & numRooms != -1) %>% # also filters out all listings with numRooms == -1
  mutate(neighborhood_01 = ifelse(muni == "ARLINGTON", ct10_id,  neighborhood_01)) %>% # use census tract 2010 ID for Arlington neighborhood variable since we don't have neighborhood names yet
st_transform(crs = 26986)

listings_summary_counts <- plyr::ddply(listings_unique_units_clean,c("year", "month", "source_id"), summarise,
                                 rentcount = length(numRooms),
                                 medrent = round(median(ask), 0))

rm(camb_housingstarts, CRS.new, event.Points, listings_unique, listings_unique_mod,
   pnt_zip.shape, zip.shape)
gc() # garbage collection -- tells us how much space we have remaining in memory

###################################### STEP 7: MONTHLY SUMMARY STATISTICS ######################################

listings_summary_full <- plyr::ddply(listings_unique_units_clean,
                                         c("numRooms"), summarise,
                                         rentcount = length(numRooms),
                                         meanrent = round(mean(ask),0),
                                         medrent = round(median(ask), 0))

listings_summary_muni <- plyr::ddply(listings_unique_units_clean,c("muni","numRooms"), summarise,
                                         rentcount = length(numRooms),
                                         meanrent = round(mean(ask),0),
                                         medrent = round(median(ask), 0))


listings_summary_nhood <- plyr::ddply(listings_unique_units_clean,c("neighborhood_01","muni","numRooms"), summarise,
                                          rentcount = length(numRooms),
                                          meanrent = round(mean(ask),0),
                                          medrent = round(median(ask), 0))

listings_summary_ct <- plyr::ddply(listings_unique_units_clean,c("ct10_id","muni","numRooms"), summarise,
                                       rentcount = length(numRooms),
                                       meanrent = round(mean(ask),0),
                                       medrent = round(median(ask), 0))

listings_summary_zip <- plyr::ddply(listings_unique_units_clean,c("zip_code","muni","numRooms"), summarise,
                                        rentcount = length(numRooms),
                                        meanrent = round(mean(ask),0),
                                        medrent = round(median(ask), 0))

listings_summary_full_age <- plyr::ddply(listings_unique_units_clean,
                                             c("source_id","numRooms","periodblt"), summarise,
                                             rentcount = length(numRooms),
                                             meanrent = round(mean(ask),0),
                                             medrent = round(median(ask), 0))

listings_summary_muni_age <- plyr::ddply(listings_unique_units_clean,c("source_id","muni","numRooms","periodblt"), summarise,
                                             rentcount = length(numRooms),
                                             meanrent = round(mean(ask),0),
                                             medrent = round(median(ask), 0))

listings_summary_nhood_age <- plyr::ddply(listings_unique_units_clean,c("source_id","neighborhood_01","muni","numRooms","periodblt"), summarise,
                                              rentcount = length(numRooms),
                                              meanrent = round(mean(ask),0),
                                              medrent = round(median(ask), 0))

listings_summary_ct_age <- plyr::ddply(listings_unique_units_clean,c("source_id","ct10_id","numRooms","periodblt"), summarise,
                                           rentcount = length(numRooms),
                                           meanrent = round(mean(ask),0),
                                           medrent = round(median(ask), 0))

listings_summary_zip_age <- plyr::ddply(listings_unique_units_clean,c("source_id","zip_code","numRooms","periodblt"), summarise,
                                            rentcount = length(numRooms),
                                            meanrent = round(mean(ask),0),
                                            medrent = round(median(ask), 0))

# Save CSV summaries for every municipality.
for (municipality in municipalities) {
  dir.create(here("data", "finished", municipality), showWarnings = FALSE)
  write.csv(filter(listings_summary_muni, muni == municipality),
            here("data", "finished", municipality, paste("listings_",municipality,"_summary_",opt$year,"-",opt$month,".csv", sep="")),
            row.names = FALSE)
  write.csv(filter(listings_summary_nhood, muni == municipality),
            here("data", "finished", municipality, paste("listings_",municipality,"_summary_nhood_",opt$year,"-",opt$month,".csv", sep="")),
            row.names = FALSE)
  write.csv(filter(listings_summary_muni_age, muni == municipality),
            here("data", "finished", municipality, paste("listings_",municipality,"_summary_age_",opt$year,"-",opt$month,".csv", sep="")),
            row.names = FALSE)
  write.csv(filter(listings_summary_nhood_age, muni == municipality),
            here("data", "finished", municipality,  paste("listings_",municipality,"_summary_nhood_age_",opt$year,"-",opt$month,".csv", sep="")),
            row.names = FALSE)
  write.csv(filter(listings_unique_units_clean_shp, muni == municipality),
            here("data", "finished", municipality, paste("listings_",municipality,"_unique_clean_full_units_",opt$year,"-",opt$month,".csv", sep="")),
            row.names = FALSE)
}

# Save Shapefile summaries for every municipality.
for (municipality in municipalities) {
  dir.create(here("data", "finished", municipality), showWarnings = FALSE)
  write_sf(filter(listings_unique_units_clean_shp, muni == municipality),
            here("data", "finished", municipality, paste("listings_",municipality,"_unique_units_",opt$year,"-",opt$month,".shp", sep="")))
}



###################################### STEP 8: WRITE TABLES ######################################
write.csv(listings_summary_counts, here("data", "finished", paste("listings_summary_counts_",opt$year,"-",opt$month,".csv", sep="")),row.names = FALSE)
write.csv(listings_unique_units, here("data", "finished", paste("listings_unique_full_units_",opt$year,"-",opt$month,".csv", sep="")),row.names = FALSE)
write.csv(listings_unique_units_clean, here("data", "finished", paste("listings_unique_clean_full_units_",opt$year,"-",opt$month,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_full,here("data", "finished", paste("listings_full_summary_",opt$year,"-",opt$month,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_muni, here("data", "finished", paste("listings_muni_summary_",opt$year,"-",opt$month,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_ct, here("data", "finished", paste("listings_ct_summary_",opt$year,"-",opt$month,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_nhood, here("data", "finished", paste("listings_nhood_summary_",opt$year,"-",opt$month,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_zip, here("data", "finished", paste("listings_zip_summary_",opt$year,"-",opt$month,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_full_age, here("data", "finished", paste("listings_full_summary_age_",opt$year,"-",opt$month,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_muni_age, here("data", "finished", paste("listings_muni_summary_age_",opt$year,"-",opt$month,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_ct_age, here("data", "finished", paste("listings_ct_summary_age_",opt$year,"-",opt$month,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_nhood_age, here("data", "finished", paste("listings_nhood_summary_age_",opt$year,"-",opt$month,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_zip_age, here("data", "finished", paste("listings_zip_summary_age_",opt$year,"-",opt$month,".csv", sep="")),row.names = FALSE)

# WRITE CODE TO AUTOMATICALLY WRITE ALL THESE FILES TO :
# K:\DataServices\Projects\Current_Projects\rental_listings_research\data\output\output_2021_03