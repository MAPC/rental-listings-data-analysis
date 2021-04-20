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

quarter <- paste0("Q",ifelse(month == "04", "1", ifelse(month == "07", "2", ifelse(month == "10", "3", ifelse(month == "01", "4")))))

municipalities <- c("BOSTON","CAMBRIDGE","QUINCY","SOMERVILLE","ARLINGTON")

file <- "K:/DataServices/Projects/Current_Projects/rental_listings_research/r_scripts/analysis/rental-listings-data-analysis/data/rental-listings_Q1-2021/rental-listings_Q1-2021/cleaner/1618519213.96647_listings_unique.csv"

###################################### STEP 1: READ IN MOST UPDATED PARCEL DATA ######################################
# Loading the parcels database file as an .RData file to reduce file size
load(file = here("data", "spatial", "parcels_consortium_abcd.RData"))

parcels_consortium_abcd <- 
  parcels_consortium_abcd %>% 
  # convert non simple features object to simple features object
  st_as_sf() %>% 
  # manually specify identified non-standard crs
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
  
  # transform non-standard crs to MA state plane (epsg code: 26986)
  st_transform(.,
               st_crs(26986))

###################################### STEP 2: READ IN RENTAL LISTINGS DATA ######################################
listings_unique <- read_csv(file)  %>%
  mutate(eight_bedroom = ifelse(numRooms == 8, eight_bedroom == 1, 0)) %>%
  dplyr::select(-1) # remove X variable (the first column) because it becomes redundant after combining the four dataframes

#categorize Quarter
colnames(listings_unique)[which(names(listings_unique) == "Year")] <- "year"
colnames(listings_unique)[which(names(listings_unique) == "Month")] <- "month"

### LAG IN POSTED TO UPDATED TIME ###
# # calculate updated lag
listings_unique$update_lag <- as.Date(listings_unique$updated_date) - as.Date(listings_unique$post_date)

###################################### STEP 3: LINK RENTAL LISTINGS DATA TO PARCEL ID ######################################
# read lat long attributes of the listing records and create event points of WGS1984 geographic projection

listings_unique_w_parcel <- 
  
  #read lat long attributes of the listing records and assign WGS84 projection (epsg code: 4326)
  st_join(listings_unique %>% 
            as_tibble() %>% 
            st_as_sf(coords = c("longitude","latitude"),
                     crs = 4326) %>% 
            # transform listing records projection to MA state plane (epsg code: 26986)
            st_transform(.,
                         crs = 26986),
          #join the points and parcels data to get spatial 
          parcels_consortium_abcd %>%
            dplyr::select(luc_adj_2, luc_adj_1, yr_built, parloc_id),
          left = TRUE)

###################################### STEP 4: LINK RENTAL LISTINGS DATA TO ZIP CODE ######################################

listings_unique <- 
  st_join(
    
    # join listings with unique parcel listings to zipcodes
    listings_unique_w_parcel,
    
    #read shape files of ZIP code boundaries
    read_sf("data/spatial/zip_MA.shp") %>%
      # select only zipcode and associated post office name
      dplyr::select(postcode, pc_name),
    # left_join
    left = TRUE) %>% 
  
  ###################################### STEP 5: CLEAN UP YEAR BUILT VARIABLE BASED ON PARCEL DATA ######################################

mutate(yr_built = as.numeric(yr_built),
       yr_built = replace_na(yr_built, 0))

remove(listings_unique_w_parcel)
#### CAMBRIDGE HOUSING STARTS PERMITS ####

listings_unique_w_camb <- 
  left_join(
    listings_unique,
    # Get unique listings with parcels for Cambridge
    st_join(
      # read in Cambridge Housing Starts and clean latitude/longitude data
      read_csv("data/partners/Cambridge/cambridge_housingstarts_updated2019.csv") %>% 
        separate(Location, c("latitude", "longitude"), sep = ",") %>% 
        mutate(longitude = as.numeric(gsub("\\)", "", longitude)),
               latitude = as.numeric(gsub("\\(", "", latitude))) %>% 
        
        # convert Cambridge Housing Starts to sf object and assign WGS84 crs (epsd code: 4326)
        st_as_sf(.,
                 coords = c("longitude", "latitude"),
                 crs = 4326) %>% 
        st_transform(.,
                     crs = 26986),
      
      # read in filtered parcel data for cambridge 
      parcels_consortium_abcd %>% 
        filter(muni == "Cambridge")) %>% 
      
      # add 'Cambridge' and 'MA' identifiers for cambridge unique listings:
      mutate(Muni = "Cambridge", 
             State = "MA") %>%
      
      # rename and simplify variable names
      rename(camb_yr_built = `Year Permitted`) %>% 
      
      # there are many listings for some parcels -- either they've had several housing starts in the same year on the same parcel, or housing starts in different years
      # here, we filter on the most recent housing start for each parcel, and if multiple housing starts happened in the same year, we filter on the Order
      # Order = Unique identifier that orders cases by year, by fiscal year (if applicable), by street number, and by house number
      # We select the highest order for a given parcel and remove duplicates.
      # Each parloc_id is associated with the most recent year housing starts occurred on that parcel.
      group_by(parloc_id) %>%
      filter(camb_yr_built == (max(camb_yr_built)),
             Order == max(Order)) %>% 
      ungroup() %>% 
      
      dplyr::select(parloc_id, camb_yr_built) %>% 
      
      # drop geometry component
      st_drop_geometry(),
    by = "parloc_id",
    na_matches = 'never') %>% # require NAs and NaN to NOT be matched -- if you match NAs, then a parloc_id NA from Cambridge with a given yr_built value can be assigned to any other municipality's parloc_NA, giving their yr_built value a camb_yr_built value! We don't want that!
  mutate(camb_yr_built = replace_na(camb_yr_built, 0))

# transfer the 'year built' fields from Cambridge's data to their equivalent column in the main column
listings_unique_w_camb$yr_built[listings_unique_w_camb$camb_yr_built >0 ] = listings_unique_w_camb$camb_yr_built[listings_unique_w_camb$camb_yr_built >0 ]

# BOSTON HOUSING COMPLETIONS ----------------------------------------------

# join Unique Boston listings with parcels to parent file
listings_unique <- left_join(
  
  # parent file with Cambridge additions
  listings_unique_w_camb,
  
  # Get unique listings with parcels for Boston
  read_csv("data/partners/Boston/Completions_3-16-18_FOR MAPC_jointoParcels.csv",
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
    # Clean names to lower case and underscore (_) separation
    janitor::clean_names() %>%
    mutate(bos_yr_built = year(mdy(complete_date))) %>%
    dplyr::select(parloc_id, bos_yr_built),
  
  # join the unique Boston listings with the parent file (which incl Cambridge data) by parloc_id, and never join if there are NA matches
  by = "parloc_id",
  na_matches = 'never') %>% 
  mutate(bos_yr_built = replace_na(bos_yr_built, 0))

remove(listings_unique_w_camb)
# transfer the 'year built' fields from Boston's data to their equivalent column in the main column
listings_unique$yr_built[listings_unique$bos_yr_built >0 ] = listings_unique$bos_yr_built[listings_unique$bos_yr_built >0 ]

#### YEAR BUILT CATEGORIZATION ####
listings_unique <- 
  listings_unique %>% 
  mutate(periodblt = case_when(yr_built >= 2011 ~ "b_2011 or later",
                               yr_built < 2011 ~ "a_Before 2011",
                               yr_built == 0 ~ "d_NA")) %>% 
  
  
  ###################################### STEP 6: FINAL DATASET WRANGLING BEFORE ANALYSIS ######################################
### rooms for rent ###
#create keyWord list for studio and 1-10 bedrooms by calling com function
mutate(roomrent = str_detect(title,
                             " ROOM RENT | ROOMMATE | ROOMIE | ONE ROOM IN TWO | ONE BEDROOM IN TWO | ONE ROOM IN THREE | ONE BEDROOM IN THREE | ONE ROOM IN FOUR | ONE BEDROOM IN FOUR | ONE ROOM IN FIVE | ONE BEDROOM IN FIVE | ONE ROOM IN SIX | ONE BEDROOM IN SIX | ONE ROOM IN SEVEN | ONE BEDROOM IN SEVEN | ONE ROOM IN EIGHT | ONE BEDROOM IN EIGHT | ONE ROOM IN NINE | ONE BEDROOM IN NINE | ONE ROOM IN HOUSE | ONE BEDROOM IN HOUSE | ONE ROOM IN CONDO | ONE BEDROOM IN CONDO | ONE ROOM IN APARTMENT | ONE BEDROOM IN APARTMENT | ROOM AVAILABLE IN TWO | BEDROOM AVAILABLE IN TWO | ROOM AVAILABLE IN THREE | BEDROOM AVAILABLE IN THREE | ROOM AVAILABLE IN FOUR | BEDROOM AVAILABLE IN FOUR | ROOM AVAILABLE IN FIVE | BEDROOM AVAILABLE IN FIVE | ROOM AVAILABLE IN SIX | BEDROOM AVAILABLE IN SIX | ROOM AVAILABLE IN SEVEN | BEDROOM AVAILABLE IN SEVEN | ROOM AVAILABLE IN EIGHT | BEDROOM AVAILABLE IN EIGHT | ROOM AVAILABLE IN NINE | BEDROOM AVAILABLE IN NINE | ROOM AVAILABLE IN HOUSE | BEDROOM AVAILABLE IN HOUSE | ROOM AVAILABLE IN APARTMENT | BEDROOM AVAILABLE IN APARTMENT | ROOM AVAILABLE IN CONDO | BEDROOM AVAILABLE IN CONDO "),
       sublet = str_detect(title,
                           " SUBLET "),
       shortterm = str_detect(title,
                              " SHORT TERM "),
       shared = str_detect(title,
                           " SHARED | SHARE "))

# Reorder by column name
listings_unique_units <- 
  listings_unique %>%
  rename(
    zip_code = postcode, 
    zip_muni = pc_name,
    studio_not_in_range = `studio _not_in_range`,
    one_bedroom_not_in_range = `one_bedroom _not_in_range`,
    two_bedroom_not_in_range = `two_bedroom _not_in_range`,
    three_bedroom_not_in_range = `three_bedroom _not_in_range`,
    four_bedroom_not_in_range = `four_bedroom _not_in_range`
    ) %>% 
  dplyr::select(id,
                ask,
                bedrooms,
                numRooms,
                title,
                original_title,
                source_id,
                survey_id,
                post_date,
                created_date,
                updated_date,
                update_lag,
                month,
                year,
                ct10_id,
                muni_ID,
                muni,
                zip_code,
                zip_muni,
                comm_type,
                neighborhood_01,
                neighborhood_02,
                neighborhood_03,
                studio,
                studio_not_in_range,
                one_bedroom,
                one_bedroom_not_in_range,
                two_bedroom,
                two_bedroom_not_in_range,
                three_bedroom,
                three_bedroom_not_in_range,
                four_bedroom,
                four_bedroom_not_in_range,
                periodblt,
                roomrent,
                sublet,
                shortterm,
                shared,
                post_at,
                created_at,
                updated_at,
                uniqueid,
                parloc_id,
                luc_adj_1,
                luc_adj_2,
                yr_built,
                camb_yr_built,
                bos_yr_built) %>%
  mutate(zip_code = as.character(zip_code)) %>% 
  filter(roomrent == FALSE &
           sublet == FALSE &
           shortterm == FALSE &
           shared == FALSE &
           update_lag<=30)

remove(listings_unique)

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

gc() # garbage collection -- tells us how much space we have remaining in memory

###################################### STEP 7: QUARTERLY SUMMARY STATISTICS ######################################
# Overall Listings summary
listings_summary_full_quarterly <-
  listings_unique_units_clean %>% 
  group_by(numRooms) %>% 
  summarise(rentcount = length(numRooms),
            meanrent = round(mean(ask),0),
            medrent = round(median(ask), 0))

# Listings summary by municipality
listings_summary_muni_quarterly <- 
  listings_unique_units_clean %>% 
  group_by(muni, numRooms) %>% 
  summarise(rentcount = length(numRooms),
            meanrent = round(mean(ask),0),
            medrent = round(median(ask), 0))

# Listings summary by neighborhood
listings_summary_nhood_quarterly <- 
  listings_unique_units_clean %>% 
  group_by(neighborhood_01, muni, numRooms) %>% 
  summarise(rentcount = length(numRooms),
            meanrent = round(mean(ask),0),
            medrent = round(median(ask), 0))

# Listings summary by Census Tract
listings_summary_ct_quarterly <- 
  listings_unique_units_clean %>% 
  group_by(ct10_id, muni, numRooms) %>% 
  summarise(rentcount = length(numRooms),
            meanrent = round(mean(ask),0),
            medrent = round(median(ask), 0))

# Listings summary by zip code
listings_summary_zip_quarterly <-
  listings_unique_units_clean %>% 
  group_by(zip_code, muni, numRooms) %>% 
  summarise(rentcount = length(numRooms),
            meanrent = round(mean(ask),0),
            medrent = round(median(ask), 0))

# Overall Listings summary with age of building
listings_summary_full_quarterly_age <-
  listings_unique_units_clean %>% 
  group_by(source_id, numRooms, periodblt) %>% 
  summarise(rentcount = length(numRooms),
            meanrent = round(mean(ask),0),
            medrent = round(median(ask), 0))

# Listings summary by municipality with age of building
listings_summary_muni_quarterly_age <-
  listings_unique_units_clean %>% 
  group_by(source_id, muni, numRooms, periodblt) %>% 
  summarise(rentcount = length(numRooms),
            meanrent = round(mean(ask),0),
            medrent = round(median(ask), 0))

# Listings summary by neighborhood with age of building
listings_summary_nhood_quarterly_age <- 
  listings_unique_units_clean %>% 
  group_by(source_id, neighborhood_01, muni, numRooms, periodblt) %>% 
  summarise(rentcount = length(numRooms),
            meanrent = round(mean(ask),0),
            medrent = round(median(ask), 0))

# Listings summary by Census Tract with age of building
listings_summary_ct_quarterly_age <-
  listings_unique_units_clean %>% 
  group_by(source_id, ct10_id, numRooms, periodblt) %>% 
  summarise(rentcount = length(numRooms),
            meanrent = round(mean(ask),0),
            medrent = round(median(ask), 0))

# Listings summary by zip code with age of building
listings_summary_zip_quarterly_age <-
  listings_unique_units_clean %>% 
  group_by(source_id, zip_code, numRooms, periodblt) %>% 
  summarise(rentcount = length(numRooms),
            meanrent = round(mean(ask),0),
            medrent = round(median(ask), 0))

# Save CSV summaries for every municipality.
for (municipality in municipalities) {
  dir.create(here("data", "finished", municipality), showWarnings = FALSE)
  write.csv(filter(listings_summary_muni_quarterly, muni == municipality),
            here("data", "finished", municipality, paste("listings_",municipality,"_summary_quarterly_",year,quarter,"_",year,month,day,".csv", sep="")),
            row.names = FALSE)
  write.csv(filter(listings_summary_nhood_quarterly, muni == municipality),
            here("data", "finished", municipality, paste("listings_",municipality,"_summary_nhood_quarterly_",year,quarter,"_",year,month,day,".csv", sep="")),
            row.names = FALSE)
  write.csv(filter(listings_summary_muni_quarterly_age, muni == municipality),
            here("data", "finished", municipality, paste("listings_",municipality,"_summary_quarterly_age_",year,quarter,"_",year,month,day,".csv", sep="")),
            row.names = FALSE)
  write.csv(filter(listings_summary_nhood_quarterly_age, muni == municipality),
            here("data", "finished", municipality,  paste("listings_",municipality,"_summary_nhood_quarterly_age_",year,quarter,"_",year,month,day,".csv", sep="")),
            row.names = FALSE)
  write.csv(filter(listings_unique_units_clean, muni == municipality),
            here("data", "finished", municipality, paste("listings_",municipality,"_unique_clean_full_units_",year,quarter,"_",year,month,day,".csv", sep="")),
            row.names = FALSE)
}

# Save Shapefile summaries for every municipality.
for (municipality in municipalities) {
  dir.create(here("data", "finished", municipality), showWarnings = FALSE)
  write_sf(filter(listings_unique_units_clean_shp, muni == municipality),
           here("data", "finished", municipality, paste("listings_",municipality,"_unique_clean_full_units_",year,quarter,"_",year,month,day,".shp", sep="")))
}

###################################### STEP 8: WRITE TABLES (MAKE SURE YOU CHANGE THE YEAR AND QUARTER IN THE FILE NAMES BELOW) ######################################
write.csv(listings_summary_counts, here("data", "finished", paste("listings_summary_counts_",year,quarter,"_",year,month,day,".csv", sep="")),row.names = FALSE)
write.csv(listings_unique_units, here("data", "finished", paste("listings_unique_full_units_",year,quarter,"_",year,month,day,".csv", sep="")),row.names = FALSE)
write.csv(listings_unique_units_clean, here("data", "finished", paste("listings_unique_clean_full_units_",year,quarter,"_",year,month,day,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_full_quarterly,here("data", "finished", paste("listings_full_summary_quarterly_",year,quarter,"_",year,month,day,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_muni_quarterly, here("data", "finished", paste("listings_muni_summary_quarterly_",year,quarter,"_",year,month,day,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_ct_quarterly, here("data", "finished", paste("listings_ct_summary_quarterly_",year,quarter,"_",year,month,day,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_nhood_quarterly, here("data", "finished", paste("listings_nhood_summary_quarterly_",year,quarter,"_",year,month,day,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_zip_quarterly, here("data", "finished", paste("listings_zip_summary_quarterly_",year,quarter,"_",year,month,day,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_full_quarterly_age, here("data", "finished", paste("listings_full_summary_quarterly_age_",year,quarter,"_",year,month,day,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_muni_quarterly_age, here("data", "finished", paste("listings_muni_summary_quarterly_age_",year,quarter,"_",year,month,day,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_ct_quarterly_age, here("data", "finished", paste("listings_ct_summary_quarterly_age_",year,quarter,"_",year,month,day,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_nhood_quarterly_age, here("data", "finished", paste("listings_nhood_summary_quarterly_age_",year,quarter,"_",year,month,day,".csv", sep="")),row.names = FALSE)
write.csv(listings_summary_zip_quarterly_age, here("data", "finished", paste("listings_zip_summary_quarterly_age_",year,quarter,"_",year,month,day,".csv", sep="")),row.names = FALSE)

###################################### STEP 9: WRITE TABLES ######################################
# WRITE CODE TO AUTOMATICALLY WRITE ALL THESE FILES TO :
# K:\DataServices\Projects\Current_Projects\rental_listings_research\data\output