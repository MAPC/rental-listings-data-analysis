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

install.packages("pacman", repos = "http://cran.us.r-project.org")
library(pacman)
pacman::p_load(plyr, dplyr, tidyverse, ggplot2, stringr, stringdist, ngram, sp, rgdal, raster, foreign, gtools, here, maptools, optparse)

today <- Sys.Date()
year<- format(today, "%Y")
month <- format(today, "%m")
day <- format(today, "%d")

municipalities <- c("BOSTON","CAMBRIDGE","QUINCY","SOMERVILLE","ARLINGTON")

option_list <- list(
  make_option(c("-m", "--month"), type="character", default=month,
              help="month", metavar="month"),
  make_option(c("-y", "--year"), type="character", default=year,
              help="year", metavar="year")
)

opt_parser <- OptionParser(usage = "%prog [options] file", option_list=option_list)
arguments <- parse_args(opt_parser, positional_arguments = 1)
opt <- arguments$options
file <- arguments$args


###################################### STEP 1: READ IN MOST UPDATED PARCEL DATA ######################################
load(file = here("data", "spatial", "parcels_consortium_abcd.RData"))
new_CRS <- "+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
CRS.new <- CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

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
WCS1984_CRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
event.Points <- SpatialPoints(data.frame(latitude = listings_unique$latitude, longitude = listings_unique$longitude),
                              proj4string = CRS(WCS1984_CRS))

#reproject the points to NAD83
event.Points <- spTransform(event.Points, CRS(new_CRS))
parcels_consortium_abcd <- spTransform(parcels_consortium_abcd, CRS(new_CRS))

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

#add the parcel attributes to the listings @@@
listings_unique_w_parcel <- cbind(listings_unique, parloc_id, luc_adj_1, luc_adj_2, yr_built)


###################################### STEP 4: LINK RENTAL LISTINGS DATA TO ZIP CODE ######################################

# check is the files exist in the directory of dsn throw
# error if not available with releavant message and exit the method
list.files(here("data", "spatial"), pattern='\\.shp$')

try(if (file.exists(paste0(here("data", "spatial"),'/zip_MA.shp')) == FALSE)
  stop("ZIP code file not available!"))

# read shape files of ZIP code boundaries
zip.shape <- readOGR(dsn=path.expand(here("data", "spatial")), layer ="zip_MA")

# project the shapefiles to NAD83
proj4string(zip.shape) <- CRS.new

zip.shape <- spTransform(zip.shape, CRS(new_CRS))

# overlay any of the boundaries with the listing records
pnt_zip.shape <- over(event.Points,zip.shape)

# add the parcel attributes to the listings @@@
listings_unique <- cbind(listings_unique_w_parcel, pnt_zip.shape$postcode, pnt_zip.shape$pc_name)


###################################### STEP 5: CLEAN UP YEAR BUILT VARIABLE BASED ON PARCEL DATA ######################################
listings_unique$yr_built[is.na(listings_unique$yr_built)]  <- 0
listings_unique$yr_built <- as.numeric(levels(listings_unique$yr_built))[listings_unique$yr_built]

#### CAMBRIDGE HOUSING STARTS PERMITS ####
parcels_cambridge = parcels_consortium_abcd[parcels_consortium_abcd$muni == "Cambridge",]
camb_housingstarts = read.csv(here("data", "partners", "Cambridge", "cambridge_housingstarts_updated2019.csv"))
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
bos_comp_older = read.csv(here("data", "partners", "Boston", "Completions_3-16-18_FOR MAPC_jointoParcels.csv"), fileEncoding="latin1")
bos_comp_update = read.csv(here("data", "partners", "Boston", "Boston Housing Completions 1.1.19 thru 9.27.19.csv"), fileEncoding="latin1")

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
                "latitude",
                "longitude",
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
listings_unique_units_clean <- listings_unique_units %>%
  filter(!(zip_code == '02108' & ask < 700) & numRooms != -1) %>% # also filters out all listings with numRooms == -1
  mutate(neighborhood_01 = ifelse(muni == "ARLINGTON", ct10_id,  neighborhood_01)) # use census tract 2010 ID for Arlington neighborhood variable since we don't have neighborhood names yet

listings_summary_counts <- ddply(listings_unique_units_clean,c("year", "month", "source_id"), summarise,
                                 rentcount = length(numRooms),
                                 medrent = round(median(ask), 0))

rm(camb_housingstarts, CRS.new, event.Points, listings_unique, listings_unique_mod,
   pnt_zip.shape, zip.shape)
gc() # garbage collection -- tells us how much space we have remaining in memory

###################################### STEP 7: MONTHLY SUMMARY STATISTICS ######################################

listings_summary_full <- ddply(listings_unique_units_clean,
                                         c("numRooms"), summarise,
                                         rentcount = length(numRooms),
                                         meanrent = round(mean(ask),0),
                                         medrent = round(median(ask), 0))

listings_summary_muni <- ddply(listings_unique_units_clean,c("muni","numRooms"), summarise,
                                         rentcount = length(numRooms),
                                         meanrent = round(mean(ask),0),
                                         medrent = round(median(ask), 0))


listings_summary_nhood <- ddply(listings_unique_units_clean,c("neighborhood_01","muni","numRooms"), summarise,
                                          rentcount = length(numRooms),
                                          meanrent = round(mean(ask),0),
                                          medrent = round(median(ask), 0))

listings_summary_ct <- ddply(listings_unique_units_clean,c("ct10_id","muni","numRooms"), summarise,
                                       rentcount = length(numRooms),
                                       meanrent = round(mean(ask),0),
                                       medrent = round(median(ask), 0))

listings_summary_zip <- ddply(listings_unique_units_clean,c("zip_code","muni","numRooms"), summarise,
                                        rentcount = length(numRooms),
                                        meanrent = round(mean(ask),0),
                                        medrent = round(median(ask), 0))

listings_summary_full_age <- ddply(listings_unique_units_clean,
                                             c("source_id","numRooms","periodblt"), summarise,
                                             rentcount = length(numRooms),
                                             meanrent = round(mean(ask),0),
                                             medrent = round(median(ask), 0))

listings_summary_muni_age <- ddply(listings_unique_units_clean,c("source_id","muni","numRooms","periodblt"), summarise,
                                             rentcount = length(numRooms),
                                             meanrent = round(mean(ask),0),
                                             medrent = round(median(ask), 0))

listings_summary_nhood_age <- ddply(listings_unique_units_clean,c("source_id","neighborhood_01","muni","numRooms","periodblt"), summarise,
                                              rentcount = length(numRooms),
                                              meanrent = round(mean(ask),0),
                                              medrent = round(median(ask), 0))

listings_summary_ct_age <- ddply(listings_unique_units_clean,c("source_id","ct10_id","numRooms","periodblt"), summarise,
                                           rentcount = length(numRooms),
                                           meanrent = round(mean(ask),0),
                                           medrent = round(median(ask), 0))

listings_summary_zip_age <- ddply(listings_unique_units_clean,c("source_id","zip_code","numRooms","periodblt"), summarise,
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
  write.csv(filter(listings_unique_units_clean, muni == municipality),
            here("data", "finished", municipality, paste("listings_",municipality,"_unique_clean_full_units_",opt$year,"-",opt$month,".csv", sep="")),
            row.names = FALSE)
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
