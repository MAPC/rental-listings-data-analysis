VPATH = data/finished data/finished/Arlington data/finished/Boston data/finished/Cambridge data/finished/Quincy data/finished/Somerville
DIR = $(CURDIR)
PROCESSOR_DIR = $(DIR)/processors
export

.PHONY: all clean

all:  listings_arlington_2019.shp listings_boston_2019.shp listings_cambridge_2019.shp listings_quincy_2019.shp listings_somerville_2019.shp

clean:
	rm -Rf data/finished/Arlington/spatial/*
	rm -Rf data/finished/Boston/spatial/*
	rm -Rf data/finished/Cambridge/spatial/*
	rm -Rf data/finished/Quincy/spatial/*
	rm -Rf data/finished/Somerville/spatial/*

listings_arlington_2019.shp: listings_arlington_unique_clean_full_units_2019_20200602.csv
	ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:4326\
		-oo X_POSSIBLE_NAMES=longitude_merge -oo Y_POSSIBLE_NAMES=latitude_merge\
		-f "ESRI Shapefile" data/finished/Arlington/spatial/$(notdir $@) data/finished/Arlington/$(notdir $<)

listings_boston_2019.shp: listings_boston_unique_clean_full_units_2019_20200602.csv
	ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:4326\
		-oo X_POSSIBLE_NAMES=longitude_merge -oo Y_POSSIBLE_NAMES=latitude_merge\
		-f "ESRI Shapefile" data/finished/Boston/spatial/$(notdir $@) data/finished/Boston/$(notdir $<)

listings_cambridge_2019.shp: listings_cambridge_unique_clean_full_units_2019_20200602.csv
	ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:4326\
		-oo X_POSSIBLE_NAMES=longitude_merge -oo Y_POSSIBLE_NAMES=latitude_merge\
		-f "ESRI Shapefile" data/finished/Cambridge/spatial/$(notdir $@) data/finished/Cambridge/$(notdir $<)

listings_quincy_2019.shp: listings_quincy_unique_clean_full_units_2019_20200602.csv
	ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:4326\
		-oo X_POSSIBLE_NAMES=longitude_merge -oo Y_POSSIBLE_NAMES=latitude_merge\
		-f "ESRI Shapefile" data/finished/Quincy/spatial/$(notdir $@) data/finished/Quincy/$(notdir $<)

listings_somerville_2019.shp: listings_somerville_unique_clean_full_units_2019_20200602.csv
	ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:4326\
		-oo X_POSSIBLE_NAMES=longitude_merge -oo Y_POSSIBLE_NAMES=latitude_merge\
		-f "ESRI Shapefile" data/finished/Somerville/spatial/$(notdir $@) data/finished/Somerville/$(notdir $<)
