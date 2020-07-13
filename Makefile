VPATH = data/finished\
				data/finished/ARLINGTON\
				data/finished/BOSTON\
				data/finished/CAMBRIDGE\
				data/finished/QUINCY\
				data/finished/SOMERVILLE
DIR = $(CURDIR)
PROCESSOR_DIR = $(DIR)/processors
export

.PHONY: all clean

all: listings_ARLINGTON_2020_Q2.shp\
	   listings_BOSTON_2020_Q2.shp\
	   listings_CAMBRIDGE_2020_Q2.shp\
	   listings_QUINCY_2020_Q2.shp\
	   listings_SOMERVILLE_2020_Q2.shp

clean:
	rm -Rf data/finished/ARLINGTON/spatial/*
	rm -Rf data/finished/BOSTON/spatial/*
	rm -Rf data/finished/CAMBRIDGE/spatial/*
	rm -Rf data/finished/QUINCY/spatial/*
	rm -Rf data/finished/SOMERVILLE/spatial/*

listings_ARLINGTON_2020_Q2.shp: $(wildcard listings_ARLINGTON_unique_clean_full_units_2020Q2_*.csv)
	mkdir -p data/finished/ARLINGTON/spatial
	ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:4326\
		-oo X_POSSIBLE_NAMES=longitude_merge -oo Y_POSSIBLE_NAMES=latitude_merge\
		-f "ESRI Shapefile" data/finished/ARLINGTON/spatial/$(notdir $@) data/finished/ARLINGTON/$(notdir $<)

listings_BOSTON_2020_Q2.shp: $(wildcard listings_BOSTON_unique_clean_full_units_2020Q2_*.csv)
	mkdir -p data/finished/BOSTON/spatial
	ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:4326\
		-oo X_POSSIBLE_NAMES=longitude_merge -oo Y_POSSIBLE_NAMES=latitude_merge\
		-f "ESRI Shapefile" data/finished/BOSTON/spatial/$(notdir $@) data/finished/BOSTON/$(notdir $<)

listings_CAMBRIDGE_2020_Q2.shp: $(wildcard listings_CAMBRIDGE_unique_clean_full_units_2020Q2_*.csv)
	mkdir -p data/finished/CAMBRIDGE/spatial
	ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:4326\
		-oo X_POSSIBLE_NAMES=longitude_merge -oo Y_POSSIBLE_NAMES=latitude_merge\
		-f "ESRI Shapefile" data/finished/CAMBRIDGE/spatial/$(notdir $@) data/finished/CAMBRIDGE/$(notdir $<)

listings_QUINCY_2020_Q2.shp: $(wildcard listings_QUINCY_unique_clean_full_units_2020Q2_*.csv)
	mkdir -p data/finished/QUINCY/spatial
	ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:4326\
		-oo X_POSSIBLE_NAMES=longitude_merge -oo Y_POSSIBLE_NAMES=latitude_merge\
		-f "ESRI Shapefile" data/finished/QUINCY/spatial/$(notdir $@) data/finished/QUINCY/$(notdir $<)

listings_SOMERVILLE_2020_Q2.shp: $(wildcard listings_SOMERVILLE_unique_clean_full_units_2020Q2_*.csv)
	mkdir -p data/finished/SOMERVILLE/spatial
	ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:4326\
		-oo X_POSSIBLE_NAMES=longitude_merge -oo Y_POSSIBLE_NAMES=latitude_merge\
		-f "ESRI Shapefile" data/finished/SOMERVILLE/spatial/$(notdir $@) data/finished/SOMERVILLE/$(notdir $<)
