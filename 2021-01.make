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

all: listings_ARLINGTON_2021_01.shp\
	   listings_BOSTON_2021_01.shp\
	   listings_CAMBRIDGE_2021_01.shp\
	   listings_QUINCY_2021_01.shp\
	   listings_SOMERVILLE_2021_01.shp

clean:
	rm -Rf data/finished/ARLINGTON/spatial/*
	rm -Rf data/finished/BOSTON/spatial/*
	rm -Rf data/finished/CAMBRIDGE/spatial/*
	rm -Rf data/finished/QUINCY/spatial/*
	rm -Rf data/finished/SOMERVILLE/spatial/*
	rm data/finished/ARLINGTON/listings_ARLINGTON_2021_01-cleaned.csv
	rm data/finished/BOSTON/listings_BOSTON_2021_01-cleaned.csv
	rm data/finished/CAMBRIDGE/listings_CAMBRIDGE_2021_01-cleaned.csv
	rm data/finished/QUINCY/listings_QUINCY_2021_01-cleaned.csv
	rm data/finished/SOMERVILLE/listings_SOMERVILLE_2021_01-cleaned.csv

listings_ARLINGTON_2021_01-cleaned.csv: listings_ARLINGTON_unique_clean_full_units_2021-1.csv
	sed -i -e '1s/\.//g' data/finished/ARLINGTON/$(notdir $<)
	in2csv -e iso-8859-1 -f csv data/finished/ARLINGTON/$(notdir $<) > data/finished/ARLINGTON/$(notdir $@)

listings_ARLINGTON_2021_01.shp: listings_ARLINGTON_2021_01-cleaned.csv
	mkdir -p data/finished/ARLINGTON/spatial
	ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:26986\
		-oo X_POSSIBLE_NAMES=longitude -oo Y_POSSIBLE_NAMES=latitude -oo AUTODETECT_TYPE=YES\
		-f "ESRI Shapefile" data/finished/ARLINGTON/spatial/$(notdir $@) data/finished/ARLINGTON/$(notdir $<)

listings_BOSTON_2021_01-cleaned.csv: listings_BOSTON_unique_clean_full_units_2021-1.csv
	sed -i -e '1s/\.//g' data/finished/BOSTON/$(notdir $<)
	in2csv -e iso-8859-1 -f csv data/finished/BOSTON/$(notdir $<) > data/finished/BOSTON/$(notdir $@)

listings_BOSTON_2021_01.shp: listings_BOSTON_2021_01-cleaned.csv
	mkdir -p data/finished/BOSTON/spatial
	ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:26986\
		-oo X_POSSIBLE_NAMES=longitude -oo Y_POSSIBLE_NAMES=latitude -oo AUTODETECT_TYPE=YES\
		-f "ESRI Shapefile" data/finished/BOSTON/spatial/$(notdir $@) data/finished/BOSTON/$(notdir $<)

listings_CAMBRIDGE_2021_01-cleaned.csv: listings_CAMBRIDGE_unique_clean_full_units_2021-1.csv
	sed -i -e '1s/\.//g' data/finished/CAMBRIDGE/$(notdir $<)
	in2csv -e iso-8859-1 -f csv data/finished/CAMBRIDGE/$(notdir $<) > data/finished/CAMBRIDGE/$(notdir $@)

listings_CAMBRIDGE_2021_01.shp: listings_CAMBRIDGE_2021_01-cleaned.csv
	mkdir -p data/finished/CAMBRIDGE/spatial
	ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:26986\
		-oo X_POSSIBLE_NAMES=longitude -oo Y_POSSIBLE_NAMES=latitude -oo AUTODETECT_TYPE=YES\
		-f "ESRI Shapefile" data/finished/CAMBRIDGE/spatial/$(notdir $@) data/finished/CAMBRIDGE/$(notdir $<)

listings_QUINCY_2021_01-cleaned.csv: listings_QUINCY_unique_clean_full_units_2021-1.csv
	sed -i -e '1s/\.//g' data/finished/QUINCY/$(notdir $<)
	in2csv -e iso-8859-1 -f csv data/finished/QUINCY/$(notdir $<) > data/finished/QUINCY/$(notdir $@)

listings_QUINCY_2021_01.shp: listings_QUINCY_2021_01-cleaned.csv
	mkdir -p data/finished/QUINCY/spatial
	ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:26986\
		-oo X_POSSIBLE_NAMES=longitude -oo Y_POSSIBLE_NAMES=latitude -oo AUTODETECT_TYPE=YES\
		-f "ESRI Shapefile" data/finished/QUINCY/spatial/$(notdir $@) data/finished/QUINCY/$(notdir $<)

listings_SOMERVILLE_2021_01-cleaned.csv: listings_SOMERVILLE_unique_clean_full_units_2021-1.csv
	sed -i -e '1s/\.//g' data/finished/SOMERVILLE/$(notdir $<)
	in2csv -e iso-8859-1 -f csv data/finished/SOMERVILLE/$(notdir $<) > data/finished/SOMERVILLE/$(notdir $@)

listings_SOMERVILLE_2021_01.shp: listings_SOMERVILLE_2021_01-cleaned.csv
	mkdir -p data/finished/SOMERVILLE/spatial
	ogr2ogr -s_srs EPSG:4326 -t_srs EPSG:26986\
		-oo X_POSSIBLE_NAMES=longitude -oo Y_POSSIBLE_NAMES=latitude -oo AUTODETECT_TYPE=YES\
		-f "ESRI Shapefile" data/finished/SOMERVILLE/spatial/$(notdir $@) data/finished/SOMERVILLE/$(notdir $<)
