# Rental Listings Data Analysis
These are the scripts for doing analysis of the rental listings data that is exported from the [rental-listing-processor](https://github.com/MAPC/rental-listing-processor).

## Process
Typically you will drop the .zip file from the rental-listing-processor into the data directory and unzip it.

The quarterly processing script will need to be updated to reflect the new quarters and filenames until it can be refcactored. Once it is done you can run it with

```sh
RScript --vanilla processors/post_analysis_QUARTER_TEMPLATE.R
```

The monthly template has been abstracted to account for changes in month and year. You will need to specify the month using `-m` and year using `-y` in the command, and then provide the path to the processed_listings.csv file output from the rental-listing-processor.

```sh
RScript --vanilla processors/post_analysis_MONTHLY_TEMPLATE.R -m 1 -y 2020 data/rental-listings_1-2020/geolocator/1594219888.060255_processed_listings.csv
```

Finally you will likely want to make shapefiles for the individual municipalities. The [Makefile](https://github.com/MAPC/rental-listings-data-analysis/blob/master/Makefile) documents this process but still needs to be abstracted. Assuming you have already run the quarterly or monthly reporting script, you should have a directory `data/finished` that contains the output of the analysis script. The Makefile will then create shapefiles in each municipal directory.
