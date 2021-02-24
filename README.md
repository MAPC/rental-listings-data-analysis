# Rental Listings Data Analysis
These are the scripts for doing analysis of the rental listings data that is exported from the [rental-listing-processor](https://github.com/MAPC/rental-listing-processor).

## Process
```sh
ssh rental-listing-processor@rental-listings.mapc.org
cd /opt/rental-listing-processor/current
vi .env.mapper
```

Update the configuration file with the current month (or quarter) and year.

```
MAPPER_YEAR=2021
MAPPER_MONTH=1
```

See `.env.mapper.template` for available options. You cannot set both a quarter and a month for a single run.

Then run the script

```sh
./process.py
```

It will take a while to run. After it finishes you should then download the output. An easy way to do this is using `scp`.


```sh
scp mzagaja@rental-listings.mapc.org:/opt/rental-listing-processor/current/files/rental-listings_1-2021.zip ./data/rental-listings_1-2021.zip
```


The quarterly processing script will need to be updated to reflect the new quarters and filenames until it can be refcactored. Once it is done you can run it with

```sh
RScript --vanilla processors/post_analysis_QUARTER_TEMPLATE.R
```

The monthly template has been abstracted to account for changes in month and year. You will need to specify the month using `-m` and year using `-y` in the command, and then provide the path to the processed_listings.csv file output from the rental-listing-processor.

```sh
RScript --vanilla processors/post_analysis_MONTHLY_TEMPLATE.R -m 1 -y 2020 data/rental-listings_1-2020/geolocator/1594219888.060255_processed_listings.csv
```

Finally you will likely want to make shapefiles for the individual municipalities. The [Makefile](https://github.com/MAPC/rental-listings-data-analysis/blob/master/Makefile) documents this process but still needs to be abstracted. Assuming you have already run the quarterly or monthly reporting script, you should have a directory `data/finished` that contains the output of the analysis script. The Makefile will then create shapefiles in each municipal directory.

Run the makefile with:

```
mkdir ARLINGTON/spatial CAMBRIDGE/spatial QUINCY/spatial BOSTON/spatial SOMERVILLE/spatial
make -f 2021-01.make
```
