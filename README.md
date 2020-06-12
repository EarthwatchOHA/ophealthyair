# Operation Healthy Air

Operation Healthy Air deploys Particulate Matter Air Quality Sensors in communities across the globe to build community data literacy and environmental resiliency by engaging and empowering citizen scientists. This repository is the underlying codebase of the programs data pipeline and visualizations.

### Prerequisites

The Command Line Interfaces used by this package use the package 'argparse' and are thus dependent on a Python distribution being present in the user's PATH.

To use this scripts in this package, run the following R code once after installation.

```
dir.create("data/spatial")
MazamaSpatialUtils::setSpatialDataDir("data/spatial")
MazamaSpatialUtils::installSpatialData()

```


## Usage

This project is centered around several R scripts, all located in the scripts/ directory. 
The main scripts users will interact with are:

* ingest_all.R
* make-site-dataviz-pkg.R
* make-site-dataviz-pkg-all.R
* weekly-sensor-health-report.R
* weekly-sensor-health-report-all.R
* make-proxy-calibration.R
* make-proxy-calibration-all.R

These scripts should primarily be run from the Rstudio Terminal by opening the project (ophealthyair/ophealthyair.Rproj)

<img src="resources/imgs/open-proj.PNG">

And running the script from the Rstudio terminal.

<img src="resources/imgs/terminal-ex.PNG">

For more info on the Rstudio terminal see [here](https://support.rstudio.com/hc/en-us/articles/115010737148-Using-the-RStudio-Terminal).

An R script is run from the terminal using the Rscript command, followed by the relative path to the file (`scripts/name_of_script.R `). 

For example: 

```
Rscript scripts/ingest_all.R
```

All of the described scripts use Command Line Interfaces, which accept options, given by appending the option tags (i.e. --catalog_path) after the script name, using either the shorthand or longhand argument tags.

```
# These commands do the same thing.
# Long Hand Tag
Rscript scripts/ingest_all.R --catalog_path Example/Path/Sensor_Catalog.xlsx

# Short Hand Tag
Rscript scripts/ingest_all.R -p Example/Path/Sensor_Catalog.xlsx

```
Multiple arguments are passed to a script by appending with a space.

```
# These commands do the same thing.
Rscript scripts/ingest_all.R --catalog_path Example/Path/Sensor_Catalog.xlsx --lookback_days 35

Rscript scripts/ingest_all.R -p Example/Path/Sensor_Catalog.xlsx -l 35

```

All scripts can be passed a --help (-h) argument to view the script options without running the script.

```
Rscript scripts/ingest_all.R --help
```
**It is recommended to run the help option before running a script.**

### ingest_all.R

Import and save Sensor Catalog from a given excel workbook and, if possible, loads data for all sensors with a Deploy Site that is not "Undeployed".

The Sensor Data ingested by this script is saved as pat_list.RDS in the data folder, and is loaded by all the following scripts.
So, before running any of the below scripts, this script should be run. I typically run it at the beginning of the day (it can take upwards of an hour to complete) if I'm going to be running any other scripts. 

### make-site-dataviz-pkg.R

Create a single data visualization package for a given site. Site options taken from the Sensor Catalog, and can be seeen in by running --help. 

### make-site-dataviz-pkg-all.R

Runs make-site-dataviz-pkg.R for every site in the Sensor Catalog.

AQI scale used for visualizations is determined by the program (as specified in the Sensor Catalog). Boston, Southern California, and Sri Lanka use the US AQI Scale. India sites use the Indian AQI scale.

Calibrations can be applied using calibration option. Currently, sites in the Boston program use the Von Hillern calibration. Sites in the India and Sri Lanka programs use the Lodhi Road calibration. No calibrations are applied to Southern California sites. For more flexibility in applying calibrations, make data visualization package using make-site-dataviz-pkg.R and selecting calibration option.

### weekly-sensor-health-report.R




## To Do:
1. Make CLI for weekly-sensor-health-report.R
2. Schedule regular tasks to run in their own environment.
3. Work on TODO's (mostly error control systems).

## License
GPL-3

## Authors

* Ian Ozeroff

## Acknowledgements
* Mazama Science
* OpenAir
* OpenAQ
