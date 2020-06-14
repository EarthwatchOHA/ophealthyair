# Operation Healthy Air

Operation Healthy Air deploys Particulate Matter Air Quality Sensors in communities across the globe to build community data literacy and environmental resiliency by engaging and empowering citizen scientists. This repository is the underlying codebase of the programs data pipeline and visualizations.

### Prerequisites

The Command Line Interfaces used by this package use the package 'argparse' and are thus dependent on a Python distribution being present in the user's PATH.

To use this scripts in this package, run the following R code once after installation.

```
devtools::install()

devtools::install_github("MazamaScience/AirSensor")
devtools::install_github("iozeroff/pavisualizR")

dir.create("outputs")
dir.create("outputs/graphics")
dir.create("outputs/graphics/cache")
dir.create("outputs/weekly-sensor-health-reports")
dir.create("outputs/data-viz-pkgs")

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

An R script is run from the terminal using the `Rscript` command, followed by the relative path to the file (`scripts/name_of_script.R `). 

For example: 

```
Rscript scripts/ingest_all.R
```

### Running the Scripts/Programs

All of the described scripts use Command Line Interfaces (CLI's).

CLI's are simple programs, which accept User Inputs.

There are two types of user inputs, ***Position Arguments***, and ***Optional Arguments***. 

Positional arguments (of which there is at most one for each program) are the first argument given to a program, and are required to run the program.

They are passed as such:

```
Rscript scripts/make-site-dataviz-pkg.R "IMD Lodhi Road"

```

Optional arguments are supplied after any positional arguments, and as you would think, are optional. All optional arguments come with a defaul value, specified in the Program's help page.

Optional arguments are passed to the program by appending the option flag (i.e. --catalog_path) after the script name, using either the shorthand or longhand argument tags (-c or --catalog_path). They come in three types, "bool", "str", and "int".

Bool arguments are TRUE or FALSE, they are marked as TRUE by simply adding the tag.

```
# These commands all do the same thing.
# Make data visualization packages for all sites and delete the unzipped file.
Rscript scripts/make-site-dataviz-pkg.R -d

Rscript scripts/make-site-dataviz-pkg.R --delete_uncompress
```

If the default is TRUE, specify FALSE by supplying FALSE after the flag.
```
# Don't delete the unzipped file (default is FALSE).
Rscript scripts/make-site-dataviz-pkg.R -d FALSE

```

Str arguments (short for string) are text arguments. They are passed by supplying quoted text after the flag.

```
# These commands do the same thing.
# Output all data visualization packages to Desktop.
Rscript scripts/make-site-dataviz-pkg-all.R -o "C:/Users/name/Desktop"

Rscript scripts/make-site-dataviz-pkg-all.R --output_dir "C:/Users/name/Desktop"
```

Int arguments (short for integer) are number arguments. They are passed, you guessed it, by supplying a number after the flag.

```
# These commands do the same thing.
# Ingest all sensors, look back 40 days for last sensor upload.
Rscript scripts/ingest_all.R "Example/Path/Sensor_Catalog.xlsx" -l 40

Rscript scripts/ingest_all.R "Example/Path/Sensor_Catalog.xlsx" --lookback_days 40

```
Multiple arguments are passed to a script by appending with a space.

```
# These commands do the same thing.
Rscript scripts/ingest_all.R --catalog_path "Example/Path/Sensor_Catalog.xlsx" --lookback_days 35

Rscript scripts/ingest_all.R -p "Example/Path/Sensor_Catalog.xlsx" -l 35

```

### HELP

All programs can be passed a --help (-h) argument to view the script options without running the script.

```
Rscript scripts/ingest_all.R --help
```
**It is recommended to run the help option before running a script.**


### ingest_all.R

Import and save Sensor Catalog from a given excel workbook and, if possible, loads data for all sensors with a Deploy Site that is not "Undeployed".

The Sensor Data ingested by this script is saved in the data folder, and is loaded by all the following scripts.
So, before running any of the below scripts, this script should be run. If I'm going to run any of the programs in a given day, I run ingest_all.R about an hour before I plan on running any other scripts. 

### make-site-dataviz-pkg.R

Create a single data visualization package for a given site. Site options taken from the Sensor Catalog, and can be seeen in by running --help. 

### make-site-dataviz-pkg-all.R

Runs make-site-dataviz-pkg.R for every site in the Sensor Catalog. All sites that will have data-visualization packages made can be found listed using --help.

AQI scale used for visualizations is determined by the program (as specified in the Sensor Catalog). Boston, Southern California, and Sri Lanka use the US AQI Scale. India sites use the Indian AQI scale.

Calibrations can be applied using calibration option. Currently, sites in the Boston program use the Von Hillern calibration. Sites in the India and Sri Lanka programs use the Lodhi Road calibration. No calibrations are applied to Southern California sites. For more flexibility in applying calibrations, make data visualization package using make-site-dataviz-pkg.R and selecting calibration option.

### weekly-sensor-health-report.R

Generate a sensor-health-report using the past week's data for a given Program.

### weekly-sensor-health-reports-all.R

Generate weekly sensor-health-reports for all programs (see --help for list).

### make-proxy-calibration.R

Create new proxy-model calibration from collocated site (based on Sensor Catalog).

### make-proxy-calibration-all.R

Create new proxy-models calibrations for all collocated sites (based on Sensor Catalog).

## License
GPL-3

## Authors

* Ian Ozeroff

## Acknowledgements
* Mazama Science
* OpenAir
* OpenAQ
