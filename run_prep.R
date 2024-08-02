#' ####################################################################### #
#' PROJECT: [BioDT CWR - ModGP] 
#' CONTENTS: 
#'  - Execution of ModGP pipeline
#'  DEPENDENCIES:
#'  - R Scripts directory containing:
#'  	- "ModGP-Outputs.R"
#'  	- "ModGP-SDM.R"
#'  	- "SHARED-APICredentials.R" 
#'  	- "SHARED-Data.R" 
#' AUTHOR: [Erik Kusch]
#' ####################################################################### #

# PREAMBLE ================================================================
set.seed(42) # making things reproducibly random
rm(list=ls())

# Read species from command-line argument
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
	# Default species
	SPECIES <- "Lathyrus"
} else {
	SPECIES <- args[1]
}
message(sprintf("SPECIES = %s", SPECIES))

## Packages ---------------------------------------------------------------
install.load.package <- function(x) {
	if (!require(x, character.only = TRUE))
		install.packages(x, repos='http://cran.us.r-project.org')
	require(x, character.only = TRUE)
}
### CRAN PACKAGES ----
package_vec <- c(
#'  - Execution of ModGP pipeline
	'cowplot', # grid plotting
	'ggplot2', # ggplot machinery
	'ggpmisc', # table plotting in ggplot environment
	'ggpubr', # t-test comparison in ggplot
	'gridExtra', # ggplot saving in PDF
	'parallel', # parallel runs
	'pbapply', # parallel runs with estimator bar
	'raster', # spatial data
	'remotes', # remote installation
	'rgbif', # GBIF access
	'rnaturalearth', # shapefiles
	'sdm', # SDM machinery
	'sf', # spatial data
	'sp', # spatial data
	'terra', # spatial data
	'tidyr', # gather()
	'usdm', # vifcor()
	'viridis' # colour palette
)
sapply(package_vec, install.load.package)

### NON-CRAN PACKAGES ----
if("KrigR" %in% rownames(installed.packages()) == FALSE){ # KrigR check
	Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
	remotes::install_github("https://github.com/cran/rgdal")
	remotes::install_github("ErikKusch/KrigR")
}
library(KrigR)

if("mraster" %in% rownames(installed.packages()) == FALSE){ # KrigR check
	remotes::install_github("babaknaimi/mraster")
}
library(mraster)

if(!("maxent" %in% unlist(getmethodNames()))){sdm::installAll()} # install methods for sdm package

## updating package_vec for handling of parallel environments
package_vec <- c(package_vec, "KrigR", "mraster")

## Functionality ----------------------------------------------------------
`%nin%` <- Negate(`%in%`) # a function for negation of %in% function

#' Progress bar for data loading
saveObj <- function(object, file.name){
	outfile <- file(file.name, "wb")
	serialize(object, outfile)
	close(outfile)
}
loadObj <- function(file.name){
	library(foreach)
	filesize <- file.info(file.name)$size
	chunksize <- ceiling(filesize / 100)
	pb <- txtProgressBar(min = 0, max = 100, style=3)
	infile <- file(file.name, "rb")
	data <- foreach(it = icount(100), .combine = c) %do% {
		setTxtProgressBar(pb, it)
		readBin(infile, "raw", chunksize)
	}
	close(infile)
	close(pb)
	return(unserialize(data))
}

## Directories ------------------------------------------------------------
### Define directories in relation to project directory
Dir.Base <- getwd()
Dir.Scripts <- file.path(Dir.Base, "R Scripts")
Dir.Data <- file.path(Dir.Base, "Data")
Dir.Data.ModGP <- file.path(Dir.Data, "ModGP")
Dir.Data.GBIF <- file.path(Dir.Data, "GBIF")
Dir.Data.Envir <- file.path(Dir.Data, "Environment")
Dir.Exports <- file.path(Dir.Base, "Exports")
Dir.Exports.ModGP <- file.path(Dir.Exports, "ModGP")
### Create directories which aren't present yet
Dirs <- grep(ls(), pattern = "Dir.", value = TRUE)
CreateDir <- sapply(Dirs, function(x){
	x <- eval(parse(text=x))
	if(!dir.exists(x)) dir.create(x)})
rm(Dirs)

## API Credentials --------------------------------------------------------
try(source(file.path(Dir.Scripts, "SHARED-APICredentials.R")))

# Choose the number of parallel processes
RUNNING_ON_LUMI <- !is.na(strtoi(Sys.getenv("CWR_ON_LUMI")))
if (RUNNING_ON_LUMI) {
	numberOfCores <- strtoi(Sys.getenv("SLURM_NTASKS"))
	if (is.na(numberOfCores)) {
		numberOfCores <- 1
	}
} else {
	numberOfCores <- parallel::detectCores()
}

# NUMBER OF CORES
if(!exists("numberOfCores")){ # Core check: if number of cores for parallel processing has not been set yet
	numberOfCores <- as.numeric(readline(prompt = paste("How many cores do you want to allocate to these processes? Your machine has", parallel::detectCores())))
} # end of Core check
message(sprintf("numberOfCores = %d", numberOfCores))

## Sourcing ---------------------------------------------------------------
source(file.path(Dir.Scripts,"SHARED-Data.R"))
source(file.path(Dir.Scripts,"ModGP-SDM.R"))
source(file.path(Dir.Scripts,"ModGP-Outputs.R"))

# DATA ====================================================================
## GBIF Data --------------------------------------------------------------
message("Retrieving GBIF data")
## species of interest
Species_ls <- FUN.DownGBIF(
	species = SPECIES, # which species to pull data for
	Dir = Dir.Data.GBIF, # where to store the data output on disk
	Force = FALSE, # do not overwrite already present data
	Mode = "ModGP", # query download for entire genus
	parallel = 1 # no speed gain here for parallelising on personal machine
	)

## Environmental Data -----------------------------------------------------
message("Retrieving environmental data")
BV_ras <- FUN.DownBV(T_Start = 1985, # what year to begin climatology calculation in
										 T_End = 2015, # what year to end climatology calculation in
										 Dir = Dir.Data.Envir, # where to store the data output on disk
										 Force = FALSE # do not overwrite already present data
										 )

## Posthoc Data -----------------------------------------------------------
message("Retrieving additional covariates")
#' For relating SDM outputs to other characteristics of interest to users
PH_nutrient <- raster("https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq1.asc")
PH_toxicity <- raster("https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq6.asc")
PH_stack <- stack(PH_nutrient, PH_toxicity)
PH_stack <- raster::resample(PH_stack, BV_ras[[1]])
PH_stack <- stack(PH_stack, BV_ras$BIO1, BV_ras$BIO12)
names(PH_stack) <- c("Nutrient", "Toxicity", "Temperature", "Soil Moisture")

## SDM Data Preparations --------------------------------------------------
message("Preparing data for SDM workflow")
SDMInput_ls <- FUN.PrepSDMData(occ_ls = Species_ls$occs, # list of occurrence data frames per species
															 BV_ras = BV_ras, # bioclimatic rasterstack
															 Dir = Dir.Data.ModGP, # where to store the data output on disk
															 Force = FALSE, # # do not overwrite already present data
															 parallel = numberOfCores # parallelised execution
															 )

# Extract the list of species names
species_names <- names(SDMInput_ls)

# Save the species names to a file
writeLines(species_names, "species_list.txt")


# Save the PH stack
FNAME <- file.path(Dir.Data.Envir, "PH_stack")
saveObj(PH_stack, file = FNAME)
