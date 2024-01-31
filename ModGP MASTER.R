#' ####################################################################### #
#' PROJECT: [BioDT CWR - ModGP] 
#' CONTENTS: 
#'  - Purpose of this code document
#'  DEPENDENCIES:
#'  - Code documents needed to execute this document
#'  - Data files
#' AUTHOR: [Erik Kusch]
#' ####################################################################### #

# PREAMBLE ================================================================
set.seed(42) # making things reproducibly random
rm(list=ls())
SPECIES <- "Lathyrus"

## Packages ---------------------------------------------------------------
install.load.package <- function(x) {
	if (!require(x, character.only = TRUE))
		install.packages(x, repos='http://cran.us.r-project.org')
	require(x, character.only = TRUE)
}
### CRAN PACKAGES ----
package_vec <- c(
	"iterators", # for icount
	"terra", # for alternative raster handling
	"rgbif", # for gbif access
	"sf", # for spatialfeatures
	"sp", # for spatialpoints and polygons
	"rnaturalearth", # for landmask
	"parallel", # for parallel runs
	"usdm", # for vifcor
	"raster", # for spatial object handling
	"sf", # for spatialfeatures
	"pbapply", # for parallelised apply functions and estimators
	"devtools", # for non-CRAN installation
	"remotes", # for non-CRAN installation
	"INLA", # for pointedSDMs
	# "intSDM", # for intSDMs
	"giscoR", # for shapefiles of Earth
	"Epi", # for ROC statistic
	"raster", # for raster objects
	"stars", # for fast raster operations
	"png", # for loading pngs back into R
	"grid", # for making pngs into ggplot object via rastergrob
	"ggplot2", # for plotting engine
	"ggpubr", # for t-test comparisons in ggplot
	"tidyr", # for gather()
	"viridis", # colour palettes
	"cowplot", # grid plotting
	"ggpmisc", # for table plotting in ggplot environment
	"gridExtra", # for smooth plot saving in PDF
	"stringr"
)
sapply(package_vec, install.load.package)

### NON-CRAN PACKAGES ----
if("KrigR" %in% rownames(installed.packages()) == FALSE){ # KrigR check
	Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
	remotes::install_github("https://github.com/cran/rgdal")
	devtools::install_github("ErikKusch/KrigR")
}
library(KrigR)

if("intSDM" %in% rownames(installed.packages()) == FALSE){
	devtools::install_github("PhilipMostert/PointedSDMs")
	devtools::install_github("PhilipMostert/intSDM")
}
library(intSDM)

## updating package_vec for handling of parallel environments
package_vec <- c(package_vec, "KrigR", "intSDM")

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
if(as.character(options("gbif_user")) == "NULL" ){
	options(gbif_user=rstudioapi::askForPassword("my gbif username"))}
if(as.character(options("gbif_email")) == "NULL" ){
	options(gbif_email=rstudioapi::askForPassword("my registred gbif e-mail"))}
if(as.character(options("gbif_pwd")) == "NULL" ){
	options(gbif_pwd=rstudioapi::askForPassword("my gbif password"))}

if(!exists("API_Key") | !exists("API_User")){ # CS API check: if CDS API credentials have not been specified elsewhere
	API_User <- readline(prompt = "Please enter your Climate Data Store API user number and hit ENTER.")
	API_Key <- readline(prompt = "Please enter your Climate Data Store API key number and hit ENTER.")
} # end of CDS API check
# NUMBER OF CORES
if(!exists("numberOfCores")){ # Core check: if number of cores for parallel processing has not been set yet
	numberOfCores <- as.numeric(readline(prompt = paste("How many cores do you want to allocate to these processes? Your machine has", parallel::detectCores())))
} # end of Core check

## Sourcing ---------------------------------------------------------------
source(file.path(Dir.Scripts, "SHARED-Data.R"))
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
PH_stack <- resample(PH_stack, BV_ras)
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

# ANALYSIS ================================================================
## SDM Pre-Selection ------------------------------------------------------
message("SDM species pre-selection")
SDMInput_ls <- FUN.PreSelect(
	Input_ls = SDMInput_ls,
	BV_ras = BV_ras,
	Occurrences = 40,
	Locations = 40,
	parallel = numberOfCores
)

## SDM Execution ----------------------------------------------------------
message("Executing SDM workflows")
SDMModel_ls <- FUN.ExecSDM(
	SDMData_ls = SDMInput_ls, 
	BV_ras = BV_ras, Dir = Dir.Exports.ModGP, Force = FALSE, KeepModels = TRUE,
	Drivers = PH_stack,
	parallel = numberOfCores)
