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

## Directories ------------------------------------------------------------
### Define directories in relation to project directory
Dir.Base <- getwd()
Dir.Scripts <- file.path(Dir.Base, "R Scripts")

source(file.path(Dir.Scripts, "ModGP-commonlines.R"))

## API Credentials --------------------------------------------------------
try(source(file.path(Dir.Scripts, "SHARED-APICredentials.R")))
if (!exists("API_User")) {
	API_User <- "none"
}
if(as.character(options("gbif_user")) == "NULL" ){
	options(gbif_user=rstudioapi::askForPassword("my gbif username"))}
if(as.character(options("gbif_email")) == "NULL" ){
	options(gbif_email=rstudioapi::askForPassword("my registred gbif e-mail"))}
if(as.character(options("gbif_pwd")) == "NULL" ){
	options(gbif_pwd=rstudioapi::askForPassword("my gbif password"))}

if(!exists("API_Key")){ # CDS API check: if CDS API credentials have not been specified elsewhere
	API_Key <- readline(prompt = "Please enter your Climate Data Store API key number and hit ENTER.")
} # end of CDS API check

# Choose the number of parallel processes
RUNNING_ON_LUMI <- FALSE

numberOfCores <- parallel::detectCores()


RUNNING_ON_DESTINE <- !is.na(strtoi(Sys.getenv("CWR_ON_DESTINE")))
if(RUNNING_ON_DESTINE){
	numberOfCores <- 9
}

# NUMBER OF CORES
if(!exists("numberOfCores")){ # Core check: if number of cores for parallel processing has not been set yet
	numberOfCores <- as.numeric(readline(prompt = paste("How many cores do you want to allocate to these processes? Your machine has", parallel::detectCores())))
} # end of Core check
message(sprintf("numberOfCores = %d", numberOfCores))

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

# ANALYSIS ================================================================
## SDM Execution ----------------------------------------------------------
message("Executing SDM workflows")
SDMModel_ls <- FUN.ExecSDM(
	SDMData_ls = SDMInput_ls, 
	BV_ras = BV_ras, 
	Dir = Dir.Exports.ModGP,
	Force = FALSE,
	Drivers = PH_stack,
	parallel = numberOfCores)
