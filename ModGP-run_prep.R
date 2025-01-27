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
Dir.Scripts <- file.path(Dir.Base, "R_scripts")

source(file.path(Dir.Scripts, "ModGP-commonlines.R"))

## API Credentials --------------------------------------------------------
try(source(file.path(Dir.Scripts, "SHARED-APICredentials.R")))
if (!exists("API_User")) {
	API_User <- "none@"
}

# Choose the number of parallel processes
RUNNING_ON_LUMI <- TRUE

numberOfCores <- strtoi(Sys.getenv("SLURM_CPUS_PER_TASK"))
if (is.na(numberOfCores)) {
	numberOfCores <- 1
}

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

# Extract the list of species names
species_names <- names(SDMInput_ls)

# Save the species names to a file
writeLines(species_names, "species_list.txt")

# Save the PH stack
FNAME <- file.path(Dir.Data.Envir, "PH_stack")
saveObj(PH_stack, file = FNAME)
