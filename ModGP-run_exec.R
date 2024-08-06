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
	stop("No species specified")
} else {
	SPECIES <- args[1]
}
message(sprintf("SPECIES = %s", SPECIES))


## Directories ------------------------------------------------------------
### Define directories in relation to project directory
Dir.Base <- getwd()
Dir.Scripts <- file.path(Dir.Base, "R Scripts")

source(file.path(Dir.Scripts, "ModGP-commonlines.R"))

# Choose the number of parallel processes
RUNNING_ON_LUMI <- TRUE

numberOfCores <- 1

message(sprintf("numberOfCores = %d", numberOfCores))

# Load the prepped data
FNAME <- file.path(Dir.Data.ModGP, paste0(strsplit(SPECIES, split = " ")[[1]][1], "_SDMData.RData"))
SDMInput_ls <- loadObj(FNAME)

# Load the bioclimatic rasterstack
FNAME <- file.path(Dir.Data.Envir, paste0("BV_", 1985, "-", 2015, ".nc"))
BV_ras <- stack(FNAME)
names(BV_ras) <- paste0("BIO", 1:19)

# Load the PH stack
FNAME <- file.path(Dir.Data.Envir, "PH_stack")
PH_stack <- loadObj(FNAME)

# Check if the species is present in the list
if (!(SPECIES %in% names(SDMInput_ls))) {
  stop(sprintf("No occurrence data found for species: %s", SPECIES))
}

# Extract the correct subspecies
occ_ls <- setNames(list(SDMInput_ls[[SPECIES]]), SPECIES)

print(SPECIES)

# Run the exec function for the subspecies
message("Executing SDM workflows in parallel")
SDMModel_ls <- FUN.ExecSDM(
	SDMData_ls = occ_ls, 
	BV_ras = BV_ras, 
	Dir = Dir.Exports.ModGP,
	Force = TRUE,
	Drivers = PH_stack,
	parallel = numberOfCores
)