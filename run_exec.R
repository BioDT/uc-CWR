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

## Packages ---------------------------------------------------------------
install.load.package <- function(x) {
	if (!require(x, character.only = TRUE))
		install.packages(x, repos='http://cran.us.r-project.org')
	require(x, character.only = TRUE)
}
### CRAN PACKAGES ----
package_vec <- c(
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

# Choose the number of parallel processes
RUNNING_ON_LUMI <- !is.na(strtoi(Sys.getenv("CWR_ON_LUMI")))
if (RUNNING_ON_LUMI) {
	numberOfCores <- 1
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