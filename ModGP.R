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
rm(list=ls()) # clearing the work environment
set.seed(42) # making things reproducibly random

## Directories ------------------------------------------------------------
### Define dicrectories in relation to project directory
Dir.Base <- getwd()
Dir.Data <- file.path(Dir.Base, "Data")
Dir.Exports <- file.path(Dir.Base, "Exports")
Dir.Data.GBIF <- file.path(Dir.Data, "GBIF")
Dir.Data.Envir <- file.path(Dir.Data, "Environment")
### Create directories which aren't present yet
Dirs <- grep(ls(), pattern = "Dir.", value = TRUE)
CreateDir <- sapply(Dirs, function(x){
	x <- eval(parse(text=x))
	if(!dir.exists(x)) dir.create(x)})

## Packages ---------------------------------------------------------------
### CRAN PACKAGES ----
install.load.package <- function(x) {
	if (!require(x, character.only = TRUE))
		install.packages(x, repos='http://cran.us.r-project.org')
	require(x, character.only = TRUE)
}
package_vec <- c(
	"iterators" # for icount
)
sapply(package_vec, install.load.package)

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

## Sourcing ---------------------------------------------------------------
source("X - Functions-Data.R")
source("X - Functions-SDM.R")
source("X - Functions-Outputs.R")

# DATA ====================================================================
## GBIF Data --------------------------------------------------------------
message("Retrieving GBIF data")
## species of interest
Species_ls <- FUN.DownGBIF(
	species = "Lathyrus sativus",
	Dir = Dir.Data.GBIF,
	Force = FALSE)

## Environmental Data -----------------------------------------------------
message("Retrieving environmental data")
BV_ras <- FUN.DownBV(T_Start = 1985, T_End = 2015, 
										 Dir = Dir.Data.Envir, Force = FALSE)

## Posthoc Data -----------------------------------------------------------
message("Retrieving additional covariates")
#' For relating SDM outputs to other characteristics of interest to users
PH_nutrient <- raster("https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq1.asc")
PH_toxicity <- raster("https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq6.asc")
PH_stack <- stack(PH_nutrient, PH_toxicity)
names(PH_stack) <- c("Nutrient", "Toxicity")

## SDM Data Preparations --------------------------------------------------
message("Preparing data for SDM workflow")
SDMInput_ls <- FUN.PrepSDMData(occ_ls = Species_ls$occs, BV_ras = BV_ras, 
															 Dir = Dir.Data, Force = FALSE)

# ANALYSIS ================================================================
## SDM Execution ----------------------------------------------------------
message("Executing SDM workflows")
SDMModel_ls <- FUN.ExecSDM(SDMData_ls = SDMInput_ls, BV_ras = BV_ras, 
													 Dir = Dir.Exports, Force = FALSE)

# ## SDM Prediction ---------------------------------------------------------
# SDMPred_ls <- FUN.PredSDM(SDMModel_ls = SDMModel_ls, BV_ras = BV_ras,
# 											 Dir = Dir.Exports, Force = FALSE)
# 
# # OUTPUTS =================================================================
# ## Plotting ---------------------------------------------------------------
# SDM_viz <- FUN.Viz(SDMModel_ls = SDMModel_ls, SDMInput_ls = SDMInput_ls, BV_ras = BV_ras,
# 				Dir = Dir.Exports)
# 
# ## Posthoc ----------------------------------------------------------------
# Posthoc_viz <- FUN.Posthoc(SDMModel_ls = SDMModel_ls, Covariates = PH_stack, CutOff = 0.6,
# 						Dir = Dir.Exports)
