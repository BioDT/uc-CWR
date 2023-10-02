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
)
sapply(package_vec, install.load.package)

## Functionality ----------------------------------------------------------
source("X - Functions-Data.R")
source("X - Functions-SDM.R")
`%nin%` <- Negate(`%in%`) # a function for negation of %in% function

# DATA ====================================================================
## GBIF Data --------------------------------------------------------------
## species of interest
Species_ls <- FUN.DownGBIF(
	species = "Lathyrus sativus",
	Dir = Dir.Data.GBIF,
	Force = FALSE)

## Environmental Data -----------------------------------------------------
BV_ras <- FUN.DownBV(T_Start = 1985, T_End = 2015, 
										 Dir = Dir.Data.Envir, Force = FALSE)

## Posthoc Data -----------------------------------------------------------
#' For relating SDM outputs to other characteristics of interest to users
PH_nutrient <- raster("https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq1.asc")
PH_toxicity <- raster("https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq6.asc")

## SDM Data Preparations --------------------------------------------------
SDMInput_ls <- FUN.PrepSDMData(occ_ls = Species_ls$occs, BV_ras = BV_ras, 
															 Dir = Dir.Data, Force = FALSE)

# ANALYSIS ================================================================
if("maxent" %nin% unlist(getmethodNames())){sdm::installAll()} # install methods for sdm package

## SDM Execution ----------------------------------------------------------
SDMModel_ls <- FUN.ExecSDM(SDMData_ls = SDMInput_ls, BV_ras = BV_ras, 
													 Dir = Dir.Exports, Force = FALSE)

## SDM Prediction ---------------------------------------------------------

# EXPORT ==================================================================
## Results ----------------------------------------------------------------
save(p1, file = file.path(Dir.Data, paste0(spec_name, "_pred.Rdata")))
save(ev, file = file.path(Dir.Data, paste0(spec_name, "_eval.Rdata")))

## Plotting ---------------------------------------------------------------
## Data -------------------------------------------------------------------