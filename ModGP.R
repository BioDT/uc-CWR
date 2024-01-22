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
	"pbapply", # for parallelised apply functions and estimators
	"usdm", # for vifcor
	"raster", # for spatial object handling
	"sf", # for spatialfeatures
	"pbapply", # for parallelised apply functions and estimators
	"devtools", # for non-CRAN installation
	# "intSDM", # for intSDMs
	"giscoR", # for shapefiles of Earth
	"Epi", # for ROC statistic
	"raster", # for raster objects
	"stars", # for fast raster operations
	"png", # for loading pngs back into R
	"grid" # for making pngs into ggplot object via rastergrob
)
sapply(package_vec, install.load.package)

### NON-CRAN PACKAGES ----
if("KrigR" %in% rownames(installed.packages()) == FALSE){ # KrigR check
	Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
	devtools::install_github("ErikKusch/KrigR")
}
library(KrigR)

if("intSDM" %in% rownames(installed.packages()) == FALSE){
	devtools::install_github("PhilipMostert/PointedSDMs")
	devtools::install_github("PhilipMostert/intSDM")
}
library(intSDM)

## API Credentials --------------------------------------------------------
try(source("X - PersonalSettings.R")) 
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

## Directories ------------------------------------------------------------
### Define directories in relation to project directory
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
PH_nutrient <- rast("https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq1.asc")
PH_toxicity <- rast("https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq6.asc")
PH_stack <- c(PH_nutrient, PH_toxicity)
PH_stack <- resample(PH_stack, rast(BV_ras))
PH_stack <- c(PH_stack, rast(BV_ras)$BIO1, rast(BV_ras)$BIO12)
names(PH_stack) <- c("Nutrient", "Toxicity", "Temperature", "Soil Moisture")

## SDM Data Preparations --------------------------------------------------
message("Preparing data for SDM workflow")
SDMInput_ls <- FUN.PrepSDMData(occ_ls = Species_ls$occs, BV_ras = BV_ras, 
															 Dir = Dir.Data, Force = FALSE)

# ANALYSIS ================================================================
## SDM Pre-Selection ------------------------------------------------------
message("SDM species pre-selection")
cov <- rast(BV_ras)[[1]]
Globe_sf <- gisco_get_countries()

useablespec_ls <- pblapply(SDMInput_ls, function(SDMModel_Iter){
		## make sf object
		Occ_df <- SDMModel_Iter$PA # SDMInput_ls$`Lathyrus vernus`$PA
		spec_name <- unique(Occ_df$species[Occ_df$PRESENCE == 1])
		Occ_df$modelSpec <- spec_name
		# print(spec_name)
		## assign correct crs
		Occ_sf <- st_as_sf(Occ_df, crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
		Occ_sf <- st_transform(Occ_sf, crs = st_crs(cov))
		## filter by land area of polygon
		Occ_sf <- st_filter(Occ_sf, Globe_sf)
		## filter additionally by covariate data
		valsext <- terra::extract(y = Occ_sf[Occ_sf$PRESENCE == 1, ], x = cov)
		useableocc <- sum(!is.na(valsext$BIO1))
		## identify unique cells filled by occurrences
		uniquecells <- sum(values(rasterize(Occ_sf, cov, field = "PRESENCE"))[,1] == 1, na.rm = TRUE)
		## report back
		data.frame(locs = useableocc, cells = uniquecells)
		})
useablespec_df <- do.call(rbind, useablespec_ls)

## SDM Execution ----------------------------------------------------------
message("Executing SDM workflows")
SDMModel_ls <- FUN.ExecSDM(
	SDMData_ls = SDMInput_ls[which(useablespec_df$locs > 40 & useablespec_df$cells > 40)], 
	BV_ras = BV_ras, Dir = Dir.Exports, Force = FALSE, KeepModels = TRUE,
	Drivers = PH_stack)

# # OUTPUTS =================================================================
# ## Shiny Data Object ------------------------------------------------------
# SDM_outs <- FUN.ShinyPrep(SDMModel_ls = SDMModel_ls, 
# 													SDMInput_ls = SDMInput_ls, 
# 													Dir = Dir.Exports)
# save(SDM_outs, file = "Tomas.RData")
# 
# ## Plotting ---------------------------------------------------------------
# SDM_viz <- FUN.Viz(SDM_outs = SDM_outs,
# 									 BV_ras = BV_ras,
# 									 Covariates = PH_stack,
# 									 Dir = Dir.Exports)
