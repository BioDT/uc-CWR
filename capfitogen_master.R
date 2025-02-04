#' ####################################################################### #
#' PROJECT: [BioDT CWR - Capfitogen] 
#' CONTENTS: 
#'  - Loading/installing packages
#'  - Execution of shared data pipeline
#'    - Species occurrence download from GBIF
#'    - Environmental data load/download
#'  - Execution of data formatting for CAPFITOGEN
#'  - Execution of Capfitogen pipeline
#'  DEPENDENCIES:
#'  - R_Scripts directory containing:
#'  	- "MoDGP-commonlines.R"
#'  	- "SHARED-APICredentials.R" -- NB! internal to project members, ask for access
#'  	- "SHARED-Data.R"
#'  	- "ELCmaps" - CAPFITOGEN tool
#'  	- "Complementa" - CAPFITOGEN tool
#' AUTHORS: [Erik Kusch, Heli Juottonen, Eva Lieungh]
#' Capfitogen credit: Parra-Quijano et al. 2021, 
#'                    https://repositorio.unal.edu.co/handle/unal/85787
#' ####################################################################### #

# PREAMBLE ================================================================
set.seed(42) # making things reproducibly random
rm(list=ls()) # clean environment

# Read species from command-line argument
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  # Default species
  SPECIES <- "Lathyrus angulatus"
} else {
  SPECIES <- args[1]
}
message(sprintf("SPECIES = %s", SPECIES))

### Define directories in relation to project directory
Dir.Base <- getwd()
Dir.Scripts <- file.path(Dir.Base, "R_scripts")

## source packages, directories, simple functions (...) 
source(file.path(Dir.Scripts, "ModGP-commonlines.R"))

## API Credentials --------------------------------------------------------
try(source(file.path(Dir.R_scripts, "SHARED-APICredentials.R")))
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

## NUMBER OF CORES
if(!exists("numberOfCores")){ # Core check: if number of cores for parallel processing has not been set yet
  numberOfCores <- as.numeric(readline(prompt = paste("How many cores do you want to allocate to these processes? Your machine has", parallel::detectCores())))
} # end of Core check
message(sprintf("numberOfCores = %d", numberOfCores))

# DATA ====================================================================
## Run SHARED-Data script -------------------------------------------------
## defines FUN.DownGBIF(), FUN.DownBV(), FUN.DownEV()
source(file.path(Dir.R_scripts, "SHARED-Data.R"))

## GBIF Data --------------------------------------------------------------
message("Downloading new or loading existing GBIF data")
## species of interest
Species_ls <- FUN.DownGBIF(
  species = SPECIES, # which species to pull data for
  Dir = Dir.Data.GBIF, # where to store the data output on disk
  Force = FALSE, # overwrite (TRUE) already present data or not (FALSE)
  Mode = "Capfitogen", # query download for one species
  parallel = 1 # no speed gain here for parallelising on personal machine
)

## Environmental Data -----------------------------------------------------
##' Bioclomatic data: 19 BioClim variables
##' is each file of each variable >20GB? 
message("Downloading new or loading existing 19 BioClim bioclimatic variables")
bioclim_data <- FUN.DownBV(
  T_Start = 2000, # what year to begin climatology calculation in
  T_End = 2001, # what year to end climatology calculation in
  Dir = Dir.Data.Envir, # where to store the data output on disk
  Force = FALSE # do not overwrite already present data
  )

## Edaphic data: 
message("Retrieving edaphic variables")
edaphic_data <- FUN.DownEV(
  Dir = Dir.Data.Envir,
  Force = FALSE,
  resample_to_match = bioclim_data[[1]]
)

# PH_nutrient <- raster("https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq1.asc")
# PH_toxicity <- raster("https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq6.asc")
# PH_stack <- stack(PH_nutrient, PH_toxicity)
# PH_stack <- raster::resample(PH_stack, # rasters to be resampled
#                              bioclim_data[[1]]) # raster with parameters to be resampled to
# PH_stack <- stack(PH_stack, bioclim_data$BIO1, bioclim_data$BIO12)
# names(PH_stack) <- c("Nutrient", "Toxicity", "Temperature", "Soil Moisture")


#' existing data in "Data/Environment/BV-1985-2015.nc" 
#' and soil data in .bil under /soil downloaded from Harmonized World
#' Soil Database version 2.0
#'
# HJ: this section is not ready, used ready .nc files for testing instead
# Three functions: 
# 1. Bioclimatic data: FUN.DownBV
# 2. Edaphic data: FUN.DownEV
# 3. Geophysical data: FUN.DownGV

# message("Retrieving environmental data")
# 
# # Bioclimatic data 
# 
# message("Retrieving bioclimatic data")
# 
# # HJ: bioclimatic data lines from ModGP. TO DO: adapt to Capfitogen
# 
# biocl_ras <- FUN.DownBV(T_Start = 1985, # what year to begin climatology calculation in
#                      T_End = 2015, # what year to end climatology calculation in
#                      Dir = Dir.Data.Envir, # where to store the data output on disk
#                     Force = FALSE # do not overwrite already present data
# )
# 
# # Edaphic data

# edaph_ras <- FUN.DownEV("soc", "silt", "sand") # TO DO

#geophy_ras <- FUN.DownGV( ) # TO DO

## read variables --------------------------------------------------------
bioclim_ras <- terra::rast(file.path(Dir.Data.Envir, 
                                     "BV_1985-2015.nc"))
bioclim_ras <- terra::project(bioclim_ras, 
                              "EPSG:4326") # WGS84; World Geodetic System 1984
BioClim_names <- c( ## BioClim variable names, see https://www.worldclim.org/data/bioclim.html
  "BIO1_Annual_Mean_Temperature",
  "BIO2_Mean_Diurnal_Range",
  "BIO3_Isothermality",
  "BIO4_Temperature_Seasonality",
  "BIO5_Max_Temperature_of_Warmest_Month",
  "BIO6_Min_Temperature_of_Coldest_Month",
  "BIO7_Temperature_Annual_Range",
  "BIO8_Mean_Temperature_of_Wettest_Quarter",
  "BIO9_Mean_Temperature_of_Driest_Quarter",
  "BIO10_Mean_Temperature_of_Warmest_Quarter",
  "BIO11_Mean_Temperature_of_Coldest_Quarter",
  "BIO12_Annual_Precipitation",
  "BIO13_Precipitation_of_Wettest_Month",
  "BIO14_Precipitation_of_Driest_Month",
  "BIO15_Precipitation_Seasonality",
  "BIO16_Precipitation_of_Wettest_Quarter",
  "BIO17_Precipitation_of_Driest_Quarter",
  "BIO18_Precipitation_of_Warmest_Quarter",
  "BIO19_Precipitation_of_Coldest_Quarter")
names(bioclim_ras) <- BioClim_names


geophys_ras <- terra::rast(file.path(Dir.Data.Envir, "geophys.nc"))

# geophys_ras <- terra::project(geophys_ras, "EPSG:4326")
# names(geophys_ras) <- geophysv
# edaph_ras <- terra::rast(file.path(Dir.Data.Envir, "edaph.nc"))
# names(edaph_ras) <- edaphv


# 

# CAPFITOGEN pipeline =========================================================
## Parameters -----------------------------------------------------------------
## copied and shortened from CAPFITOGEN's "Parameters_SelecVar.R" script.
# ruta <- "C:/CAPFITOGEN3" # replace with other paths
extent <- pais <- "World"
pasaporte <- file.path(Dir.Data.GBIF, "filename") # species observations - enter GBIF data file, check if column names work
geoqual <- FALSE # ?
# totalqual<-60 #Only applies if geoqual=TRUE
distdup <- 1 # distance threshold in km to remove duplicates from same population
resol1 <- "Celdas 1x1 km aprox (30 arc-seg)" # resolution, change to 9x9
buffy <- FALSE # buffer zone?
# tamp <- 1000 #Only applies when buffy=TRUE
bioclimv <- BioClim_names #c("tmean_1","vapr_annual","prec_1") # bioclimatic variables, altered by HJ with existing data
edaphv <- c("s_silt","s_sand","s_soilwater_cap") #  edaphic variables (defaults from SOILGRIDS)
geophysv <- c("alt","aspect") # geophysical variables
latitud <- FALSE #Only applies if ecogeo=TRUE; whether to use latitude variable (Y) as a geophysical variable from 'pasaporte'
longitud <- FALSE 
percenRF <- 0.66 # percentage of variables that will be selected by Random Forest 
percenCorr <- 0.33 # percentage of variables that will be selected by the analysis of bivariate correlations, which is executed after the selection by Random Forest (for example, if you wanted to select 1/3 of the total of variables by bivariate correlations, percenRF would be 0.33
CorrValue <- 0.5 # correlation threshold value, above (in its positive form) or below (in its negative form) of which it is assumed that there is a correlation between two variables.
pValue <- 0.05 # significance threshold value for bivariate correlations.
nminvar <- 3 # minimum number of variables to select per component. For example, although the processes of variable selection by RF and bivariate correlation indicate that two variables will be selected, if the nminvar number is 3, the selection process by correlations will select the three least correlated variables.
ecogeopcaxe <- 4 # number of axes (principal components) that will be shown in the tables of eigenvectors, eigenvalues and the PCA scores. ecogeopcaxe cannot be greater than the smallest number of variables to be evaluated per component
resultados <- Dir.Results # directory to place results


## Variable selection: SelecVar ------------------------------------------------
#' run variable selection (script VarSelection.R for each category of environmental variables):
#' 
source(file.path(Dir.R_scripts, "VarSelection.R")) # complete HJs version, or implement original CAPFITOGEN solution with some additional code before/after?

message("Selecting variables")
bioclim_ext <- FUN.VarSelection(specdata = Species_ls$occs, #occ_ls, #
                                       varstack = bioclim_ras)
                                      # buf = 2 # HJ: buffer doesn't work properly with terra, gets stuck?


geophys_ext <- FUN.VarSelection(specdata = Species_ls$occs, #occ_ls
                                       varstack = geophys_ras)
                                      # buf = 2 # HJ: buffer doesn't work properly with terra, gets stuck?

edaph_ext <- FUN.VarSelection(specdata = Species_ls$occs, #occ_ls
                                  varstack = edaph_ras
                                  # buf = 2 # HJ: buffer doesn't work properly with terra, gets stuck?
                                  )

#results <- "results/SelectVar"

## Clustering and map creation: ELCmapas ---------------------------------------
message("Clustering and creating maps")

# inputs to clustering: extracted values after variable selection

bioclim_cl <- FUN.KmeansClust(ext_values = bioclim_ext, 
                              max_clusters = 8, 
                              vartype = 'bioclim')
geophys_cl <- FUN.KmeansClust(ext_values = geophys_ext, 
                              max_clusters = 8, 
                              vartype = 'geophys')
edaph_cl <- FUN.KmeansClust(ext_values = edaph_ext, 
                            max_clusters = 8, 
                            vartype = 'edaph')


# HJ: function NOT READY yet, end of map creation doesn't work
maps <- FUN.ELCmaps(edaph = edaph_clust, 
                    bioclim = bioclim_clust, 
                    geophys = geophys_cl)



