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
#https://github.com/ErikKusch/KrigR/blob/94cdb9e0aa3cff9f996575f4fb3d10c617eb37ab/metadata/reanalysis-era5-land.RData
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
{# set API credentials for access to climate data store (CDS)
  try(source(file.path(Dir.R_scripts, "SHARED-APICredentials.R")))
  if (as.character(options("gbif_user")) == "NULL") {
    options(gbif_user = rstudioapi::askForPassword("my gbif username"))
  }
  if (as.character(options("gbif_email")) == "NULL") {
    options(gbif_email = rstudioapi::askForPassword("my registred gbif e-mail"))
  }
  if (as.character(options("gbif_pwd")) == "NULL") {
    options(gbif_pwd = rstudioapi::askForPassword("my gbif password"))
  }
  
  if (!exists("API_Key") |
      !exists("API_User")) {
    # CS API check: if CDS API credentials have not been specified elsewhere
    API_User <-
      readline(prompt = "Please enter your Climate Data Store API user number and hit ENTER.")
    API_Key <-
      readline(prompt = "Please enter your Climate Data Store API key number and hit ENTER.")
  } # end of CDS API check
}
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
### Bioclomatic data ------
##' 19 BioClim variables
##' FUN.DownBV uses KrigR to download ERA5 data from Climate Data Store (CDS)
##' is each file of each variable >20GB? 
##' Will this download Global Multi-resolution Terrain Elevation Data (GMTED2010) as well?
##' Temporal coverage: January 1950 to present ? https://cds.climate.copernicus.eu/datasets/derived-era5-land-daily-statistics?tab=overview
message("Downloading new or loading existing 19 BioClim bioclimatic variables")
bioclim_data <- FUN.DownBV(
  T_Start = 1999, # what year to begin climatology calculation in
  T_End = 1999, # what year to end climatology calculation in
  Dir = Dir.Data.Envir, # where to store the data output on disk
  Force = FALSE # do not overwrite already present data
  )

bioclim_variables <- terra::rast(file.path(Dir.Data.Envir, "BV_1985-2015.nc"))
BioClim_names <- c( 
  ## BioClim variable names, see https://www.worldclim.org/data/bioclim.html
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
names(bioclim_variables) <- BioClim_names

### Edaphic data ------ 
## NB! each file at 250x250m is ~20GB...
message("Downloading new or loading existing edaphic/soil variables")

edaphic_variables <- FUN.DownEV(
  Dir = Dir.Data.Envir,
  Force = TRUE,
  resample_to_match = bioclim_variables[[1]]
)

### Geophysical data ------

geophysical_variables <- FUN.DownGV(
  Dir = Dir.Data.Envir,
  Force = FALSE,
  resample_to_match = bioclim_variables[[1]]
)
  
terra::res(geophysical_variables)
geophysical_variables <- rast(geophysical_variables)

# CAPFITOGEN pipeline =========================================================
## Download CAPFITOGEN scripts and files --------------------------------------
files_to_download <- c(
  "https://raw.githubusercontent.com/HMauricioParra/Capfitogen/main/scripts/Tools%20Herramientas/ELCmapas.R",
  "https://raw.githubusercontent.com/HMauricioParra/Capfitogen/main/scripts/Parameters%20scripts%20(English)/Parameters_ELCmapas_2024_BioDT.R"
)

## download files in a for loop
for (i in 1:length(files_to_download)) {
  file_url = files_to_download[i]
  temp <- tempfile()
  download.file(file_url, temp)
  file_name = sub(".*/", "", files_to_download[i])
  message(paste("downloaded CAPFITOGEN script", file_name))
  file.copy(temp, paste0(Dir.R_scripts,"/", file_name))
  unlink(temp)
}

# alternative: download the whole repository,..
download.file(url = "https://github.com/HMauricioParra/Capfitogen/archive/refs/heads/main.zip",
              destfile = "capfitogen-main.zip")
unzip(zipfile = "capfitogen-main.zip")

Dir.Capfitogen = file.path(Dir.Base, "Capfitogen-main/")

# make folder for storing error log
dir.create(paste0(Dir.Results.ECLMap, "/Error"))

## Variable selection ---------------------------------------------------------
# run variable selection based on variable inflation factor usdm::vif
all_predictors <- c(bioclim_variables, 
                    #edaphic_variables, 
                    geophysical_variables)

predictor_vifs <-
  vifcor(
    all_predictors,# replace with either BV, EV, GV to run separately per type
    th = 0.8, # threshold of correlation
    keep = NULL, # if wanted, list variables to keep no matter what
    size = 1000, # subset size in case of big data (default 5000)
    method = "pearson" # 'pearson','kendall','spearman'
  )

variables_to_keep <-
  names(all_predictors)[names(all_predictors) %nin% predictor_vifs@excluded]

message("variables kept after excluding most correlated:")
print(variables_to_keep)

# subset variables to exclude highly correlated ones
predictors <- all_predictors[[(variables_to_keep)]]

# save variables in CAPFITOGEN folder
dir.create(file.path(Dir.Capfitogen,
           "rdatapoints/world/9x9"))

saveRDS(predictors,
        "Capfitogen-main/rdatapoints/world/9x9/base9x9.RData")

for (i in 1:length(depth(predictors))) {
  file_name_path = file.path("Capfitogen-main/rdatapoints/world/9x9",
                             paste0(names(predictors[[i]]),".tif"))
  writeRaster(predictors[[i]],
              file_name_path)
}

## Parameters -----------------------------------------------------------------
## copied and shortened from CAPFITOGEN scripts. 
## TO to: DELETE UNNECESSARY PARAMS

pais <- "World"
#pasaporte <- file.path(Dir.Data.GBIF, "filename") # species observations - enter GBIF data file, check if column names work
#geoqual <- FALSE # ?
# totalqual<-60 #Only applies if geoqual=TRUE
distdup <- 1 # distance threshold in km to remove duplicates from same population
resol1 <- "10x10" # resolution, change to 9x9
#buffy <- FALSE # buffer zone?
latitud <- FALSE #Only applies if ecogeo=TRUE; whether to use latitude variable (Y) as a geophysical variable from 'pasaporte'
longitud <- FALSE 
#percenRF <- 0.66 # percentage of variables that will be selected by Random Forest 
#percenCorr <- 0.33 # percentage of variables that will be selected by the analysis of bivariate correlations, which is executed after the selection by Random Forest (for example, if you wanted to select 1/3 of the total of variables by bivariate correlations, percenRF would be 0.33
#CorrValue <- 0.5 # correlation threshold value, above (in its positive form) or below (in its negative form) of which it is assumed that there is a correlation between two variables.
#pValue <- 0.05 # significance threshold value for bivariate correlations.
nminvar <- 3 # minimum number of variables to select per component. For example, although the processes of variable selection by RF and bivariate correlation indicate that two variables will be selected, if the nminvar number is 3, the selection process by correlations will select the three least correlated variables.
#ecogeopcaxe <- 4 # number of axes (principal components) that will be shown in the tables of eigenvectors, eigenvalues and the PCA scores. ecogeopcaxe cannot be greater than the smallest number of variables to be evaluated per component
resultados <- Dir.Results.ECLMap # directory to place results
ruta <- Dir.Capfitogen


## Clustering and map creation: ELCmapas ---------------------------------------
message("Clustering and creating maps")

# Set additional parameters
bioclimv <- BioClim_names #
edaphv <- names(edaphic_variables) #  edaphic variables (defaults from SOILGRIDS)
geophysv <- names(geophysical_variables) # geophysical variables

maxg <- 3 # maximum number of clusters per component 

metodo <- "kmeansbic" # clustering algorithm type. Options: medoides, elbow, calinski, ssi, bic
iterat <- 10 # if metodo="Calinski" or "ssi", the number of iterations to calculate the optimal number of clusters.

# run the script
source(file.path(Dir.Capfitogen, 
                 "/scripts/Tools Herramientas/ELCmapas_BioDT.R"))

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



