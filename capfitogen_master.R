#' ####################################################################### #
#' PROJECT: [BioDT CWR - Capfitogen] 
#' CONTENTS: 
#'  - Loading/installing packages
#'  - Execution of data pipeline
#'    - Species occurrence download from GBIF
#'    - Environmental data load/download
#'  - Data formatting and parameter definition for CAPFITOGEN
#'  - Execution of CAPFITOGEN tools 
#'    - 'ELC maps'
#'    - 'Complementa'
#'  - Visualisation of outputs
#'  DEPENDENCIES:
#'  - R_Scripts directory containing:
#'  	- "MoDGP-commonlines.R"
#'  	- "SHARED-APICredentials.R" -- NB! internal to project members, ask for access
#'  	- "SHARED-Data.R"
#' AUTHORS: [Eva Lieungh, Erik Kusch, Heli Juottonen, Desalegn Chala]
#' Capfitogen credit: Parra-Quijano et al. 2021, 
#'    https://repositorio.unal.edu.co/handle/unal/85787
#' ####################################################################### #

# PREAMBLE ================================================================
set.seed(42) # making things reproducibly random
rm(list=ls()) # clean environment
gc()

# Read species from command-line argument
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  # Default species
  SPECIES <- "Lathyrus angulatus"
} else {
  SPECIES <- args[1]
}
message(sprintf("SPECIES = %s", SPECIES))

# Define directories in relation to project directory
Dir.Base <- getwd()
Dir.Scripts <- file.path(Dir.Base, "R_scripts")

# source packages, directories, simple functions (...) 
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
{
  if (!exists("numberOfCores")) {
    # Core check: if number of cores for parallel processing has not been set yet
    numberOfCores <-
      as.numeric(readline(
        prompt = paste(
          "How many cores do you want to allocate to these processes? Your machine has",
          parallel::detectCores()
        )
      ))
  } # end of Core check
  message(sprintf("numberOfCores = %d", numberOfCores))
}

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
bioclim_variables <- FUN.DownBV(
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
  target_resolution = c(250, 250),
  Force = FALSE,
  resample_to_match = bioclim_variables[[1]]
)

### Geophysical data ------
geophysical_variables <- FUN.DownGV(
  Dir = Dir.Data.Envir,
  Force = FALSE,
  resample_to_match = bioclim_variables[[1]]
)

# CAPFITOGEN pipeline =========================================================
## Download CAPFITOGEN scripts ------------------------------------------------
# download and unzip CAPFITOGEN repository
if (!file.exists("capfitogen-main.zip")) {
  download.file(url = "https://github.com/HMauricioParra/Capfitogen/archive/refs/heads/main.zip",
              destfile = "capfitogen-main.zip")
  unzip(zipfile = "capfitogen-main.zip")
}

if (!file.exists(file.path(Dir.Results.Complementa.Error,"process_info.txt"))) {
  file.create(file.path(Dir.Results.Complementa.Error,"process_info.txt"))
}

## Format GBIF data -----------------------------------------------------------
# need a data frame named 'puntos' = points with occurrence points
puntos <- data.frame(POINTID = 1:length(Species_ls[["occs"]][["DECLATITUDE"]]),
                     POINT_X = Species_ls[["occs"]][["DECLONGITUDE"]],
                     POINT_Y = Species_ls[["occs"]][["DECLATITUDE"]])

### create 'pasaporte' ----
#' pasaporte file uses Darwincore names? 
#' So it should be OK to use the occurrences from the GBIF download directly.
#' Place it in the right folder so Capfitogen can find it:
pasaporte_file_name = paste0(sub(pattern = " ",
                                 replacement = "_",
                                 SPECIES),
                             ".txt")

write.table(Species_ls[["occs"]],
            file.path("Capfitogen-main/Pasaporte",
                      pasaporte_file_name),
            sep = "\t",)

pasaportest <- read.table("Capfitogen-main/Pasaporte/Lathyrus_angulatus.txt",
                          header = TRUE)

## Variable selection ---------------------------------------------------------
# run variable selection based on variable inflation factor usdm::vif
all_predictors <- c(bioclim_variables, 
                    #edaphic_variables, # Error in xcor[mx[1], mx[2]] : subscript out of bounds / In addition: Warning message: / [spatSample] fewer values returned than requested 
                    geophysical_variables)

predictor_vifs <-
  vifcor(
    all_predictors,# replace with either BV, EV, GV to run separately per type
    th = 0.8, # threshold of correlation
    keep = NULL, # if wanted, list variables to keep no matter what
    size = 5000, # subset size in case of big data (default 5000)
    method = "pearson" # 'pearson','kendall','spearman'
  )

variables_to_keep <-
  names(all_predictors)[names(all_predictors) %nin% predictor_vifs@excluded]

message("variables kept after excluding the most correlated ones:")
print(variables_to_keep)

# subset variables to exclude highly correlated ones
predictors <- all_predictors[[(variables_to_keep)]]
predictors <- raster::stack(predictors)

# save variables in CAPFITOGEN folder
if (!dir.exists(file.path(Dir.Capfitogen, "rdatapoints/world/9x9"))) {
  dir.create(file.path(Dir.Capfitogen, "rdatapoints/world/9x9"))
  dir.create(file.path(Dir.Capfitogen, "rdatamaps/world/9x9"),
             recursive = TRUE)
}

saveRDS(predictors,
        "Capfitogen-main/rdatapoints/world/9x9/base9x9.RData")
save(predictors,
     file = "Capfitogen-main/rdatapoints/world/9x9/base9x9.RData")

predictor_names <- names(predictors)
  
for (i in 1:dim(predictors)[3]) {
  file_name_path = file.path("Capfitogen-main/rdatamaps/world/9x9",
                             paste0(names(predictors[[i]]),".tif"))
  writeRaster(predictors[[i]],
              file_name_path,
              overwrite=TRUE)
}

## Parameters -----------------------------------------------------------------
## used in CAPFITOGEN scripts below
ruta <- Dir.Capfitogen # path to capfitogen scripts
pais <- "World" # global extent - big modifications will be necessary to use different extent
pasaporte <- pasaporte_file_name # species occurrence data

geoqual <- FALSE
totalqual<-30 # Only applies if GEOQUAL=TRUE, must be a value between 0 and 100
duplicat <- TRUE # duplicat=TRUE indicates that records of the same GENUS/SPECIES/SUBTAXA will be deleted 
distdup <- 1 # distance threshold in km to remove duplicates from same population
resol1 <- "9x9" # resolution, change to 9x9
latitud <- FALSE #Only applies if ecogeo=TRUE; whether to use latitude variable (Y) as a geophysical variable from 'pasaporte'
longitud <- FALSE 
# nminvar <- 3 # minimum number of variables to select per component. For example, although the processes of variable selection by RF and bivariate correlation indicate that two variables will be selected, if the nminvar number is 3, the selection process by correlations will select the three least correlated variables.

bioclimv <- predictor_names[grep("BIO", predictor_names)] #
edaphv <- names(geophysical_variables)#names(edaphic_variables) #  edaphic variables (defaults from SOILGRIDS)
geophysv <- names(geophysical_variables) # geophysical variables
maxg <- 20 # maximum number of clusters per component 
metodo <- "kmeansbic" # clustering algorithm type. Options: medoides, elbow, calinski, ssi, bic
iterat <- 10 # if metodo="Calinski" or "ssi", the number of iterations to calculate the optimal number of clusters.

# parameters for Complementa tool
gaptype <- FALSE # Note: Representa tool a prerequisite of gaptype=TRUE 
gaptresh <- 4 #Only applies if gaptype=TRUE
gapna <- "exclude" #Only applies if gaptype=TRUE
celdas <- TRUE # Note: If celdas=TRUE, a complementarity analysis will be run by cells (grid)
resol1 <- "9x9"#"celdas 10x10 km aprox (5 arc-min)" #Only applies if celdas=TRUE
nceldas <- 10 #Only applies if celdas=TRUE, number of cells in a ranking (from most to least important in terms of taxa richness accumulation)
areas <- TRUE # If areas=TRUE, a complementary analysis will be run per protected areas (polygons), which can come from a world database (WDPA) or from a shapefile provided by the user. If areas=TRUE, at least one of the following two options (or both), WDPA or propio, must be TRUE, otherwise it may cause errors.
WDPA <- TRUE #Only applies if areas=TRUE
propio <- FALSE # =own, alternative user defined file instead of WDPA
nombre <- "EcuadorAreasProt" #Only applies if propio=TRUE, name of alternative shapefile
campo <- "objectid" #Only applies if propio=TRUE, in campo you must specify the column of the shapefile table that contains the identifier code (ID) of each object (polygon) in the map of protected areas that the user provides through the shapefile. The name of the column must be inserted as it appears in the shapefile table, otherwise errors are generated
nareas <- 5 # the number of protected areas where the points from the passport table coordinates fall, areas organized in a ranking (from most to least important in terms of accumulation of taxa richness) that will be analyzed in detail. It can generate a problem or error if nareas is a very large number and the passport table has few records, or few different species, or all the points are highly concentrated spatially. 
coveran <- TRUE # if coveran=TRUE a coverage analysis will be generated for the network of protected areas and a folder called CoverageAnalysis should appear in the results within the resultados para areas folder 
niveltax <- "species"# At which taxonomic level the complementarity analysis is going to run (3 options: "genus", "species" or "subtaxa"). Take into account the following: If "genus" is selected, , in the GENUS column of the passport table there must be at least two different genera, or the same for "species" (SPECIES column) or "subtaxa" (SUBTAXA column)... if there are only NA values or there is only one value in the target column, it can generate errors.
datanatax <- FALSE # whether the NA values in genus, species or subtaxa will be taken into account as a different value. Any TRUE or FALSE option does not usually generate problems or errors.
mapaelcf <- TRUE # Note: Will an ELC map from a previous execution of the ELCmapas tool be used as an additional factor for classifying the taxonomic ranks for the complementarity analysis?
mapaelc <- "mapa_elc_world.grd" #Only applies if mapaelcf=TRUE, mapaelc must contain the name of the ELC map obtained by previously using the ELCmapas tool (.grd and .gri files that must always be in the CAPFITOGEN3/ELCmapas folder)
datanaelc <- FALSE # Only applies if mapaelcf=TRUE, indicates whether (TRUE) the records that fall in NA zones on the ELC map will be taken into account or not (FALSE)
data0elc <- FALSE #Only applies if mapaelcf=TRUE, indicates whether (TRUE) the records that fall in category 0 on the ELC map will be taken into account or not (FALSE)

## Clustering and map creation: ELCmapas ---------------------------------------
resultados <- Dir.Results.ECLMap # directory to place results

message("Clustering and creating maps")

# run the script
##' NB! Change made in capfitogen script: 
##' replaced 'extract' with 'raster::extract' 
##' (some other package masked it and caused an error)
source(file.path(Dir.Capfitogen, 
                 "/scripts/Tools Herramientas/ELCmapas_BioDT.R"))
setwd(Dir.Base)

# quick visualisation of output
elc_tif_outputs <- list.files(path = Dir.Results.ECLMap,
                              pattern = "*.tif")

for (i in elc_tif_outputs) {
  map_i = rast(paste0(Dir.Results.ECLMap, 
                      "/", i))
  plot(map_i,
       main = i)
}

## Overlaying conservation maps "Complementa" ---------------------------------
resultados <- Dir.Results.Complementa

message("running Capfitogen Complementa tool for conservation areas")


#' NB! Manually copied the script into the folder, as it is missing on GH...
#' NB! changed "" to " " in line 91:
#' pasaporte<-read.delim(paste("Pasaporte/",pasaporte,sep=" "))
#' 
# run the script
pasaporte <- pasaporte_file_name
source(file.path(Dir.Capfitogen, 
                 "/scripts/Tools Herramientas/Complementa.R"))
#' stops at line 173 mapaelcf if loop
setwd(Dir.Base)


# visualise output