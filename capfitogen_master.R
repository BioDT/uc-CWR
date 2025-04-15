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
#'  - Capfitogen submodule (from https://github.com/evalieungh/Capfitogen)
#'  - R_Scripts directory containing:
#'  	- "MoDGP-commonlines.R"
#'  	- "SHARED-APICredentials.R" -- NB! internal to project, ask for access
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
message(paste("------------------------------", 
              " starting data download/load  ",
              "------------------------------", 
              sep = "\n"))

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

## Environmental Data (CAPFITOGEN) --------------------------------------------
# make a template raster to resample to
template_raster <- rast(nrows=1800, ncols=4320, nlyr=1)
values(template_raster) <- rnorm(ncell(template_raster),1,1)
ext(template_raster) <- 
print(ext(example_r))

# download the default data from CAPFITOGEN.
all_predictors <- FUN.DownCAPFITOGEN(
  Dir = Dir.Data.Envir,
  Force = FALSE,
  resample_to_match = template_raster
)

## Protected areas database -----------------------------------------------
#' download shapefiles for protected areas to overlay with Complementa tool.
#' The FUN.DownWDPA function will save the file to a folder, but not load it 
#' into RStudio as an object.
MmmYYYY <- format(Sys.Date(), "%b%Y")
FUN.DownWDPA(
  # download from url:
  wdpa_url = paste0(
    "https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_",
    MmmYYYY, "_Public_shp.zip"),
  # save the downloaded zipfile as:
  wdpa_destination = file.path(Dir.Capfitogen.WDPA,
                               paste0("WDPA_",
                                      MmmYYYY,
                                      "_Public_shp.zip")),
  # do not overwrite existing data
  Force = FALSE)

# TEMP check wdpa files
wdpa <- read_sf(file.path(Dir.Capfitogen.WDPA, 
                          "wdpa", "global_wdpa_polygons.gpkg"))

# if supplied, crop all the data to a map of native species range
crop_to_native_range <- function(
    Dir = getwd(),
    Force = TRUE,
    native_range_map = NULL) {
  # define a file name
  FNAME <- file.path("Data/Environment/capfitogen_cropped.nc")
  
  # check if cropped data already exists and whether to overwrite
  if (!Force & file.exists(FNAME)) {
    message(
      paste0(FNAME, "exists already. It has been loaded from the disk.
             If you wish to override the present data, please specify Force = TRUE"))
    capfitogen_cropped <- rast(FNAME)
    return(capfitogen_cropped)
  } else if (!file.exists(FNAME)) {
    # if Force = TRUE or there is no preexisting data, 
    # check that the native range map is supplied and valid
    if (!is.null(native_range_map)) {
      # get the extent of the supplied native range
      
      # crop the environmental data to the native range
      
      # save the cropped version
    }
  }
}

# CAPFITOGEN pipeline =========================================================
message(paste("------------------------------", 
              " starting Capfitogen pipeline ",
              "------------------------------", 
              sep = "\n"))

### Format GBIF data ----
# need a data frame named 'puntos' = points with occurrence points
puntos <- data.frame(POINTID = 1:length(Species_ls[["occs"]][["DECLATITUDE"]]),
                     POINT_X = Species_ls[["occs"]][["DECLONGITUDE"]],
                     POINT_Y = Species_ls[["occs"]][["DECLATITUDE"]])

### create 'pasaporte' ----
message("create Pasaporte file")
#' Capfitogen uses a file named "pasaporte" with species/taxa occurrences. 
#' The file uses Darwincore names, so it should be OK to use the occurrences 
#' from the GBIF download directly. 
#' Place it in the right folder so Capfitogen can find it:
pasaporte_file_name = paste0(sub(pattern = " ",
                                 replacement = "_",
                                 SPECIES),
                             ".txt")

write.table(Species_ls[["occs"]],
            file.path("Capfitogen/Pasaporte",
                      pasaporte_file_name),
            sep = "\t",)

## Variable selection ---------------------------------------------------------
message("running variable selection")

# predefine list of variables to keep
predictors_to_keep <- c(
  
)

# run variable selection based on variable inflation factor usdm::vif
predictor_vifs <-
  vifcor(
    all_predictors,
    th = 0.8, # threshold of correlation
    keep = NULL, # if wanted, vector of variables to keep no matter what
    size = 5000, # subset size in case of big data (default 5000)
    method = "pearson" # 'pearson','kendall','spearman'
  )

# check which variables are kept
variables_to_keep <-
  names(all_predictors)[names(all_predictors) %nin% predictor_vifs@excluded]
message("variables kept after excluding the most correlated ones:")
print(variables_to_keep)

# subset variables to exclude highly correlated ones
predictors <- all_predictors[[(variables_to_keep)]]
predictors <- raster::stack(predictors)

# get bioclimatic variable names that matches Capfitogen's format
predictor_names <- names(predictors)
bioclim_predictor_names <- predictor_names[grep("BIO", predictor_names)]
bioclim_predictor_codes <- sub("_.*", "", tolower(bioclim_predictor_names))
bioclim_predictor_codes <- sub("o", "o_", bioclim_predictor_codes)
capfitogen_bioclim_names <- read.delim("Capfitogen/bioclim.txt", 
                                       fileEncoding = "latin1")
bioclim_subset <-
  capfitogen_bioclim_names[capfitogen_bioclim_names$VARCODE %in% bioclim_predictor_codes, ]
bioclim_predictor_names_capfitogen <- bioclim_subset$VARDESCR

# save variables in CAPFITOGEN folder
if (!dir.exists(file.path(Dir.Capfitogen, "rdatapoints/world/9x9"))) {
  dir.create(file.path(Dir.Capfitogen, "rdatapoints/world/9x9"),
             recursive = TRUE)
  dir.create(file.path(Dir.Capfitogen, "rdatamaps/world/9x9"),
             recursive = TRUE)
}

saveRDS(predictors,
        "Capfitogen/rdatapoints/world/9x9/base9x9.RData")
save(predictors,
     file = "Capfitogen/rdatapoints/world/9x9/base9x9.RData")

# names(predictors[[1:length(bioclim_predictor_names)]]) <- bioclim_predictor_codes
predictor_names_for_saving <-
  c(bioclim_predictor_codes,
    predictor_names[grep("BIO", predictor_names, invert = TRUE)]
    )

for (i in 1:dim(predictors)[3]) {
  file_name_path = file.path("Capfitogen/rdatamaps/world/9x9",
                             paste0(predictor_names_for_saving[i],".tif"))
  writeRaster(predictors[[i]],
              file_name_path,
              overwrite = TRUE)
}

## Modify Capfitogen possible values ------------------------------------------

# add a line with our data resolution to list of possible values
load(file.path(Dir.Capfitogen, "resol.RData"))
if (nrow(resol[resol$resol == "9x9",]) < 1) {
  line = paste("\"celdas 9x9 km aprox (4.5 arc-min)\"",	"\"9x9\"",	0.075,
               sep = "\t")
  write(line,
        file = file.path(Dir.Capfitogen, "resol.txt"),
        append = TRUE)
  
  resol <- rbind(
    resol,
    data.frame(
      resolucion = "celdas 9x9 km aprox (4.5 arc-min)",
      resol = "9x9",
      resoldec = 0.075))
  
  save(resol, file = file.path(Dir.Capfitogen, "resol.RData"))
}
rm(resol)

# create template file for logging script processes
if (!file.exists(file.path(Dir.Capfitogen.Error,"process_info.txt"))) {
  file.create(file.path(Dir.Capfitogen.Error,"process_info.txt"))
}

# add geophysical variables to list of possible variables
load(file.path(Dir.Capfitogen, "geophys.RData"))
if (nrow(geophys[geophys$VARCODE == "wind_max", ]) < 1) {
  geophys <- rbind(
    geophys,
    data.frame(
      VARID = 145,
      VARCODE = "wind_max",
      VARDESCR_EN = "mean wind speed of windiest month (annual max of monthly means)",
      VARDESCR = "mean_wind_speed_of_windiest_month",
      VARUNIDAD = "ms-1",
      VARFUENTE = "Derivada de Worldclim",
      VARMODULO = "Geofisico/Geophysic",
      FUENTELINK = "http://worldclim.org"
    )
  )
  save(geophys, file = file.path(Dir.Capfitogen, "geophys.RData"))
  
  # rename geophysical variable files ---- NB! will break when edaphic vars are added...
  number_of_geophys_variables <- length(predictor_names[grep("BIO",
                                                             predictor_names,
                                                             invert = TRUE)])
  for (i in 1:number_of_geophys_variables) {
    from_name = predictor_names[grep("BIO",
                                     predictor_names,
                                     invert = TRUE)][i]
    
    to_name = geophys[geophys$VARDESCR == from_name, "VARCODE"][1]
    file.rename(
      from = file.path(
        "Capfitogen/rdatamaps/world/9x9",
        paste0(from_name, ".tif")),
      to = file.path("Capfitogen/rdatamaps/world/9x9",
                     paste0(to_name, ".tif"))
    )
  }
}

rm(geophys)

## Clustering and map creation: ELCmapas ---------------------------------------
message("setting parameters and running ELC map script (ecogeographic land characterization)")
### Parameters for ELC maps ----
ruta <- Dir.Capfitogen # path to capfitogen scripts
resultados <- Dir.Capfitogen.ELCMap # directory to place results
pasaporte <- pasaporte_file_name # species occurrence data

pais <- "world" # global extent - big modifications will be necessary to use different extent
geoqual <- FALSE
totalqual<-30 # Only applies if GEOQUAL=TRUE, must be a value between 0 and 100
duplicat <- TRUE # duplicat=TRUE indicates that records of the same GENUS/SPECIES/SUBTAXA will be deleted 
distdup <- 1 # distance threshold in km to remove duplicates from same population
resol1 <- "celdas 9x9 km aprox (4.5 arc-min)" # resolution
latitud <- FALSE #Only applies if ecogeo=TRUE; whether to use latitude variable (Y) as a geophysical variable from 'pasaporte'
longitud <- FALSE 

bioclimv <- bioclim_predictor_names_capfitogen #
edaphv <- names(geophysical_variables)#names(edaphic_variables) #  edaphic variables (defaults from SOILGRIDS)
geophysv <- names(geophysical_variables) # geophysical variables

maxg <- 20 # maximum number of clusters per component 
metodo <- "kmeansbic" # clustering algorithm type. Options: medoides, elbow, calinski, ssi, bic
iterat <- 10 # if metodo="Calinski" or "ssi", the number of iterations to calculate the optimal number of clusters.

# run the script
message("Clustering and creating maps")
source(file.path(Dir.Capfitogen, 
                 "scripts/Tools Herramientas/ELCmapas.R"))
setwd(Dir.Base)

## Overlaying conservation maps "Complementa" ---------------------------------
#' create template file for logging script processes
if (!file.exists(file.path(Dir.Results.Complementa.Error,"process_info.txt"))) {
  file.create(file.path(Dir.Results.Complementa.Error,"process_info.txt"))
}

### parameters for Complementa ----
{
resultados <- Dir.Results.Complementa
pasaporte <- pasaporte_file_name

gaptype <- FALSE # Note: Representa tool a prerequisite of gaptype=TRUE 
gaptresh <- 4 #Only applies if gaptype=TRUE
gapna <- "exclude" #Only applies if gaptype=TRUE

celdas <- TRUE # Note: If celdas=TRUE, a complementarity analysis will be run by cells (grid)
resol1 <- "celdas 10x10 km aprox (5 arc-min)" #Only applies if celdas=TRUE
nceldas <- 10 #Only applies if celdas=TRUE, number of cells in a ranking (from most to least important in terms of taxa richness accumulation)

areas <- TRUE # If areas=TRUE, a complementary analysis will be run per protected areas (polygons), which can come from a world database (WDPA) or from a shapefile provided by the user. If areas=TRUE, at least one of the following two options (or both), WDPA or propio, must be TRUE, otherwise it may cause errors.
WDPA <- FALSE #Only applies if areas=TRUE
propio <- TRUE # =own, alternative user defined file instead of WDPA
nombre <- "global_wdpa_polygons.shp" #Only applies if propio=TRUE, name of alternative shapefile
campo <- "objectid" #Only applies if propio=TRUE, in campo you must specify the column of the shapefile table that contains the identifier code (ID) of each object (polygon) in the map of protected areas that the user provides through the shapefile. The name of the column must be inserted as it appears in the shapefile table, otherwise errors are generated
nareas <- 5 # the number of protected areas where the points from the passport table coordinates fall, areas organized in a ranking (from most to least important in terms of accumulation of taxa richness) that will be analyzed in detail. It can generate a problem or error if nareas is a very large number and the passport table has few records, or few different species, or all the points are highly concentrated spatially. 
coveran <- TRUE # if coveran=TRUE a coverage analysis will be generated for the network of protected areas and a folder called CoverageAnalysis should appear in the results within the resultados para areas folder 

niveltax <- "species"# At which taxonomic level the complementarity analysis is going to run (3 options: "genus", "species" or "subtaxa"). Take into account the following: If "genus" is selected, , in the GENUS column of the passport table there must be at least two different genera, or the same for "species" (SPECIES column) or "subtaxa" (SUBTAXA column)... if there are only NA values or there is only one value in the target column, it can generate errors.
datanatax <- FALSE # whether the NA values in genus, species or subtaxa will be taken into account as a different value. Any TRUE or FALSE option does not usually generate problems or errors.

mapaelcf <- TRUE # Note: Will an ELC map from a previous execution of the ELCmapas tool be used as an additional factor for classifying the taxonomic ranks for the complementarity analysis?
mapaelc <- "mapa_elc_world.grd" #Only applies if mapaelcf=TRUE, mapaelc must contain the name of the ELC map obtained by previously using the ELCmapas tool (.grd and .gri files that must always be in the CAPFITOGEN3/ELCmapas folder)
datanaelc <- FALSE # Only applies if mapaelcf=TRUE, indicates whether (TRUE) the records that fall in NA zones on the ELC map will be taken into account or not (FALSE)
data0elc <- FALSE # Only applies if mapaelcf=TRUE, indicates whether (TRUE) the records that fall in category 0 on the ELC map will be taken into account or not (FALSE)
}

# run the script
message("running Capfitogen Complementa tool for conservation areas")
setwd(Dir.Base)
source(file.path(Dir.Capfitogen, 
                 "/scripts/Tools Herramientas/Complementa.R"))

setwd(Dir.Base)

message(" - - - end of capfitogen script - - - ")

# end
