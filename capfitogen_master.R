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
#'  - R Scripts directory containing:
#'  	- "ModGP-Outputs.R"
#'  	- "ModGP-SDM.R"
#'  	- "SHARED-APICredentials.R" -- NB! internal to project members, ask for access
#'  	- "SHARED-Data.R" 
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

## Packages ---------------------------------------------------------------
# Define function to load and/or install packages
install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}

# HJ: to do: remove unneeded packages
# EL: trying to comment out some now to test if we run into issues

### CRAN PACKAGES ----
package_vec <- c(
  #'automap', # automatic interpolation (for KrigR)
  'cowplot', # grid plotting
  'exactextractr', # HJ: added to solve extraction problems
  #'geodata', # HJ: added to get soil data for testing
  'ggplot2', # ggplot machinery
  'ggpmisc', # table plotting in ggplot environment
  'ggpubr', # t-test comparison in ggplot
  'gridExtra', # ggplot saving in PDF
  'ncdf4', # handling NetCDF files
  'parallel', # parallel runs
  'pbapply', # parallel runs with estimator bar
  #'raster', # spatial data ----------------------- should be replaced by terra
  'remotes', # remote installation
  'rgbif', # GBIF access
  #'rnaturalearth', # shapefiles
  'sdm', # SDM machinery
  'sf', # spatial data
  'sp', # spatial data
  'terra', # spatial data
  'tidyr', # gather()
  'usdm', # vifcor()
  'viridis', # colour palette
  'bit64',
  
  # Capfitogen SelectVar packages 
  # HJ: added here from Capfitogen SelectVar script. To do: remove unnecessary ones
  'dismo',
  'cluster',
  'ade4',
  'labdsv',
  'mclust',
  'clustvarsel',
  #'randomForest', # ---------------- replace with ranger?
  'ranger',
  
  # Capfitogen ECLmapas packages
  # HJ: added here from Capfitogen ECLmapas script. To do: remove unnecessary ones
  'modeltools',
  'flexmix',
  'fpc',
  'vegan',
  'adegenet' #find.clusters ELCmap.R
)
sapply(package_vec, install.load.package)

# ### NON-CRAN PACKAGES ---- are these necessary for capfitogen?
# if("KrigR" %in% rownames(installed.packages()) == FALSE){ # KrigR check
#   Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
#   remotes::install_github("https://github.com/cran/rgdal") # requires gdal-config!
#   remotes::install_github("ErikKusch/KrigR")
# }
# library(KrigR) # is KrigR necessary for capfitogen? 
# 
# if("mraster" %in% rownames(installed.packages()) == FALSE){ # KrigR check
#   remotes::install_github("babaknaimi/mraster")
# }
# library(mraster)
# 
# if(!("maxent" %in% unlist(getmethodNames()))){sdm::installAll()} # install methods for sdm package
# 
# # updating package_vec for handling of parallel environments
# package_vec <- c(package_vec, "KrigR", "mraster")

## Functionality ----------------------------------------------------------
`%nin%` <- Negate(`%in%`) # a function for negation of %in% (that is, %not_in%)

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
### assuming your working directory is '<yourlocalpath>/uc-CWR' and
### you have downloaded & unzipped the capfitogen scripts and data
{
Dir.Base <- getwd()
Dir.Scripts <- file.path(Dir.Base, "scripts")
Dir.R_scripts <- file.path(Dir.Base, "R_scripts")
Dir.Results <- file.path(Dir.Base, "results")
Dir.Results.SelectVar <- file.path(Dir.Results, "SelectVar")
Dir.Results.ECLMap <- file.path(Dir.Results, "ECLMap")
Dir.Data <- file.path(Dir.Base, "Data")
Dir.Data.Capfitogen <- file.path(Dir.Data, "Capfitogen")
Dir.Data.GBIF <- file.path(Dir.Data, "GBIF")
Dir.Data.Envir <- file.path(Dir.Data, "Environment")
Dir.Exports <- file.path(Dir.Base, "Exports")
Dir.Exports.Capfitogen <- file.path(Dir.Exports, "Capfitogen")
}
### Create directories which aren't present yet
Dirs <- grep(ls(), pattern = "Dir.", value = TRUE)
CreateDir <- sapply(Dirs, function(x){
  x <- eval(parse(text=x))
  if(!dir.exists(x)) dir.create(x)})
rm(Dirs)

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
# NUMBER OF CORES
if(!exists("numberOfCores")){ # Core check: if number of cores for parallel processing has not been set yet
  numberOfCores <- as.numeric(readline(prompt = paste("How many cores do you want to allocate to these processes? Your machine has", parallel::detectCores())))
} # end of Core check
message(sprintf("numberOfCores = %d", numberOfCores))

## Sourcing ---------------------------------------------------------------
source(file.path(Dir.R_scripts, "SHARED-Data.R"))
source(file.path(Dir.R_scripts, "VarSelection.R"))
source(file.path(Dir.R_scripts, "ELCMap.R")) #HJ: clustering ready, map part not

# DATA ====================================================================
## GBIF Data --------------------------------------------------------------

message("Retrieving GBIF data")
## species of interest
Species_ls <- FUN.DownGBIF(
  species = SPECIES, # which species to pull data for
  Dir = Dir.Data.GBIF, # where to store the data output on disk
  Force = TRUE, # do not overwrite already present data
  Mode = "Capfitogen", # query download for one species
  parallel = 1 # no speed gain here for parallelising on personal machine
)

## Environmental Data -----------------------------------------------------

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
# 
# message("Retrieving edaphic data")
# 
# #HJ: enter here which edaphic variables chosen?
# 
# edaph_ras <- FUN.DownEV("soc", "silt", "sand") # TO DO
#
#
#
# Geophysical data

# message("Retrieving geophysical data")

# HJ: enter here which geophysical variables chosen?

#geophy_ras <- FUN.DownGV( ) # TO DO



# HJ: part below was used to test scripts with existing .nc files
# ask Heli for the files if needed
# reading in existing environmental variable files
# file format: NetCDF (.nc)

# selecting a set of variables for each data type:
# bioclimv <- c("tmean_1","vapr_annual","prec_1")
# edaphv <- c("s_silt","s_sand","s_soilwater_cap")
# geophysv <- c("alt","aspect")
# 
# bioclim_ras <- terra::rast(file.path(Dir.Data.Envir, "bioclim.nc"))
# bioclim_ras <- terra::project(bioclim_ras, "EPSG:4326")
# names(bioclim_ras) <- bioclimv
# geophys_ras <- terra::rast(file.path(Dir.Data.Envir, "geophys.nc"))
# geophys_ras <- terra::project(geophys_ras, "EPSG:4326")
# names(geophys_ras) <- geophysv
# edaph_ras <- terra::rast(file.path(Dir.Data.Envir, "edaph.nc"))
# names(edaph_ras) <- edaphv


## Select variables --------------------------------------------------

#### SelecVar #############################

# run variable selection (script VarSelection.R for each category of environmental variables):

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

#### ELCmapas #############################
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



