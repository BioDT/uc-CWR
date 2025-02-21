## Default flags for runtime environment
RUNNING_ON_LUMI <- FALSE
RUNNING_ON_DESTINE <- FALSE

## Packages ---------------------------------------------------------------
install.load.package <- function(x) {
	if (!require(x, character.only = TRUE))
		install.packages(x, repos='http://cran.us.r-project.org')
	require(x, character.only = TRUE)
}

# HJ: to do: remove unneeded packages
# EL: trying to comment out some now to test if we run into issues

### CRAN PACKAGES ----
package_vec <- c(
  'automap', # automatic interpolation (for KrigR)
  'cowplot', # grid plotting
  'exactextractr', # HJ: added to solve extraction problems
  #'geodata', # HJ: added to get soil data for testing
  'ggplot2', # ggplot machinery
  'ggpp',
  'ggpmisc', # table plotting in ggplot environment
  'ggpubr', # t-test comparison in ggplot
  'gridExtra', # ggplot saving in PDF
  'ncdf4', # handling NetCDF files
  'parallel', # parallel runs
  'pbapply', # parallel runs with estimator bar
  'raster', # spatial data ----------------------- should be replaced by terra
  'remotes', # remote installation
  'rgbif', # GBIF access
  'rnaturalearth', # shapefiles
  'rnaturalearthdata', # needed for FUN.Down.BV()
  'sdm', # SDM machinery
  'sf', # spatial data
  'sp', # spatial data
  'terra', # spatial data
  'tidyr', # gather()
  'usdm', # vifcor()
  'viridis', # colour palette
  'bit64',
  'iterators',
  'gdalUtilities', # to download from SoilGrids (FUN.DownEV)
  
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

### NON-CRAN PACKAGES ----
# check if KrigR is missing or outdated 
if(packageVersion("KrigR") < "0.9.6.1" ||
   "KrigR" %in% rownames(installed.packages()) == FALSE) {
  message("installing KrigR from github.com/ErikKusch/KrigR")
  devtools::install_github("https://github.com/ErikKusch/KrigR",
                           ref = "Development")
}
library(KrigR)

if ("mraster" %in% rownames(installed.packages()) == FALSE) {
  # KrigR check
  remotes::install_github("babaknaimi/mraster")
}
library(mraster)

if (!("maxent" %in% unlist(getmethodNames()))) {
  sdm::installAll()
} # install methods for sdm package

## updating package_vec for handling of parallel environments
package_vec <- c(package_vec, "KrigR", "mraster")

## Functionality ----------------------------------------------------------
#' a function for negation of %in% function
`%nin%` <- Negate(`%in%`) 

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
Dir.Data <- file.path(Dir.Base, "Data")
Dir.Data.ModGP <- file.path(Dir.Data, "ModGP")
Dir.Data.GBIF <- file.path(Dir.Data, "GBIF")
Dir.Data.Envir <- file.path(Dir.Data, "Environment")
Dir.Exports <- file.path(Dir.Base, "Exports")
Dir.Exports.ModGP <- file.path(Dir.Exports, "ModGP")
Dir.Exports.Capfitogen <- file.path(Dir.Exports, "Capfitogen")
Dir.R_scripts <- file.path(Dir.Base, "R_scripts")
Dir.Capfitogen <- file.path(Dir.Base, "Capfitogen-main")
Dir.Results <- file.path(Dir.Base, "results")
Dir.Results.ECLMap <- file.path(Dir.Results, "ECLMap")
Dir.Results.ECLMap.Error <- file.path(Dir.Results.ECLMap, "Error")
Dir.Results.Complementa <- file.path(Dir.Results, "Complementa")
Dir.Results.Complementa.Error <- file.path(Dir.Results.Complementa, "Error")

### Create directories which aren't present yet
Dirs <- grep(ls(), pattern = "Dir.", value = TRUE)
CreateDir <- sapply(Dirs, function(x){
	x <- eval(parse(text=x))
	if(!dir.exists(x)) dir.create(x)})
rm(Dirs)
