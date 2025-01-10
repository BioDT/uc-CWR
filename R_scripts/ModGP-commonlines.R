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

## Sourcing ---------------------------------------------------------------
source(file.path(Dir.Scripts,"SHARED-Data.R"))
source(file.path(Dir.Scripts,"ModGP-SDM.R"))
source(file.path(Dir.Scripts,"ModGP-Outputs.R"))
