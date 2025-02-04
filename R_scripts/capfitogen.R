# this script starts with a copy of @trossi's 'capfitogen' script and will serve 
# as notes and tests for scripting. To be deleted when a full capfitogen pipeline
# is in place.
## Heli's handover notes --------------------------------------
#' Capfitogen R script modifications for CWR BioDT project
#' Heli Juottonen, CSC (heli.juottonen@csc.fi) (please email if any questions!)
#' 
#' Main script: capfitogen_master_061124.R
#' 
#' sources the following scripts:
#' 
#' 1. SHARED-Data.R
#' 	1. downloading species data from GBIF: FUN.DownGBIF (from ModGP)
#' 	2. downloading environmental data (.nc files):
#' 		FUN.DownBV, bioclimatic data (as in ModGP, modifications needed?)
#' 		FUN.DownEV, edaphic data (not ready, only a very rough draft)
#' 		FUN.DownGV, geophysical data (not ready, only a very rough draft)
#' 
#' 		unclear: best way to obtain the data as .nc files?
#' 		unclear: where to define which specific variables downloaded for each category?
#' 		unclear: how and where and if to set the geographic area?
#' 
#' 		outputs (=inputs for the next step): 
#' 			sf file of species occurrence: Species_ls$occs
#' 			raster stacks: bioclim_ras, edaph_ras, geophys_ras
#' 
#' 2. VarSelection.R
#' 	1. selection of variables separately for each category (bioclimatic, edaphic, geophysical):
#' 		FUN.VarSelection
#' 		uses the vifcor approach as ModGP
#' 
#' 		outputs (= inputs for the next step): 
#' 		values of selected variables extracted from raster stacks: bioclim_ext, edaph_ext, geophys_ext 
#' 
#' 3. ELCMap.R
#' 	1. clustering (kmeans/BIC): FUN.KmeansClust
#' 		run separately for each variable category (bioclimatic, edaphic, geophysical)
#' 
#' 		outputs (= inputs for the next step): bioclim_cl, geophys_cl, edaph_cl
#' 		(coordinates, cluster membership, extracted environmental variable values)
#' 
#' 	2. creating maps (not ready, requires someone with proper spatial data R skills)
#' 
#' 
#-------------------------------------------------
#test downloading soil data from harmonized world soil database v2.0 (hwsd)
hwsd_path = file.path(getwd(), "Data", "Environment")
hwsd_zipfile = paste(hwsd_path, "/HWSD2.zip", sep = "")
url = "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/HWSD2_RASTER.zip"
library(httr)
GET(url, write_disk(hwsd_zipfile))
## 1 km (30 arc-second) resolution
unzip(hwsd_zipfile,
      exdir = paste(hwsd_path, "/soil", sep = ""))
# test reading BIL file
hwsd_raster <- terra::rast("Data/Environment/soil/HWSD2.bil")
hwsd_raster
plot(hwsd_raster)
summary(hwsd_raster)
res(hswd_raster)
names(hwsd_raster[[1]])
# aggregate to coarser resolution by a factor of 9
# (bin 9x9 neighbouring pixels into one, and assign the bigger pixel the mean)
soil30 <- aggregate(hwsd_raster, fact = 9, fun = mean)
soil30
plot(soil30)
# a plot is made, but of what? There is only one layer of values, and it's not obvoius to me what those values are...

# testing with SoilGrids instead, looks there are more variables there
# see https://www.isric.org/explore/soilgrids/soilgrids-access
library(terra)
library(gdalUtilities)
#projection_string = '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection, https://en.wikipedia.org/wiki/Goode_homolosine_projection

soilGrids_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

  #' overview of datasets: https://www.isric.org/explore/soilgrids/faq-soilgrids#What_do_the_filename_codes_mean
  #' NB! Each global map occupies circa 5 GB! It takes a while to download.
  #' bdod_0-5cm_mean.vrt # bdod=Bulk density of the fine earth fraction, cg/cm³
  #' cec_0-5cm_mean.vrt # cec = Cation Exchange Capacity of the soil, 	mmol(c)/kg
  #' cfvo_0-5cm_mean.vrt # cfvo = Volumetric fraction of coarse fragments (> 2 mm) 	cm3/dm3 (vol‰)
  #' silt_0-5cm_mean.vrt # silt =	Proportion of silt particles (≥ 0.002 mm and ≤ 0.05/0.063 mm) in the fine earth fraction 	g/kg
  #' clay_0-5cm_mean.vrt # clay = Proportion of clay particles (< 0.002 mm) in the fine earth fraction 	g/kg
  #' sand_0-5cm_mean.vrt # sand =	Proportion of sand particles (> 0.05/0.063 mm) in the fine earth fraction 	g/kg
  #' nitrogen_0-5cm_mean.vrt # nitrogen = Total nitrogen (N) 	cg/kg
  #' phh2o_0-5cm_mean.vrt # phh2o =	Soil pH 	pHx10
  #' ocd_0-5cm_mean.vrt # ocd =	Organic carbon density 	hg/m³
  #' ocs_0-30cm_mean.vrt # ocs =	Organic carbon stocks 	t/ha
  #' soc_0-5cm_mean.vrt # soc =	Soil organic carbon content in the fine earth fraction 	dg/kg
  #' 
  #' in addition, https://files.isric.org/soilgrids/latest/data/wrb/
  #' has maps of soil types, as estimated probability of occurrence per type.
  #' MostProbable.vrt has the most probable soil type per gridcell.
  #' 
  # gdal_translate() converts raster data between different formats.
soilGrids_data <- gdal_translate(
  src_dataset = paste0(soilGrids_url,'ocs/ocs_0-30cm_mean.vrt'),
                                 paste0(Dir.Data.Envir, 
                                        "/crop_roi_igh_r.tif"),
                                 tr = c(2500,2500) # target resolution 
)

crop_roi_igh_r <- rast(paste0(Dir.Data.Envir, 
                              "/crop_roi_igh_r.tif"))

plot(crop_roi_igh_r)
crop_roi_igh_r
summary(crop_roi_igh_r)
# test downloading CAPFITOGEN scripts into R_Scripts


#-------------------------------------------------
# Main input file (pasaporte):
#   LathyrusData-ForCapfitogen_27oct2023.txt (by Carrie)
# Filter only one species for testing:
#   head -n 1 LathyrusData-ForCapfitogen_27oct2023.txt > LathyrusData-ForCapfitogen_27oct2023_niger_only.txt
#   grep "Lathyrus niger" LathyrusData-ForCapfitogen_27oct2023.txt >> LathyrusData-ForCapfitogen_27oct2023_niger_only.txt

# Global options
pasaporte_file <- "LathyrusData-ForCapfitogen_27oct2023_niger_only.txt"
country <- "World"
# resolution <- "Celdas 1x1 km aprox (30 arc-seg)"
# resolution <- "Celdas 5x5 km aprox (2.5 arc-min)"
resolution <- "celdas 20x20 km aprox (10 arc-min)"

# Paths
results_dpath <- file.path(getwd(), "Resultados")
root_dpath <- file.path(getwd(), "CAPFITOGEN3")
param_dpath <- file.path(root_dpath, "scripts", "Parameters scripts (English)")
tools_dpath <- file.path(root_dpath, "scripts", "Tools Herramientas")

dir.create(results_dpath)

# We execute SelecVar and ELCMapas modules in order
# The structure of each module execution is:
#  - execute the corresponding parameters file for default settings
#  - override relevant settings (ruta etc.) using variables defined above
#  - execute the correspoding analysis script (unless done already)
# Note! Scripts write to a common log file: CAPFITOGEN3/Error/process_info.txt

#### SelecVar #############################
message("SelecVar")
source(file.path(param_dpath, "Parameters_SelecVar_2021.R"))
file.copy(file.path(getwd(), pasaporte_file), file.path(root_dpath, "Pasaporte", pasaporte_file), overwrite=TRUE)
ruta <- root_dpath
pasaporte <- pasaporte_file
geoqual <- FALSE
pais <- country
resol1 <- resolution
resultados <- file.path(results_dpath, "SelecVar")
dir.create(resultados)
if (file.exists(file.path(resultados, "SelectedVariables_edaphic.xls"))) {
    message("- skipping")
} else {
    message("- executing")
    source(file.path(tools_dpath, "SelectVar.R"))
    # Prevent crosstalk with the next step
    rm(geophys)
}

#### ELCmapas #############################
message("ELCmapas")
source(file.path(param_dpath, "Parameters_ELCmapas_2021.R"))
ruta <- root_dpath
pais <- country
resol1 <- resolution
resultados <- file.path(results_dpath, "ELCmapas")
dir.create(resultados)
if (file.exists(file.path(resultados, "Producto.RData"))) {
    message("- skipping")
} else {
    message("- executing")
    source(file.path(tools_dpath, "ELCmapas.R"))
}
