# this script starts with a copy of @trossi's 'capfitogen' script and will serve 
# as notes and tests for scripting. To be deleted when a full capfitogen pipeline
# is in place.
#-------------------------------------------------
#test downloading soil data from harmonized world soil database v2.0
hwsd_path = file.path(getwd(), "Data", "Environment")
hwsd_zipfile = paste(hwsd_path, "/HWSD2.zip", sep = "")
url = "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/HWSD2_RASTER.zip"
library(httr)
GET(url, write_disk(hwsd_zipfile))
## 1 km (30 arc-second) resolution
unzip(hwsd_zipfile,
      exdir = paste(hwsd_path, "/soil", sep = ""))
# test reading BIL file
testraster <- terra::rast("Data/Environment/soil/HWSD2.bil")
testraster
# aggregate to coarser resolution by a factor of 9
# (bin 9x9 neighbouring pixels into one, and assign the bigger pixel the mean)
soil30 <- aggregate(testraster, fact = 9, fun = mean)
soil30
plot(soil30)
# a plot is made, but of what? There is only one layer of values, and it's not obvoius to me what those values are...


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
