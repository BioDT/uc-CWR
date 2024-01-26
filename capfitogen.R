# Main input file:
#   LathyrusData-ForCapfitogen_27oct2023.txt (by Carrie)
# Filter only one species for testing:
#   head -n 1 LathyrusData-ForCapfitogen_27oct2023.txt > LathyrusData-ForCapfitogen_27oct2023_niger_only.txt
#   grep "Lathyrus niger" LathyrusData-ForCapfitogen_27oct2023.txt >> LathyrusData-ForCapfitogen_27oct2023_niger_only.txt

# Global options
root_dpath <- file.path(getwd(), "CAPFITOGEN3")
results_dpath <- file.path(getwd(), "Resultados")
param_dpath <- file.path(root_dpath, "scripts", "Parameters scripts (English)")
tools_dpath <- file.path(root_dpath, "scripts", "Tools Herramientas")

pasaporte_file <- "LathyrusData-ForCapfitogen_27oct2023_niger_only.txt"  # Input file (see explanation above)

country <- "World"
# resolution <- "Celdas 1x1 km aprox (30 arc-seg)"
# resolution <- "Celdas 5x5 km aprox (2.5 arc-min)"
resolution <- "celdas 20x20 km aprox (10 arc-min)"


# We execute SelecVar and ELCMapas modules in order
# The structure of each module execution is:
#  - execute the corresponding parameters file for default settings
#  - override relevant settings (ruta etc.) using variables defined above
#  - execute the correspoding analysis script (unless done already)
# Note! Scripts write to a common log file: CAPFITOGEN3/Error/process_info.txt

dir.create(results_dpath)


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

