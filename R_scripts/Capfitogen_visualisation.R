##############################################################
#' Visualisation of Capfitogen inputs and output
#' CONTENTS:
#'  - Visualisation of input data
#'    - World Database on Protected Areas (WDPA)
#'  - Visualisation of outputs
#'    - ELC maps
#'    - 
#' DEPENDENCIES:
#'  - capfitogen_master.R (to download and create data)
#'  - ModGP-commonlines.R (packages, paths)
#' AUTHORS: [Eva Lieungh]
#' ###########################################################

# Load dependencies ------------------------------------------
# Define directories in relation to project directory
Dir.Base <- getwd()
Dir.Scripts <- file.path(Dir.Base, "R_scripts")

# source packages, directories, simple functions (...) 
source(file.path(Dir.Scripts, "ModGP-commonlines.R"))

# VISUALISE INPUTS ===========================================

# WDPA -------------------------------------------------------





# VISUALISE OUTPUTS ==========================================

# ELC maps ---------------------------------------------------
## quick visualisation ----
# List all the .tif files in the directory
elc_tif_outputs <- list.files(path = Dir.Results.ELCMap, 
                              pattern = "\\.tif$", 
                              full.names = TRUE)

# Loop over each .tif file
for (file_path in elc_tif_outputs) {
  # Read the raster file
  map_i <- rast(file_path)
  
  # Replace NaN with NA (if they exist)
  map_i[is.nan(values(map_i))] <- NA
  
  # Create a mask to highlight non-zero areas
  non_zero_mask <- mask(map_i, !is.na(map_i))
  
  # Convert to points to find non-zero values' extent
  points <- as.points(non_zero_mask, na.rm = TRUE)
  
  # If there are any valid points, proceed with cropping
  if (!is.null(points) && nrow(points) > 0) {
    # Calculate extent directly from the non-empty points
    coordinates <- terra::geom(points)[, c("x", "y")]
    xmin = min(coordinates[,"x"])
    xmax = max(coordinates[,"x"])
    ymin = min(coordinates[,"y"])
    ymax = max(coordinates[,"y"])
    non_zero_extent <- ext(xmin, xmax, ymin, ymax)
    
    # Crop the raster using this extent
    cropped_map <- crop(map_i, non_zero_extent)
    
    # Plot the cropped raster
    plot(cropped_map, main = basename(file_path))
  } else {
    plot(map_i, main = paste(basename(file_path), "(No non-zero values)"))
  }
}


# Complementa ---------------------------------------------------------

complementa_map <- rast(
  file.path(Dir.Results.Complementa,
            "AnalisisCeldas_CellAnalysis/Complementa_map.tif"))
plot(complementa_map)
complementa_map[is.nan(values(complementa_map))] <- NA
non_zero_mask <- mask(complementa_map,
                      !is.na(complementa_map))
complementa_points <- as.points(non_zero_mask, na.rm = TRUE)
plot(complementa_points)

map(
  'world',
  col = "grey",
  fill = TRUE,
  bg = "white",
  lwd = 0.05,
  mar = rep(0, 4),
  border = 0,
  ylim = c(-80, 80)
)
points(complementa_points)