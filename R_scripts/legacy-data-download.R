# Legacy code for downloading custom edaphic (soil) and geophysical data

# by Eva Lieungh

# this scripts defines functions in the same style as in SHARED-Data.R.
# Example use:
# 
# ### Edaphic data ------ 
# ## NB! each file at 250x250m is ~20GB...
# message("Downloading new or loading existing edaphic/soil variables")
# 
# edaphic_variables <- FUN.DownEV(
#   Dir = Dir.Data.Envir,
#   target_resolution = c(250, 250),
#   Force = FALSE,
#   resample_to_match = bioclim_variables[[1]]
# )
# 
# ### Geophysical data ------
# message("Downloading new or loading existing geophysical variables")
# geophysical_variables <- FUN.DownGV(
#   Dir = Dir.Data.Envir,
#   Force = FALSE,
#   resample_to_match = bioclim_variables[[1]]
# )


# EDAPHIC DATA DOWNLOAD -------------------------------------------------------
# NB! Works for some variables, but the data set is incomplete. 
# This function should be modified to add or replace data for capfitogen.
FUN.DownEV <-
  function(Dir = getwd(), # where to store the data output on disk
           target_resolution = c(250, 250),
           Force = FALSE, # do not overwrite already present data,
           resample_to_match = FALSE) {
    # define a file name
    FNAME <- file.path(Dir, "edaphic.nc")
    message(FNAME)
    
    # check if file already exists and whether to overwrite
    if (!Force & file.exists(FNAME)) {
      EV_ras <- rast(FNAME)
      message(
        "Data has already been downloaded. It has been loaded from the disk. If you wish to override the present data, please specify Force = TRUE"
      )
      return(EV_ras)
    }
    
    # if Force=TRUE or the file doesn't already exist:
    if (Force | !file.exists(FNAME)) {
      ## download data from SoilGrids ----
      message("Start downloading data from SoilGrids: files.isric.org/soilgrids/latest/data/")
      soilGrids_url = "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"
      
      #' overview of datasets: https://www.isric.org/explore/soilgrids/faq-soilgrids#What_do_the_filename_codes_mean
      #' NB! Each global map occupies circa 20 GB for 250x20m resolution!
      #' It takes a while to download.
      #' In addition, https://files.isric.org/soilgrids/latest/data/wrb/
      #' has maps of soil types, as estimated probability of occurrence per type.
      #' MostProbable.vrt has the most probable soil type per gridcell.
      #' Soil salinity: https://data.isric.org/geonetwork/srv/eng/catalog.search#/metadata/c59d0162-a258-4210-af80-777d7929c512
      
      SoilGrids_variables_in <-
        c("bdod/bdod_0-5cm_mean", # Bulk density of the fine earth fraction, cg/cm³
          "cec/cec_0-5cm_mean",   # Cation Exchange Capacity of the soil, 	mmol(c)/kg
          "cfvo/cfvo_0-5cm_mean", # Volumetric fraction of coarse fragments (> 2 mm) 	cm3/dm3 (vol‰)
          "silt/silt_0-5cm_mean", # Proportion of silt particles (≥ 0.002 mm and ≤ 0.05/0.063 mm) in the fine earth fraction 	g/kg
          "clay/clay_0-5cm_mean", # Proportion of clay particles (< 0.002 mm) in the fine earth fraction 	g/kg
          "sand/sand_0-5cm_mean", # Proportion of sand particles (> 0.05/0.063 mm) in the fine earth fraction 	g/kg
          "nitrogen/nitrogen_0-5cm_mean", # Total nitrogen (N) 	cg/kg
          "phh2o/phh2o_0-5cm_mean", # Soil pH 	pHx10
          "ocd/ocd_0-5cm_mean",   # Organic carbon density 	hg/m³
          "ocs/ocs_0-30cm_mean",  # Organic carbon stocks 	t/ha
          "soc/soc_0-5cm_mean")   # Soil organic carbon content in the fine earth fraction 	dg/kg
      
      SoilGrids_variables <- sub(".*/", "", SoilGrids_variables_in)
      
      soilGrids_data <- NULL
      
      for (i in 1:length(SoilGrids_variables_in)) {
        variable_name = SoilGrids_variables[i]
        
        message(SoilGrids_variables[i])
        
        path_to_downloaded_file <- paste0(Dir.Data.Envir, "/",
                                          SoilGrids_variables[i], ".tif")
        
        # if variable is not downloaded already, ...
        ifelse(
          !file.exists(path_to_downloaded_file),
          # download it, ...
          downloaded_variable <- gdalUtilities::gdal_translate(
            src_dataset = paste0(soilGrids_url,
                                 SoilGrids_variables_in[i], ".vrt"),
            dst_dataset = path_to_downloaded_file,
            tr = target_resolution # target resolution
          ),
          # or else load it from file
          downloaded_variable <- path_to_downloaded_file
        )
        
        ## load variable as raster
        downloaded_raster <- rast(downloaded_variable)
        plot(downloaded_raster, main = SoilGrids_variables[i])
        
        ### resample ----
        ## if provided, resample to match another raster object's origin and resolution
        if (!missing(resample_to_match)) {
          message(paste0("resampling raster to match ", names(resample_to_match)))
          resample_to_match <- rast(resample_to_match)
          
          ## project downloaded rasters to match resample_to_match file
          projection_to_match <- terra::crs(resample_to_match)
          terra::crs(downloaded_raster) <- projection_to_match
          
          ## resample
          downloaded_raster <-
            terra::resample(downloaded_raster,
                            resample_to_match)
        }
        
        soilGrids_data <- c(soilGrids_data, downloaded_raster)
        
      }
      
      ## HSWD downloads ----
      ## download additional rasters from HSWD
      message("Downloading data from HSWD (harmonised world soil database) via fao.org")
      
      path_to_PH_nutrient = file.path(Dir, "HSWD_PH_nutrient.tif")
      if (!file.exists(path_to_PH_nutrient)) {
        download.file(url = "https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq1.asc",
                      destfile = path_to_PH_nutrient)
      }
      PH_nutrient <- rast(path_to_PH_nutrient)
      
      path_to_PH_toxicity = file.path(Dir, "HSWD_PH_toxicity.tif")
      if (missing(path_to_PH_toxicity)) {
        message("downloading HSWD PH toxicity")
        download.file(url = "https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq6.asc",
                      destfile = path_to_PH_toxicity)
      }
      PH_toxicity <- rast(path_to_PH_toxicity)
      
      ### resample ----
      ## if provided, resample to match another raster object's origin and resolution
      if (!missing(resample_to_match)) {
        message(paste0("resampling raster to match ", names(resample_to_match)))
        resample_to_match <- rast(resample_to_match)
        
        ## project downloaded rasters to match resample_to_match file
        projection_to_match <- terra::crs(resample_to_match)
        terra::crs(PH_nutrient) <- projection_to_match
        terra::crs(PH_toxicity) <- projection_to_match
        
        ## resample
        PH_nutrient <-
          terra::resample(PH_nutrient,
                          resample_to_match)
        
        PH_toxicity <-
          terra::resample(PH_toxicity,
                          resample_to_match)
        
      }
      
      ### combine and rename rasters ----
      EV_rasters <- rast(c(soilGrids_data,
                           PH_nutrient, PH_toxicity))
      
      names(EV_rasters) <- c(SoilGrids_variables,
                             "Nutrient", "Toxicity")
      
      ### Saving ----
      message(paste0("saving as netCDF:", FNAME))
      terra::writeCDF(EV_rasters,
                      filename = FNAME,
                      overwrite = FALSE)
      
      EV_rasters
      
    }
  }

# GEOPHYSICAL DATA DOWNLOAD --------------------------------------------------
#' define a function to load existing data or download them to
#' the given directory. 
FUN.DownGV <-
  function(Dir = getwd(),# where to store the data output on disk
           Force = FALSE,# do not overwrite already present data,
           resample_to_match = FALSE) {
    # define a file name
    FNAME <- file.path(Dir, "geophysical.nc")
    
    # check if file already exists and whether to overwrite
    if (!Force & file.exists(FNAME)) {
      GV_raster <- rast(FNAME)
      names(GV_raster) <- c("elevation",
                            "mean_wind_speed_of_windiest_month")
      message(
        "Data has already been downloaded with these specifications. It has been loaded from the disk. If you wish to override the present data, please specify Force = TRUE"
      )
      
      return(GV_raster)
      
    }
    
    # if the file doesn't already exist:
    if (Force | !file.exists(FNAME)) {
      message("downloading or loading geophysical data")
      
      ## Download digital elevation model (DEM) ------
      ##' Fick, S.E. and R.J. Hijmans, 2017. WorldClim 2
      ##' https://doi.org/10.1002/joc.5086
      message("- digital elevation model")
      if (!file.exists(paste0(Dir, "/wc2.1_2.5m_elev.tif"))) {
        worldclim_dem_url = "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_2.5m_elev.zip"
        temp <- tempfile()
        download.file(worldclim_dem_url, temp)
        unzip(zipfile = temp,
              exdir = Dir)
        unlink(temp)
      }
      dem <- rast(paste0(Dir, "/wc2.1_2.5m_elev.tif"))
      names(dem) <- "Elevacion"
      
      ## Download wind speed ------
      ##' WorldClim 2
      message("- mean wind speed of windiest month (annual max of monthly means)")
      if (!file.exists(paste0(Dir, "/wc2.1_2.5m_wind_max.tif"))) {
        worldclim_wind_url = "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_2.5m_wind.zip"
        temp <- tempfile()
        download.file(worldclim_wind_url, temp)
        unzip(zipfile = temp,
              exdir = Dir)
        unlink(temp)
        
        ## read monthly wind speed and find annual max of monthly means
        month_numbers = c(paste0("0", 2:9), as.character(10:12))
        wind_stack <- rast(paste0(Dir, "/wc2.1_2.5m_wind_01.tif"))
        for (i in month_numbers) {
          raster_i = rast(paste0(Dir, "/wc2.1_2.5m_wind_", i, ".tif"))
          wind_stack <- c(wind_stack, raster_i)
        }
        max_wind <- terra::app(wind_stack, max)
        writeRaster(max_wind,
                    filename = paste0(Dir, "/wc2.1_2.5m_wind_max.tif"))
      }
      wind <- rast(paste0(Dir, "/wc2.1_2.5m_wind_max.tif"))
      names(wind) <- "mean_wind_speed_of_windiest_month"
      
      ## Resample ------
      ## if provided, resample to match another raster object's origin and resolution
      if (!missing(resample_to_match)) {
        message(paste0("resampling raster to match ", names(resample_to_match)))
        resample_to_match <- rast(resample_to_match)
        
        ## project downloaded rasters to match resample_to_match file
        projection_to_match <- terra::crs(resample_to_match)
        terra::crs(dem) <- projection_to_match
        terra::crs(wind) <- projection_to_match
        message("projected to match input")
        
        ## resample
        dem <- terra::resample(dem,
                               resample_to_match)
        message("dem successfully resampled")
        
        wind <- terra::resample(wind,
                                resample_to_match)
        message("wind successfully resampled")
      }
      
      ### combine rasters
      geophysical_rasters <- c(dem, wind)
      
      ## Saving ----
      message("saving as NetCDF")
      terra::writeCDF(geophysical_rasters,
                      filename = FNAME,
                      overwrite = TRUE)
      
      geophysical_rasters
    }
  }

# WGS84 = EPSG:4326
## Download digital elevation model (DEM) from 
##' Jarvis A., H.I. Reuter, A. Nelson, E. Guevara, 2008, Hole-filled 
##' seamless SRTM data V4, International Centre for Tropical Agriculture 
##' (CIAT), available from http://srtm.csi.cgiar.org.
#dem <- rast("https://srtm.csi.cgiar.org/wp-content/uploads/files/250m/tiles250m.jpg")

