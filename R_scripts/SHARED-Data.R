#' ####################################################################### #
#' PROJECT: [BioDT CWR] 
#' CONTENTS: 
#'  - GBIF Data Download Functionality
#'  - Bioclimatic Variable Climatology creation for qsoil1 and qsoil2 combined
#'  DEPENDENCIES:
#'  - None
#' AUTHOR: [Erik Kusch]
#' ####################################################################### #

# GBIF DOWNLOAD FUNCTION --------------------------------------------------
#' queries download from GBIF, handles and cleans data, 
#' returns SF MULTIPOINT object and GBIF download metadata
FUN.DownGBIF <- function(
    species = NULL, # species name as character for whose genus data is to be downloaded
		Dir = getwd(), # where to store the data
		Force = FALSE, # overwrite existing data?
		Mode = "ModGP", # which specification to run, either for whole GENUS of supplied species (ModGP), or for species directly (Capfitogen)
		parallel = 1 # an integer, 1 = sequential; always defaults to sequential when Mode == "Capfitogen"
){
	## Preparing species name identifiers
	input_species <- species

	## Focusing on Genus-part of the name if Mode is set to ModGP
	if(Mode == "ModGP"){
		species <- strsplit(input_species, " ")[[1]][1]
	}
	
	## File name and data presence check
	FNAME <- file.path(Dir, paste0(species, ".RData"))
	if(!Force & file.exists(FNAME)){
		save_ls <- loadObj(FNAME)
		message("Data has already been downloaded with these specifications. It has been loaded from the disk. \nIf you wish to override the present data, please specify Force = TRUE")
		return(save_ls)
	}
	
	## Function start
	message("Starting GBIF data retrieval")
  
	## GBIF taxa Query ----
	if(Mode == "ModGP"){
		message(paste("## Resolving", species, "at genus level"))
		RankGBIF <- "genus"
	}
	if(Mode == "Capfitogen"){
		message(paste("## Resolving", species, "at species level"))
		RankGBIF <- "species"
	}
	GBIF_match <- name_backbone(name = species, 
															rank = RankGBIF, 
															kingdom = "plante")
	
	## Extracting taxonkey
	tax_ID <- ifelse(GBIF_match$rank != toupper(RankGBIF), NA, 
									 GBIF_match$usageKey[GBIF_match$rank == toupper(RankGBIF)])
	
	## checking GBIF match success
	if(is.na(tax_ID)){
		print(GBIF_match)
		stop("The provided species name could not be resolved at GBIF. Please find the match at GBIF above.")
	}
	
	## GBIF Data Query ----
	message(paste("## Downloading", 
								occ_count(taxonKey = tax_ID, 
													hasCoordinate = TRUE,
													occurrenceStatus = "PRESENT"), 
								"GBIF records"))
	occ_down <- occ_download(pred("taxonKey", tax_ID), 
													 pred("hasCoordinate", TRUE),
													 pred("occurrenceStatus", "PRESENT"),
													 format = "SIMPLE_CSV")
	curlopts <- list(http_version = 2) # needed on Mac to avoid HTTP issues in next line (see here: https://github.com/ropensci/rgbif/issues/579)
	occ_meta <- occ_download_wait(occ_down, 
	                              status_ping = 30, 
																curlopts = list(), 
																quiet = FALSE) # wait for download to finish
	occ_get <- occ_download_get(occ_down, path = Dir) # download data
	curlopts <- list(http_version = 1) # resetting this to not affect other functions
	
	## Data Loading ----
	message("Loading GBIF Data into R")
	occ_occ <- occ_download_import(occ_get) # import downloaded data
	
	## Manipulating GBIF Data ----
	### Resolving Common Issues ----
	message("Resolving Common Data Issues. Removing occurrences")
	## removing bases of record that may not be linked to coordinates properly
	message("... that may not be linked to coordinates properly")
	occ_occ <- occ_occ[occ_occ$basisOfRecord %nin% c("PRESERVED_SPECIMEN", 
	                                                 "MATERIAL_CITATION"), ]
	## removing highly uncertain locations, i.e., anything more than 1km in uncertainty
	message("... with >1km uncertainty")
	occ_occ <- occ_occ[occ_occ$coordinateUncertaintyInMeters <= 1000, ]
	## removing rounded coordinates
	message("... with rounded coordinates")
	occ_occ <- occ_occ[-grep(occ_occ$issue, 
	                         pattern = "COORDINATE_ROUNDED"), ]
	## removing empty species rows
	message("... with empty species rows")
	occ_occ <- occ_occ[occ_occ$species != "" & !is.na(occ_occ$species), ]
	
	### Parallel Set-Up ----
	if(parallel == 1 | Mode == "Capfitogen"){parallel <- NULL} # no parallelisation
	### This needs to be commented back in when wanting to run code below directly
	if(!is.null(parallel) && !RUNNING_ON_LUMI){ # parallelisation
		message("Registering cluster for parallel processing")
		print("Registering cluster")
		parallel <- parallel::makeCluster(parallel)
		on.exit(stopCluster(parallel))
		print("R Objects loading to cluster")
		parallel::clusterExport(parallel, 
		                        varlist = c(
		                          "package_vec", 
			                        "install.load.package",
			                        "occ_occ"), 
		                        envir = environment())
		print("R Packages loading on cluster")
		clusterpacks <- clusterCall(parallel, 
		                            function() sapply(package_vec, 
		                                              install.load.package))
	}
	
	### Making SF for species ----
	message("Extracting species-level data into MULTIPOINT objects")
	GBIF_specs <- unique(occ_occ$species)
	
	##' Making a list of spatialfeatures MULTIPOINT objects 
	##' denoting unique locations of presence per species
	specs_ls <- pblapply(
	  GBIF_specs,
	  cl = parallel,
	  FUN = function(x) {
	    ## dataframe of occurrences
	    spec_df <- occ_occ[occ_occ$species == x,]
	    ## unique locations
	    spec_uniloca <- occ_occ[occ_occ$species == x,
	                            c("species",
	                              "decimalLatitude",
	                              "decimalLongitude")]
	    ## remove duplicates (of populations)
	    spec_df <- spec_df[!duplicated(spec_uniloca),
	                       c("gbifID",
	                         "datasetKey",
	                         "occurrenceID",
	                         "species",
	                         "scientificName",
	                         "speciesKey",
	                         "decimalLatitude",
	                         "decimalLongitude",
	                         "coordinateUncertaintyInMeters",
	                         "eventDate",
	                         "basisOfRecord",
	                         "recordNumber",
	                         "issue"
	                       )]
	    spec_df$presence <- 1
	    st_as_sf(spec_df,
	             coords = c("decimalLongitude",
	                        "decimalLatitude"))
	  }
	)
	names(specs_ls) <- GBIF_specs
	
	## Making list into single data frame when Capfitogen mode is toggled on.
	# HJ: section below to create a Capfitogen data frame not used
	# species data included as the sf file created above
	if(Mode == "Capfitogen"){
	  message("Making data for Capfitogen mode")
	  message(FNAME)
		specs_ls <- specs_ls[[1]]
		## create capfitogen data frame
		CapfitogenColumns <- c("INSTCODE", 
		                       "ACCENUMB", 
		                       "COLLNUMB", 
		                       "COLLCODE", 
		                       "COLLNAME", 
		                       "COLLINSTADDRESS", 
		                       "COLLMISSID", 
		                       "GENUS", 
		                       "SPECIES", 
		                       "SPAUTHOR", 
		                       "SUBTAXA", 
		                       "SUBTAUTHOR", 
		                       "CROPNAME", 
		                       "ACCENAME", 
		                       "ACQDATE", 
		                       "ORIGCTY", 
		                       "NAMECTY", 
		                       "ADM1", 
		                       "ADM2", 
		                       "ADM3", 
		                       "ADM4", 
		                       "COLLSITE", 
		                       "DECLATITUDE", 
		                       "LATITUDE", 
		                       "DECLONGITUDE", 
		                       "LONGITUDE", 
		                       "COORDUNCERT", 
		                       "COORDDATUM", 
		                       "GEOREFMETH", 
		                       "ELEVATION", 
		                       "COLLDATE", 
		                       "BREDCODE", 
		                       "BREDNAME", 
		                       "SAMPSTAT", 
		                       "ANCEST", 
		                       "COLLSRC", 
		                       "DONORCODE", 
		                       "DONORNAME", 
		                       "DONORNUMB", 
		                       "OTHERNUMB", 
		                       "DUPLSITE", 
		                       "DUPLINSTNAME", 
		                       "STORAGE", 
		                       "MLSSTAT", 
		                       "REMARKS")
		CapfitogenData <- data.frame(matrix(data = NA, 
		                                    nrow = nrow(specs_ls), 
		                                    ncol = length(CapfitogenColumns)))
		colnames(CapfitogenData) <- CapfitogenColumns
		## Create unique rownames for the ACCENUMB
		CapfitogenData$ACCENUMB <- seq(from = 1, 
		                               to = nrow(CapfitogenData), 
		                               by = 1)
		## Add in the species, latitude and longitude (nothing else at this point)
		CapfitogenData$SPECIES <- specs_ls$species
		CapfitogenData$DECLATITUDE <- st_coordinates(specs_ls)[,"Y"]
		CapfitogenData$DECLONGITUDE <- st_coordinates(specs_ls)[,"X"]
		specs_ls_capfitogen <- CapfitogenData
	}
	
	### Returning Object to Disk and Environment ----
	ifelse(Mode == "Capfitogen",
	       occs = specs_ls_capfitogen,
	       occs = specs_ls)
	save_ls <- list(meta = occ_meta,
									occs = occs
									# json = JSON_ls
									)
	
	saveObj(save_ls, file = FNAME)
	unlink(occ_get) # removes .zip file
	
	### JSON RO-CRATE creation ----
	message("Create .json RO-crate (research object) metadata")
	JSON_ls <- jsonlite::read_json("ro-crate-metadata.json")
	
	JSON_ls$`@graph`[[2]]$hasPart[[1]]$`@id` <- basename(FNAME)
	JSON_ls$`@graph`[[2]]$about[[1]]$`@id` <- paste0("https://www.gbif.org/species/", 
	                                                 tax_ID) # gbif taxon ID
	JSON_ls$`@graph`[[2]]$creator$`@id` <- c(JSON_ls$`@graph`[[2]]$creator$`@id`, 
	                                         as.character(options("gbif_email")))
	JSON_ls$`@graph`[[2]]$author$`@id` <- c(JSON_ls$`@graph`[[2]]$author$`@id`, 
	                                        as.character(options("gbif_email")))
	JSON_ls$`@graph`[[2]]$datePublished <- Sys.time()
	JSON_ls$`@graph`[[2]]$name <- paste("Cleaned GBIF occurrence records for", 
	                                    RankGBIF, species)
	JSON_ls$`@graph`[[2]]$keywords <- list("GBIF", 
	                                       "Occurrence", 
	                                       "Biodiversity", 
	                                       "Observation", 
	                                       Mode)
	JSON_ls$`@graph`[[2]]$description <- paste(Mode, 
	                                           "input data for", 
	                                           species)
	
	JSON_ls$`@graph`[[3]]$name <- basename(FNAME)
	JSON_ls$`@graph`[[3]]$contentSize <- file.size(FNAME)
	JSON_ls$`@graph`[[3]]$encodingFormat <- "application/RData"
	JSON_ls$`@graph`[[3]]$`@id` <- basename(FNAME)

	JSON_ls$`@graph`[[4]]$name <- c(as.character(options("gbif_user")), 
	                                JSON_ls$`@graph`[[4]]$name)
	JSON_ls$`@graph`[[4]]$`@id` <- c(JSON_ls$`@graph`[[4]]$`@id`, 
	                                 as.character(options("gbif_email")))
	JSON_ls$`@graph`[[4]]$`@type` <- c(JSON_ls$`@graph`[[4]]$`@type`, 
	                                   "Organisation")
	
	JSON_ls$`@graph`[[5]]$agent$`@id` <- c(JSON_ls$`@graph`[[5]]$agent$`@id`, 
	                                       as.character(options("gbif_email")))

	JSON_ls$`@graph`[[5]]$instrument$`@id` <- "https://github.com/BioDT/uc-CWR"
	
	con <- file(file.path(Dir, paste0(species, ".json")))
	writeLines(jsonlite::toJSON(JSON_ls, pretty = TRUE), con)
	close(con)
	
	save_ls
}

# BIOCLIMATIC DATA DOWNLOAD --------------------------------------------
#' queries, downloads, and computes bioclimatic variables 
#' at global extent from ERA5-Land. Water availability is based on 
#' soil moisture level 1 (0-7cm) and 2 (7-28cm)
FUN.DownBV <- function(
    T_Start = 1970, # what year to begin climatology calculation in
		T_End = 2000, # what year to end climatology calculation in
		Dir = getwd(), # where to store the data output on disk
		Force = FALSE # do not overwrite already present data
) {
  # Workaround for Dir, 1/2
  # original_wd = getwd()
  # setwd(Dir)
  # End of workaround
  
  FNAME <- file.path(Dir, paste0("BV_", T_Start, "-", T_End, ".nc"))
  
  if (!Force & file.exists(FNAME)) {
    BV_ras <- stack(FNAME)
    names(BV_ras) <- paste0("BIO", 1:19)
    message(
      "Data has already been downloaded with these specifications.
		        It has been loaded from the disk. If you wish to override
		        the present data, please specify Force = TRUE"
    )
    return(BV_ras)
  }
  
  ### Raw soil moisture level data ----
  #' We download raw soil moisture data for
  #' layers 1 (0-7cm) and 2 (7-28cm) separately.
  #' These are then summed up and used in the
  #' bioclimatic variable computation of KrigR
  FNAME_RAW <-
    file.path(
      Dir,
      paste(
        tools::file_path_sans_ext(basename(FNAME)),
        "volumetric_soil_water_layer_1",
        "RAW.nc",
        sep = "_"
      )
    )
  if (!file.exists(FNAME_RAW)) {
    #### Downloading ----
    Qsoil1_ras <- CDownloadS(
      Variable = "volumetric_soil_water_layer_1", # could also be total_precipitation
      DataSet = "reanalysis-era5-land-monthly-means",
      Type = "monthly_averaged_reanalysis",
      DateStart = paste0(T_Start, "-01-01 00:00"),
      DateStop = paste0(T_End, "-12-31 23:00"),
      TResolution = "month",
      Dir = Dir,
      Extent = ne_countries(type = "countries", scale = "medium")[, 1],
      FileName = "Qsoil1",
      API_User = API_User,
      API_Key = API_Key
    )
    
    Qsoil2_ras <- CDownloadS(
      Variable = "volumetric_soil_water_layer_2",
      # could also be total_precipitation
      DataSet = "reanalysis-era5-land-monthly-means",
      Type = "monthly_averaged_reanalysis",
      DateStart = paste0(T_Start, "-01-01 00:00"),
      DateStop = paste0(T_End, "-12-31 23:00"),
      TResolution = "month",
      Dir = Dir,
      Extent = ne_countries(type = "countries", scale = "medium")[, 1],
      FileName = "Qsoil2",
      API_User = API_User,
      API_Key = API_Key
    )
    
    #### Combining ----
    QSoilCombin_ras <- Qsoil1_ras + Qsoil2_ras
    
    #### Saving ----
    terra::metags(QSoilCombin_ras) <- terra::metags(Qsoil1_ras)
    QSoilCombin_ras <- KrigR:::Meta.NC(
      NC = QSoilCombin_ras,
      FName = FNAME_RAW,
      Attrs = terra::metags(QSoilCombin_ras),
      Write = TRUE,
      Compression = 9
    )
    
    ### Deleting unnecessary files ----
    unlink(list.files(Dir, pattern = "Qsoil", full.names = TRUE))
  }
  
  ### Bioclimatic data ----
  BV_ras <- BioClim(
    Temperature_Var = "2m_temperature",
    Temperature_DataSet = "reanalysis-era5-land",
    Temperature_Type = NA,
    Water_Var = "volumetric_soil_water_layer_1", # could also be total_precipitation
    Water_DataSet = "reanalysis-era5-land-monthly-means",
    Water_Type = "monthly_averaged_reanalysis",
    Y_start = T_Start,
    Y_end = T_End,
    Extent = ne_countries(type = "countries", scale = "medium")[, 1],
    Dir = Dir,
    FileName = basename(FNAME),
    FileExtension = ".nc",
    Compression = 9,
    API_User = API_User,
    API_Key = API_Key
  )
  
  ### JSON RO-CRATE creation ----
  JSON_ls <- jsonlite::read_json("ro-crate-metadata.json")
  
  JSON_ls$`@graph`[[2]]$hasPart[[1]]$`@id` <- basename(FNAME)
  JSON_ls$`@graph`[[2]]$about[[1]]$`@id` <-
    "https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land"
  JSON_ls$`@graph`[[2]]$datePublished <-
    Sys.time() # tail(file.info(FNAME)$ctime)
  JSON_ls$`@graph`[[2]]$name <-
    "Bioclimatic data obtained from ERA5-Land. Water avialbility is denoted via the sum of soil moisture layer 1 and 2."
  JSON_ls$`@graph`[[2]]$keywords <- list("ERA5-Land",
                                         "ECMWF",
                                         "Bioclimatic Variables",
                                         "Soil Moisture")
  JSON_ls$`@graph`[[2]]$description <-
    "Bioclimatic data obtained from ERA5-Land. Water avialbility is denoted via the sum of soil moisture layer 1 and 2."
  
  JSON_ls$`@graph`[[3]]$name <- basename(FNAME)
  JSON_ls$`@graph`[[3]]$contentSize <- file.size(FNAME)
  JSON_ls$`@graph`[[3]]$`@id` <- basename(FNAME)
  
  JSON_ls$`@graph`[[5]]$instrument$`@id` <-
    "https://doi.org/10.1088/1748-9326/ac48b3"
  
  con <- file(file.path(Dir,
                        paste0(
                          tools::file_path_sans_ext(basename(FNAME)),
                          ".json"
                        )))
  writeLines(jsonlite::toJSON(JSON_ls, pretty = TRUE),
             con)
  close(con)
  
  message(
    paste0(
      "ERA5 citation:\nCopernicus Climate Change Service, Climate Data Store, (2024): ERA5-land post-processed daily-statistics from 1950 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS), DOI: 10.24381/cds.e9c9c792 ",
      "(Accessed on ", Sys.Date(),")")
  )
  
  # Workaround for Dir, 2/2
  #	setwd(original_wd)
  # End of workaround
  
  BV_ras
}

# EDAPHIC DATA DOWNLOAD -------------------------------------------------------
# INCOMPLETE! Works for some variables, but the data set is incomplete.
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
          "cec/cec_0-5cm_mean")#, # Cation Exchange Capacity of the soil, 	mmol(c)/kg
      #"cfvo/cfvo_0-5cm_mean", # Volumetric fraction of coarse fragments (> 2 mm) 	cm3/dm3 (vol‰)
      #"silt/silt_0-5cm_mean")#, # Proportion of silt particles (≥ 0.002 mm and ≤ 0.05/0.063 mm) in the fine earth fraction 	g/kg
      #"clay/clay_0-5cm_mean", # Proportion of clay particles (< 0.002 mm) in the fine earth fraction 	g/kg
      #"sand/sand_0-5cm_mean", # Proportion of sand particles (> 0.05/0.063 mm) in the fine earth fraction 	g/kg
      #"nitrogen/nitrogen_0-5cm_mean", # Total nitrogen (N) 	cg/kg
      #"phh2o/phh2o_0-5cm_mean", # Soil pH 	pHx10
      #"ocd/ocd_0-5cm_mean",# Organic carbon density 	hg/m³
      #"ocs/ocs_0-30cm_mean",# Organic carbon stocks 	t/ha
      #"soc/soc_0-5cm_mean")# Soil organic carbon content in the fine earth fraction 	dg/kg
      
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
      names(dem) <- "elevation"
      
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
      
      ## resample ------
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
      
      ### Saving ----
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

# DRAFT: Google Earth Engine downloads. -------------------------------------
#' Almost working, but missing user/project credentials and login. 
#' See https://developers.google.com/earth-engine/guides/auth
#' ee.Authenticate()
#' ee.Initialize(project='my-project')
# 
# install.packages("reticulate") # python environment - https://rstudio.github.io/reticulate/
# install.packages("rgeedim") # search and download Google Earth Engine imagery with Python
# 
# library(reticulate)
# 
# virtualenv_create(envname = "uc_CWR", # saved under /Documents/.virtualenvs/uc_CWR
#                   packages = c("numpy","geedim"),
#                   python = "C:/Program Files/Python3.10/python.exe"
# )
# 
# virtualenv_list()
# 
# use_virtualenv("uc_CWR")
# 
# library(rgeedim)
# 
# names(geedim()$enums)
# 
# 
# ## Download CHILI: Continuous Heat-Insolation Load Index
# ##' Theobald, D. M., Harrison-Atlas, D., Monahan, W. B., & Albano, C. M. 
# ##' (2015). Ecologically-relevant maps of landforms and physiographic 
# ##' diversity for climate adaptation planning. PloS one, 10(12), e0143619
# 
# chili_img_id <- gd_image_from_id('CSP/ERGo/1_0/Global/ALOS_CHILI')
# 
# chili <-  
#   gd_download(chili_img_id,
#     filename = 'chili.tif',
#     resampling = "bilinear",
#     scale = 2500, # scale=10: request ~10m resolution
#     overwrite = TRUE,
#     silent = FALSE
#   )
