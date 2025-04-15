#' ####################################################################### #
#' PROJECT: [BioDT CWR] 
#' CONTENTS: 
#'  - GBIF Data Download Functionality
#'  - Bioclimatic Variable Climatology creation for qsoil1 and qsoil2 combined
#'  DEPENDENCIES:
#'  - None
#' AUTHORS: [Erik Kusch, Eva Lieungh]
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


# CAPFITOGEN DATA DOWNLOAD ----------------------------------------------------
# Download the standard data from CAPFITOGEN for the globe.
FUN.DownCAPFITOGEN <-
  function(Dir = getwd(),
           Force = FALSE,
           resample_to_match = NULL) {
    # define a file name
    FNAME <- file.path("Data/Environment/capfitogen.nc")
    
    # check if file already exists and whether to overwrite
    if (!Force & file.exists(FNAME)) {
      capfitogen_rasters <- rast(FNAME)
      message(
        paste(
          FNAME,
          "exists already. It has been loaded from the disk.
         If you wish to override the present data, please specify Force = TRUE"
        )
      )
      return(capfitogen_rasters)
    }
    
    # if Force=TRUE or the file doesn't already exist:
    if (Force | !file.exists(FNAME)) {
      ## download data from Capfitogen Google drive ----
      message("Start downloading 10x10 data from Capfitogen google drive.")
      
      # scrape Capfitogen's google drive to get direct download links (rdatamaps/world/10x10)
      folder_id <- "1Xxt1ztTkLITAUbTyJzjePs2CpfETwF_u"
      embedded_url <- paste0("https://drive.google.com/embeddedfolderview?id=", 
                             folder_id, "#list")
      
      # Scrape the page
      page <- read_html(embedded_url)
      
      # Extract <a> tags (these contain filenames + links)
      file_links <- page %>% html_nodes("a") %>% html_attr("href")
      file_names <- page %>% html_nodes("a") %>% html_text()
      
      tif_files <- data.frame(name = file_names, link = file_links)
      
      # Extract file ID from link
      tif_files$file_id <- substr(tif_files$link, 
                                  start = 33, stop = 65)
      tif_files$download_url <- paste0("https://drive.google.com/uc?export=download&id=", tif_files$file_id)
      
      message("installing gdown with pip install")
      system("pip install gdown")
      
      # create directory to store tiff files
      dir.create(path = paste0(Dir, "/capfitogen"),
                 showWarnings = FALSE)
      
      # set long timeout to aoid interrupting downloads
      options(timeout = 1000)
      
      # download each file separately by google id
      for (i in 1:nrow(tif_files)) {
        file_name = tif_files$name[i]
        message(file_name)
        if (!file.exists(paste0(Dir, "/capfitogen/", tif_files$name[i]))){
        download.file(
          url = tif_files$download_url[i],
          destfile = paste0(Dir, "/capfitogen/", tif_files$name[i])
          )
        }
      }

      # list the downloaded files
      file_list <- list.files(paste0(Dir.Data.Envir, "/capfitogen"))
      message("downloaded files:")
      print(file_list)
      
      # read in and format rasters one by one from file name
      rasters <- NULL
      for (i in 1:length(file_list)) {
        file_path_i <- file.path(Dir.Data.Envir, "capfitogen", file_list[i])
        raster_i <- rast(file_path_i)
        # rename raster
        names(raster_i) <- tif_files$name[i]
        message(names(raster_i))
        
        # resample
        # if provided, resample to match another raster object's origin and resolution
        if (is.null(resample_to_match)) {
          message("No resampling.")
        } else if (inherits(resample_to_match, "SpatRaster")) {
          message(paste0("Resampling rasters to match ", names(resample_to_match)))
          
          resample_to_match <- rast(resample_to_match)
          
          ## project downloaded rasters to match resample_to_match file
          projection_to_match <- terra::crs(resample_to_match)
          
          terra::crs(raster_i) <- projection_to_match
          
          try(raster_i <- terra::resample(raster_i, resample_to_match))
        } else {
          stop("Invalid input for resample_to_match. Must be a SpatRaster or NULL.")
        }
      
        message(paste("adding raster", names(raster_i)))
        rasters <- c(rasters, raster_i)
      }
      
      typeof(rasters)
      str(rasters)      
      
      # save rasters as a NetCDF file
      message(paste0("saving as netCDF:", FNAME))
      terra::writeCDF(rasters, filename = FNAME, overwrite = FALSE)
      rasters
    }
  }

# WORLD DATABASE ON PROTECTED AREAS ---------------------------------------
#' UNEP-WCMC and IUCN (2025), Protected Planet: 
#' The World Database on Protected Areas (WDPA) [Online], 
#' Cambridge, UK: UNEP-WCMC and IUCN. Available at: www.protectedplanet.net.
#' https://www.protectedplanet.net/en/thematic-areas/wdpa
FUN.DownWDPA <- function(wdpa_url = FALSE,
                         wdpa_destination = file.path(Dir.Capfitogen.WDPA, "WDPA_Feb2025_Public_shp.zip"),
                         Force = FALSE) {
  # get the current month and year (from system time)
  MmmYYYY <- format(Sys.Date(), "%b%Y")

  # set a path to wdpa shapefiles
  wdpa_path <- file.path(Dir.Capfitogen.WDPA, "wdpa")

  # define the file name of global wdpa shapefile to be created
  FNAME <- file.path(wdpa_path, "global_wdpa_polygons.gpkg")

  # check if the final wdpa file already exists and whether to overwrite
  if (!Force & file.exists(FNAME)) {
    message(paste0("A global wdpa file with polygons exists already: ", FNAME))
  } else {
    if (!file.exists(wdpa_destination)) {
      # check if a wdpa url is provided
      if (wdpa_url == FALSE) {
        message("please provide a valid url for download.
            Download links change monthly, and follow the format
            https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_Apr2025_Public_shp.zip.
            See https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA")
      } else {
        # start download
        message("downloading zipped WDPA shapefiles, ca 4GB")
        # set long timeout to avoid interrupting download
        options(timeout = 10000)
        # download the zipped files
        download.file(
          url = wdpa_url,
          destfile = wdpa_destination,
          cacheOK = FALSE
        )
      }

      # unzip files
      message(paste("unzipping WDPA shapefiles to", Dir.Capfitogen.WDPA))
      unzip(zipfile = wdpa_destination, exdir = Dir.Capfitogen.WDPA)

      # unzip split shapefile downloads
      message("unzipping shapefiles split in download")

      shapefile_names <- c(
        paste0("WDPA_", MmmYYYY, "_Public_shp-points.cpg"),
        paste0("WDPA_", MmmYYYY, "_Public_shp-points.dbf"),
        paste0("WDPA_", MmmYYYY, "_Public_shp-points.prj"),
        paste0("WDPA_", MmmYYYY, "_Public_shp-points.shp"),
        paste0("WDPA_", MmmYYYY, "_Public_shp-points.shx"),
        paste0("WDPA_", MmmYYYY, "_Public_shp-polygons.cpg"),
        paste0("WDPA_", MmmYYYY, "_Public_shp-polygons.dbf"),
        paste0("WDPA_", MmmYYYY, "_Public_shp-polygons.prj"),
        paste0("WDPA_", MmmYYYY, "_Public_shp-polygons.shp"),
        paste0("WDPA_", MmmYYYY, "_Public_shp-polygons.shx")
      )

      shapefile_paths <- file.path(wdpa_path, shapefile_names)

      # loop over zip directories with parts of the global data (numbered 0, 1, 2)
      for (i in 0:2) {
        # define name of the current directory to be unzipped
        zipfilename <-
          file.path(
            Dir.Capfitogen.WDPA,
            paste0("WDPA_", MmmYYYY, "_Public_shp_", i, ".zip")
          )

        # unzip the directory containing shapefiles
        unzip(zipfile = zipfilename, exdir = wdpa_path)
        message(paste0("unzipped ", zipfilename, "\nto ", wdpa_path))

        # rename shapefiles with numbers to prevent overwriting them
        new_shapefile_names <- file.path(wdpa_path, paste0(i, "_", shapefile_names))
        file.rename(from = shapefile_paths, to = new_shapefile_names)
      }

      # delete unnecessary files
      message("deleting redundant files (translations etc.)")
      files_to_keep <- c(
        wdpa_path,
        wdpa_destination,
        file.path(
          Dir.Capfitogen.WDPA,
          paste0("WDPA_sources_", MmmYYYY, ".csv")
        )
      )

      files_to_delete <-
        list.files(Dir.Capfitogen.WDPA,
          full.names = TRUE
        )[list.files(Dir.Capfitogen.WDPA,
          full.names = TRUE
        ) %nin% files_to_keep]
      file.remove(files_to_delete, recursive = TRUE)

      # prepare list of shapefiles to be combined
      wdpa_polygon_shapefiles <-
        # list polygon shapefiles in WDPA directory
        substr(
          unique(sub(
            "\\..*", "",
            list.files(wdpa_path)[grep(
              pattern = "polygon",
              x = shapefile_names
            )]
          )),
          3, 34
        )

      shapefile_list <- list()

      for (i in 0:2) {
        # read in all the polygon shapefile layers
        layer_name <- paste0(i, "_", wdpa_polygon_shapefiles)
        shapefile_list[[i + 1]] <-
          read_sf(dsn = wdpa_path, layer = layer_name)
      }

      # merge parts into one global shapefile
      message("combining parts of the WDPA shapefile. This can take a while ---")
      wdpa <- do.call(rbind, shapefile_list)
      message("Complete WDPA successfully combined.")

      # wdpa$WDPAID <- as.character(wdpa$WDPAID)
      # wdpa$text_field <- iconv(wdpa$text_field, to = "ASCII//TRANSLIT")

      # save complete wdpa
      message("save as GeoPackage")
      st_write(wdpa, file.path(wdpa_path, "global_wdpa_polygons.gpkg"))
      message(paste0(
        "global WDPA saved as: ",
        file.path(wdpa_path, "global_wdpa_polygons.gpkg")
      ))

      message("save as shapefile")
      # st_write(wdpa, FNAME)
      st_write(
        wdpa,
        "global_wdpa_polygons.shp",
        layer_options = "ENCODING=UTF-8",
        field_type = c(WDPAID = "Character")
      )
      message("global WDPA saved as global_wdpa_polygons.shp")
    }
  }
}

# end
