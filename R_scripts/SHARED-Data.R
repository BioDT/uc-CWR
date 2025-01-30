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
# queries download from GBIF, handles and cleans data, returns SF MULTIPOINT object and GBIF download metadata
FUN.DownGBIF <- function(species = NULL, # species name as character for whose genus data is to be downloaded
												 Dir = getwd(), # where to store the data
												 Force = FALSE, # overwrite existing data?
												 Mode = "ModGP", # which specification to run, either for whole GENUS of supplied species (ModGP), or for species directly (Capfitogen)
												 parallel = 1 # an integer, 1 = sequential; always defaults to sequential when Mode == "Capfitogen"
){
	## Preparing species name identifiers
	input_species <- species
	
	## Focussing on Genus-part of the name if Mode is set to ModGP
	if(Mode == "ModGP"){
		species <- strsplit(input_species, " ")[[1]][1]
	}
	
	## Filename and data presence check
	FNAME <- file.path(Dir, paste0(species, ".RData"))
	if(!Force & file.exists(FNAME)){
		save_ls <- loadObj(FNAME)
		message("Data has already been downloaded with these specifications previously. It has been loaded from the disk. If you wish to override the present data, please specify Force = TRUE")
		return(save_ls)
	}
	
	## Function start
	message("Starting GBIF data retrieval")
	## GBIF ID Query ----
	## GBIF query
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
	occ_meta <- occ_download_wait(occ_down, status_ping = 30, 
																curlopts = list(), quiet = FALSE) # wait for download to finish
	occ_get <- occ_download_get(occ_down, path = Dir) # download data
	curlopts <- list(http_version = 1) # resetting this to not affect other functions
	
	## Data Loading ----
	message("Loading GBIF Data into R")
	occ_occ <- occ_download_import(occ_get) # import downloaded data
	
	## Manipulating GBIF Data ----
	### Resolving Common Issues ----
	message("Resolving Common Data Issues")
	## removing bases of record that may not be linked to coordinates properly
	occ_occ <- occ_occ[occ_occ$basisOfRecord %nin% c("PRESERVED_SPECIMEN", 
	                                                 "MATERIAL_CITATION"), ]
	## removing highly uncertain locations, i.e., anything more than 1km in uncertainty
	occ_occ <- occ_occ[occ_occ$coordinateUncertaintyInMeters <= 1000, ]
	## removing rounded coordinates
	occ_occ <- occ_occ[-grep(occ_occ$issue, 
	                         pattern = "COORDINATE_ROUNDED"), ]
	## removing empty species rows
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
	
	## Making a list of spatialfeatures MULTIPOINT objects denoting unique locations of presence per species
	specs_ls <- pblapply(GBIF_specs, 
											 cl = parallel,
											 FUN = function(x){
											 	spec_df <- occ_occ[occ_occ$species == x, ]
											 	spec_uniloca <- occ_occ[occ_occ$species == x, 
											 	                        c("species", 
											 	                          "decimalLatitude",
											 	                          "decimalLongitude")]
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
											 										   "issue")]   											 	
											 	spec_df$presence <- 1
											 	st_as_sf(spec_df, 
											 	         coords = c("decimalLongitude", 
											 	                    "decimalLatitude"))
											 })
	names(specs_ls) <- GBIF_specs
	
	## Making list into single data frame when Capfitogen mode is toggled on.
	if(Mode == "Capfitogen"){
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
		                       "ADM1", "ADM2", "ADM3", "ADM4", 
		                       "COLLSITE", 
		                       "DECLATITUDE", "LATITUDE", 
		                       "DECLONGITUDE", "LONGITUDE", 
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
		CapfitogenData$ACCENUMB <- seq(from = 1, to = nrow(CapfitogenData), by = 1)
		## Add in the species, latitude and longitude (nothing else at this point)
		CapfitogenData$SPECIES <- specs_ls$species
		CapfitogenData$DECLATITUDE <- st_coordinates(specs_ls)[,"Y"]
		CapfitogenData$DECLONGITUDE <- st_coordinates(specs_ls)[,"X"]
		specs_ls <- CapfitogenData
	}
	
	### Returning Object to Disk and Environment ----
	save_ls <- list(meta = occ_meta,
									occs = specs_ls
									# ,
									# json = JSON_ls
									)
	
	saveObj(save_ls, file = FNAME)
	unlink(occ_get) # removes .zip file
	
	### JSON RO-CRATE creation ----
	JSON_ls <- jsonlite::read_json("ro-crate-metadata.json")
	
	JSON_ls$`@graph`[[2]]$hasPart[[1]]$`@id` <- basename(FNAME)
	JSON_ls$`@graph`[[2]]$about[[1]]$`@id` <- paste0("https://www.gbif.org/species/", tax_ID) # gbif ID
	JSON_ls$`@graph`[[2]]$creator$`@id` <- c(JSON_ls$`@graph`[[2]]$creator$`@id`, as.character(options("gbif_email")))
	JSON_ls$`@graph`[[2]]$author$`@id` <- c(JSON_ls$`@graph`[[2]]$author$`@id`, as.character(options("gbif_email")))
	JSON_ls$`@graph`[[2]]$datePublished <- Sys.time()
	JSON_ls$`@graph`[[2]]$name <- paste("Cleaned GBIF occurrence records for", RankGBIF, species)
	JSON_ls$`@graph`[[2]]$keywords <- list("GBIF", "Occurrence", "Biodiversity", "Observation", Mode)
	JSON_ls$`@graph`[[2]]$description <- paste(Mode, "input data for", species)
	
	JSON_ls$`@graph`[[3]]$name <- basename(FNAME)
	JSON_ls$`@graph`[[3]]$contentSize <- file.size(FNAME)
	JSON_ls$`@graph`[[3]]$encodingFormat <- "application/RData"
	JSON_ls$`@graph`[[3]]$`@id` <- basename(FNAME)
	
	JSON_ls$`@graph`[[4]]$name <- c(as.character(options("gbif_user")), JSON_ls$`@graph`[[4]]$name)
	JSON_ls$`@graph`[[4]]$`@id` <- c(JSON_ls$`@graph`[[4]]$`@id`, as.character(options("gbif_email")))
	JSON_ls$`@graph`[[4]]$`@type` <- c(JSON_ls$`@graph`[[4]]$`@type`, "Organisation")
	
	JSON_ls$`@graph`[[5]]$agent$`@id` <- c(JSON_ls$`@graph`[[5]]$agent$`@id`, as.character(options("gbif_email")))
	JSON_ls$`@graph`[[5]]$instrument$`@id` <- "https://github.com/BioDT/uc-CWR"
	
	con <- file(file.path(Dir, paste0(species, ".json")))
	writeLines(jsonlite::toJSON(JSON_ls, pretty = TRUE), con)
	close(con)
	
	save_ls
}

# BIOCLIMATIC VARIABLE DOWNLOAD --------------------------------------------
#' queries, downloads, and computes bioclimatic variables 
#' at global extent from ERA5-Land. Water availability is based on 
#' soil moisture level 1 (0-7cm) and 2 (7-28cm)
FUN.DownBV <- function(
    T_Start = 1970, # what year to begin climatology calculation in
		T_End = 2000, # what year to end climatology calculation in
		Dir = getwd(), # where to store the data output on disk
		Force = FALSE # do not overwrite already present data
		){
	FNAME <- file.path(Dir, paste0("BV_", T_Start, "-", T_End, ".nc"))
	
	if(!Force & file.exists(FNAME)){
		BV_ras <- stack(FNAME)
		names(BV_ras) <- paste0("BIO", 1:19)
		message("Data has already been downloaded with these specifications previously. It has been loaded from the disk. If you wish to override the present data, please specify Force = TRUE")
		return(BV_ras)
	}
	
	### Raw soil moisture level data ----
	#' Download raw soil moisture data for layers 1 (0-7cm) and 
	#' 2 (7-28cm) separately. 
	#' These are summed up and used in KrigR's bioclimatic variable computation 
	if(!file.exists(
		file.path(Dir, 
							paste(tools::file_path_sans_ext(basename(FNAME)), 
										"volumetric_soil_water_layer_1", 
										"RAW.nc", sep = "_"))
	)){
		#### Downloading ----
		Qsoil1_ras <- CDownloadS(
			Variable = "volumetric_soil_water_layer_1", # could also be total_precipitation
			DataSet = "reanalysis-era5-land-monthly-means",
			Type = "monthly_averaged_reanalysis",
			DateStart = paste0(T_Start, "-01-01 00:00"),
			DateStop = paste0(T_End, "-12-31 23:00"),
			TResolution = "month",
			Dir = Dir,
			Extent = ne_countries(type = "countries", scale = "medium")[,1],
			FileName = "Qsoil1",
			API_User = API_User,
			API_Key = API_Key
		)
		
		Qsoil2_ras <- CDownloadS(
			Variable = "volumetric_soil_water_layer_2", # could also be total_precipitation
			DataSet = "reanalysis-era5-land-monthly-means",
			Type = "monthly_averaged_reanalysis",
			DateStart = paste0(T_Start, "-01-01 00:00"),
			DateStop = paste0(T_End, "-12-31 23:00"),
			TResolution = "month",
			Dir = Dir,
			Extent = ne_countries(type = "countries", scale = "medium")[,1],
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
			FName = file.path(Dir, 
												paste(tools::file_path_sans_ext(FNAME), 
															"volumetric_soil_water_layer_1", "RAW.nc", 
															sep = "_")),
			Attrs = terra::metags(QSoilCombin_ras), Write = TRUE,
			Compression = 9
		)
		
		### Deleting unnecessary files ----
		unlink(list.files(Dir, pattern = "Qsoil", full.names = TRUE))
	}
	
	### Bioclimatic data ----
	if(file.exists(file.path(Dir, paste0("BV_", T_Start, "-", T_End, ".nc")))){
		BV_ras <- stack(file.path(Dir, paste0("BV_", T_Start, "-", T_End, ".nc")))
	}else{
		BV_ras <- BioClim(
			Temperature_Var = "2m_temperature",
			Temperature_DataSet = "reanalysis-era5-land",
			Temperature_Type = NA,
			Water_Var = "volumetric_soil_water_layer_1", # could also be total_precipitation
			Water_DataSet = "reanalysis-era5-land-monthly-means",
			Water_Type = "monthly_averaged_reanalysis",
			Y_start = T_Start, Y_end = T_End,
			Extent = ne_countries(type = "countries", scale = "medium")[,1],
			Dir = Dir, FileName = paste0("BV_", T_Start, "-", T_End, ".nc"), 
			FileExtension = ".nc", Compression = 9, # file storing
			API_User = API_User, 
			API_Key = API_Key
		)
	}
	
	### JSON RO-CRATE creation ----
	JSON_ls <- jsonlite::read_json("ro-crate-metadata.json")
	
	JSON_ls$`@graph`[[2]]$hasPart[[1]]$`@id` <- basename(FNAME)
	JSON_ls$`@graph`[[2]]$about[[1]]$`@id` <- 
	  "https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land"
	JSON_ls$`@graph`[[2]]$datePublished <- Sys.time() # tail(file.info(FNAME)$ctime)
	JSON_ls$`@graph`[[2]]$name <- 
	  "Bioclimatic data obtained from ERA5-Land. Water avialbility is denoted via the sum of soil moisture layer 1 and 2."
	JSON_ls$`@graph`[[2]]$keywords <- list("ERA5-Land", 
	                                       "ECMWF", 
	                                       "Bioclimatic Variables", 
	                                       "Soil Moisture")
	JSON_ls$`@graph`[[2]]$description <- "Bioclimatic data obtained from ERA5-Land. Water avialbility is denoted via the sum of soil moisture layer 1 and 2."
	
	JSON_ls$`@graph`[[3]]$name <- basename(FNAME)
	JSON_ls$`@graph`[[3]]$contentSize <- file.size(FNAME)
	JSON_ls$`@graph`[[3]]$`@id` <- basename(FNAME)
	
	JSON_ls$`@graph`[[5]]$instrument$`@id` <- 
	  "https://doi.org/10.1088/1748-9326/ac48b3"
	
	con <- file(file.path(Dir, 
	                      paste0(tools::file_path_sans_ext(basename(FNAME)), 
	                             ".json")))
	writeLines(jsonlite::toJSON(JSON_ls, 
	                            pretty = TRUE), 
	           con)
	close(con)
	
	BV_ras
}

## EDAPHIC DATA DOWNLOAD ------------------------------------------------------
# HJ: new part, INCOMPLETE!
# EL: data also need to be changed to get the same resolution as the BioClim 
# variables downloaded with FUN.DownBV

FUN.DownEV <- function(Dir = getwd(), # where to store the data output on disk
                       Force = FALSE, # do not overwrite already present data, 
                       ){
  FNAME <- file.path(Dir, "edaphic.nc")
  
  # check if file exists and whether to overwrite
  if(!Force & file.exists(FNAME)){
    EV_ras <- stack(FNAME)
    #names(EV_ras) <- paste0("BIO", 1:19) # replace with edaphic names vector
    message("Data has already been downloaded with these specifications. It has been loaded from the disk. If you wish to override the present data, please specify Force = TRUE")
    return(EV_ras)
  }
  
  if(!file.exists(
    
  )){
    ## downloading
    
  }
  evarg <- c(arg1, arg2, arg3)
  # evargs <- commandArgs(trailingOnly = TRUE)
  #evarg <- c("soc", "silt")
  edaph_ras <- soil_world(evarg, depth = 5, path = file.path(Dir.Data.Envir, "Edaphic"))
  names(edaph_ras) <- evarg
  
  ### Masking ----
  # whole world
  #Land_sp <- ne_countries(type = "countries", scale = "medium")
  
  # HJ: for testing/ to match previous Capfitogen tests: only Spain
  # HJ: this is an attempt to do the same thing with terra that was done with KrigR in ModGP (see below)
  # please switch back to KrigR is wanted/needed
  Land_sp <- ne_states("Spain")
  edaph_ras <- crop(edaph_ras, terra::ext(Land_sp))
  edaph_ras <- terra::mask(edaph_ras, vect(Land_sp))
  
  # BV_ras <- crop(BV_ras, extent(Land_sp))
  # BV_mask <- KrigR:::mask_Shape(base.map = BV_ras[[1]], Shape = Land_sp[,"name"])
  # BV_ras <- mask(BV_ras, BV_mask)
  
  ### Saving ----
  terra::writeCDF(edaph_ras, filename = FNAME, overwrite = TRUE)
  unlink(file.path(Dir.Data.Envir, "Edaphic", "soil_world", "*.tif"))
  
  edaph_ras
  
}
