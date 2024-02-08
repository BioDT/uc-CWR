#' ####################################################################### #
#' PROJECT: [BioDT CWR] 
#' CONTENTS: 
#'  - SDM Functionality
#'  	- Data Preparation
#'  	- Model Execution
#'  DEPENDENCIES:
#'  - None
#' AUTHOR: [Erik Kusch]
#' ####################################################################### #

# SDM DATA PREPARATION FUNCTION -------------------------------------------
FUN.PrepSDMData <- function(occ_ls = NULL, # list of occurrences per species in sf objects
														BV_ras = NULL, # stack of environmental variables
														Dir = NULL, # where to save output
														Occurrences = 40, # how may unique occurrences to have at least for retaining a species
														Locations = 40, # how may unique locations to have at least for retaining a species
														Force = FALSE, # whether to force re-running
														parallel = 1 # an integer, 1 = sequential
){
	
	FNAME <- file.path(Dir, paste0(strsplit(names(occ_ls)[1], split = " ")[[1]][1], "_SDMData.RData"))
	
	if(file.exists(FNAME) & !Force){
		return_ls <- loadObj(FNAME)
		warning("SDM data have already been prepared with these specifications previously. They have been loaded from the disk. If you wish to override the present data, please specify Force = TRUE")
		return(return_ls)
	}
	message("Preparing occurrence data for SDM workflow")
	
	Species_sf <- do.call(rbind, Species_ls$occs)
	
	### Parallel Set-Up ----
	if(parallel == 1){parallel <- NULL} # no parallelisation
	
	### This needs to be commented back in when wanting to run code below directly
	if(!is.null(parallel) && (is.na(strtoi(Sys.getenv("CWR_ON_LUMI"))))){ # parallelisation
		message("Registering cluster for parallel processing")
		print("Registering cluster")
		parallel <- parallel::makeCluster(parallel)
		on.exit(stopCluster(parallel))
		print("R Objects loading to cluster")
		parallel::clusterExport(parallel, varlist = c(
			"package_vec", "install.load.package",
			"Species_sf", "BV_ras"
		), envir = environment())
		print("R Packages loading on cluster")
		clusterpacks <- clusterCall(parallel, function() sapply(package_vec, install.load.package))
	}
	
	### Preparing Data Species by Species ----
	message("Figuring out Presences and Absences for each species")
	return_ls <- pblapply(occ_ls, 
												cl = parallel,
												FUN = function(SDMData_iter){
		# SDMData_iter <- occ_ls[[1]]
		Presences_sf <- SDMData_iter["species"]
		
		### Initial Environmental NA match Removal ----
		NAcheck <- raster::extract(BV_ras, st_coordinates(Presences_sf), df = TRUE)
		Presences_sf <- Presences_sf[!is.na(rowSums(NAcheck)), ]
		if(nrow(Presences_sf) < 1){
			PA_df <- data.frame(Presences_sf)
		}else{
			### Training Region Limiting ----
			buffer_sf <- st_union(st_buffer(Presences_sf, 15)) # 15 degree buffer around points
			
			### Environmental Data Colinearity ----
			BV_iter <- crop(BV_ras, extent(st_bbox(buffer_sf)))
			BV_iter <- mask(BV_iter, as(buffer_sf, "Spatial"))
			v <- usdm::vifcor(terra::rast(BV_iter), th = 0.7) # variable inflation factor
			biomod <- exclude(BV_iter, v) # now exclude those with high cor and vif
			
			### Pseudoabsences ----
			#' to generate pseudo-absence points. 1e4 randomly selected points where the model target is absent but other species are present this can be further modified - for example limiting it by minimum convex that encompasses only 90% of the occurrence data 10% of the distant occurrence points won't be considered 
			#' Absences
			SpeciesNon_sf <- Species_sf[Species_sf$species != unique(SDMData_iter$species), ] # select non-target species records
			SpeciesNon_sf <- st_filter(SpeciesNon_sf, buffer_sf) # select only those in buffered area
			set.seed(42) # setting seed for reproducibly random process
			Absences_sf <- SpeciesNon_sf[sample(1:nrow(SpeciesNon_sf), 
																					size = ifelse(nrow(SpeciesNon_sf)>1e4, 1e4, nrow(SpeciesNon_sf))), ] # select absences
			Absences_sf$PRESENCE <- 0 # assign absence	
			#' Presences
			Presences_sf <- SDMData_iter
			Presences_sf$PRESENCE <- 1 # assign presence
			
			### Combining PA and P data ----
			PA_sf <- rbind(Absences_sf, Presences_sf)
			
			### Extracting Environmental data ----
			Predictors <- raster::extract(biomod, st_coordinates(PA_sf))
			PA_sf <- cbind(PA_sf, Predictors)
			
			### Making into dataframe ----
			PA_df <- as.data.frame(PA_sf)
			lon <- PA_df$lon <- st_coordinates(PA_sf)[,1]
			lat <- PA_df$lat <- st_coordinates(PA_sf)[,2]
			# PA_df <- na.omit(PA_df)
		}
		
		### Creating SDM Input Object ----
		if(nrow(PA_df) == 0){
			data_SDM <- NA
		}else{
			data_SDM <- sdmData(PRESENCE~.+coords(lon+lat), 
													train = na.omit(PA_df[
														, c("lon","lat","PRESENCE", colnames(Predictors))
													])
													)
		}
		
		### Unique Locations & Observations ----
		Occ_df <- PA_df
		spec_name <- unique(Occ_df$species[Occ_df$PRESENCE == 1])
		Occ_df$modelSpec <- spec_name
		
		## identify useable data
		if(nrow(Occ_df) == 0){
			useableocc <- 0
			uniquecells <- 0
		}else{
			## load covariates raster in
			cov <- rast(readAll(BV_ras)[[1]])
			cov_crs <- st_crs(cov)
			## assign correct crs
			Occ_sf <- st_as_sf(Occ_df, crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
			Occ_sf <- st_transform(Occ_sf, crs = cov_crs)
			## filter additionally by covariate data
			valsext <- terra::extract(y = Occ_sf[Occ_sf$PRESENCE == 1, ], x = cov)
			useableocc <- sum(!is.na(valsext$BIO1))
			## identify unique cells filled by occurrences
			uniquecells <- sum(values(rasterize(Occ_sf, cov, field = "PRESENCE"))[,1] == 1, na.rm = TRUE)
		}
		
		### Returning to outside of apply ----
		list(PA = PA_df,
				 SDMData = data_SDM,
				 Useable = data.frame(locs = useableocc, cells = uniquecells))
		
	})
	
	### Limitting to useable species ----
	useablespec_df <- do.call(rbind, lapply(return_ls, "[[", "Useable"))
	return_ls <- return_ls[which(useablespec_df$locs > Occurrences & useablespec_df$cells > Locations)]
	
	### Returning Object to Disk and Environment ----
	saveObj(return_ls, file = FNAME)
	
	### JSON RO-CRATE creation ----
	JSON_ls <- jsonlite::read_json("ro-crate-metadata.json")
	
	JSON_ls$`@graph`[[2]]$hasPart[[1]]$`@id` <- basename(FNAME)
	JSON_ls$`@graph`[[2]]$about[[1]]$`@id` <- paste("Presence/Absence data frames for", strsplit(names(occ_ls)[1], split = " ")[[1]][1], "species")
	JSON_ls$`@graph`[[2]]$datePublished <- Sys.time()
	JSON_ls$`@graph`[[2]]$name <- paste("Presence/Absence data frames for", strsplit(names(occ_ls)[1], split = " ")[[1]][1], "species")
	JSON_ls$`@graph`[[2]]$keywords <- list("GBIF", "Occurrence", "Biodiversity", "Observation", "ModGP", "SDM")
	JSON_ls$`@graph`[[2]]$description <- paste("SDM input data for ModGP")
	
	JSON_ls$`@graph`[[3]]$name <- basename(FNAME)
	JSON_ls$`@graph`[[3]]$contentSize <- file.size(FNAME)
	JSON_ls$`@graph`[[3]]$encodingFormat <- "application/RData"
	JSON_ls$`@graph`[[3]]$`@id` <- basename(FNAME)
	
	JSON_ls$`@graph`[[5]]$instrument$`@id` <- "https://github.com/BioDT/uc-CWR"
	
	con <- file(file.path(Dir, paste0(tools::file_path_sans_ext(basename(FNAME)), ".json")))
	writeLines(jsonlite::toJSON(JSON_ls, pretty = TRUE), con)
	close(con)
	
	return_ls
}

# SDM EXECUTION & PREDICTION -----------------------------------------------
FUN.ExecSDM <- function(SDMData_ls = NULL, # list of presences/absences per species in sf objects
												BV_ras = NULL, # stack of environmental variables
												Dir = NULL, # where to save output
												Force = FALSE, # whether to force re-running
												Drivers = NULL, # which drivers to make PNG response curve plots for
												parallel = 1 # an integer, 1 = sequential
){
	GenName <- strsplit(names(SDMData_ls)[1], split = " ")[[1]][1]
	Dir.Genus <- file.path(Dir, GenName)
	if(!dir.exists(Dir.Genus)){dir.create(Dir.Genus)}
	FNAME <- file.path(Dir, paste0(GenName, "_SDMData.RData"))
	
	if(file.exists(FNAME) & !Force){
		SDMModel_ls <- loadObj(FNAME)
		warning("SDM data have already been prepared with these specifications previously. They have been loaded from the disk. If you wish to override the present data, please specify Force = TRUE")
		return(SDMModel_ls)
	}
	
	### Loading rasters into memory ----
	BV_ras <- readAll(BV_ras)
	Drivers <- readAll(Drivers)
	
	### Parallel Set-Up ----
	if(parallel == 1){parallel <- NULL} # no parallelisation
	
	### This needs to be commented back in when wanting to run code below directly
	if(!is.null(parallel) && (!is.na(strtoi(Sys.getenv("CWR_ON_LUMI"))))){ # parallelisation
		message("Registering cluster for parallel processing")
		print("Registering cluster")
		parallel <- parallel::makeCluster(parallel)
		on.exit(stopCluster(parallel))
		print("R Objects loading to cluster")
		parallel::clusterExport(parallel, varlist = c(
			"package_vec", "install.load.package",
			"BV_ras", "Drivers", "Dir", "Dir.Genus", 
			"GenName", "FUN.Viz", "FUN.ShinyPrep", "Plot_BC", "%nin%",
			"parallel")
		, envir = environment())
		print("R Packages loading on cluster")
		clusterpacks <- clusterCall(parallel, function() sapply(package_vec, install.load.package))
	}
	
	SDMModel_ls <- pblapply(SDMData_ls, 
													cl = parallel,
													FUN = function(SDMModel_Iter){
														# SDMModel_Iter <- SDMData_ls[[1]]
														
														# SETTING UP PARALLEL EXECUTION ------- 
														SDMpar <- ifelse(!is.null(parallel), 1, parallel::detectCores())
														
														# EXTRACTING DATA FROM LIST -------
														PA_df <- SDMModel_Iter$PA
														data_SDM <- SDMModel_Iter$SDMData
														spec_name <- unique(PA_df$species[PA_df$PRESENCE == 1])
														
														# GETTING DIRECTORIES AND NAMES -------
														print(spec_name)
														Dir.Species <- file.path(Dir.Genus, strsplit(spec_name, " ")[[1]][2])
														if(!dir.exists(Dir.Species)){dir.create(Dir.Species)}
														FNAMEInner <- file.path(Dir.Species, "SDM.RData")
														
														# RUNNING ENSEMBLE MODEL -------
														if(file.exists(FNAMEInner)){
															return_ls <- loadObj(FNAMEInner)
														}else{
															## executing mdeols
															model_SDM <- sdm(~., data_SDM, ## discuss settings here!!!
																							 methods = c("maxent","gbm","GAM"),
																							 replications = c("sub", "boot"),
																							 test.p = 25,
																							 n = 2,
																							 parallelSetting = list(ncore = SDMpar, method = "parallel")
																							 )
															## building ensemble
															ensemble_SDM <- ensemble(model_SDM, BV_ras, 
																											 filename = file.path(Dir.Species, "ensemble"), 
																											 setting = list(method = "weighted", 
																											 							 stat = "tss", opt = 2), 
																											 overwrite = TRUE)
															## evaluate models
															eval_SDM <- getEvaluation(model_SDM, stat = c("TSS", "threshold"))
															## prediction
															prediction_SDM <- predict(model_SDM, BV_ras, 
																												filename = file.path(Dir.Species, "prediction"), 
																												overwrite = TRUE)
															# this block is needed to load fully into memory
															ensemble_SDM <- readAll(ensemble_SDM)
															modelnames <- names(prediction_SDM) # keep names
															prediction_SDM <- stack(file.path(Dir.Species, "prediction")) # index on drive
															prediction_SDM <- readAll(prediction_SDM) # load fully from file
															names(prediction_SDM) <- modelnames # assign names back on
															binarised_SDM <- prediction_SDM > eval_SDM$threshold ## is this correct!!!
															proportion_SDM <- sum(binarised_SDM)/nlayers(binarised_SDM)
															
															## save rasters
															raster::writeRaster(ensemble_SDM, 
																									filename = file.path(Dir.Species, "Continuous.nc"), format = "CDF")
															raster::writeRaster(prediction_SDM, 
																									filename = file.path(Dir.Species, "MODELS-Continuous.nc"), format = "CDF")
															raster::writeRaster(binarised_SDM, 
																									filename = file.path(Dir.Species, "MODELS-Binarised.nc"), format = "CDF")
															raster::writeRaster(proportion_SDM, 
																									filename = file.path(Dir.Species, "Proportion.nc"), format = "CDF")
															
															## make a list of outputs
															return_ls <- list(
																Models = list(
																	model = model_SDM,
																	evalalutation = eval_SDM),
																Prediction = list(
																	continuous = ensemble_SDM,
																	proportion = proportion_SDM,
																	MODELS_continuous = prediction_SDM,
																	MODELS_binarised = binarised_SDM
																)
																)
															saveObj(return_ls, file = FNAMEInner)
															
															## unlink SDM workflow files
															unlink(list.files(Dir.Species, pattern = "prediction", full.names = TRUE))
															unlink(list.files(Dir.Species, pattern = "ensemble", full.names = TRUE))
														}
														
														if(length(list.files(Dir.Species, pattern = "RESPCURV")) == nlayers(Drivers)){
															message("Shiny data and plots already produced.")
														}else{
															# MAKING SHINY OUTPUTS ----
															Shiny_ls <- FUN.ShinyPrep(PA = PA_df, Dir_spec = Dir.Species)
															
															# MAKING PNGs FOR SHINY AND PRESENTATIONS ----
															Plots_ls <- FUN.Viz(Shiny_ls, 
																									Model_ras = stack(return_ls$Prediction$continuous,
																																		return_ls$Prediction$proportion), 
																									BV_ras, Covariates = Drivers,
																									CutOff = 0.6,
																									Dir_spec = Dir.Species)
														}
														
														# JSON RO-CRATE creation ----
														JSON_ls <- jsonlite::read_json(file.path(Dir.Base, "ro-crate-metadata.json"))
														
														## shiny data 
														FNAMEShiny <- file.path(Dir.Species, "ShinyData.RData")
														
														JSON_ls$`@graph`[[2]]$hasPart[[1]]$`@id` <- basename(FNAMEShiny)
														JSON_ls$`@graph`[[2]]$about[[1]]$`@id` <- paste("Data required for shiny app for", spec_name)
														
														JSON_ls$`@graph`[[2]]$datePublished <- Sys.time()
														JSON_ls$`@graph`[[2]]$keywords <- list("GBIF", "Occurrence", "Biodiversity", "Observation", "ModGP", "SDM")
														JSON_ls$`@graph`[[2]]$name <- paste("ModGP outputs for", spec_name)
														JSON_ls$`@graph`[[2]]$description <- paste("ModGP Outputs")
														
														
														JSON_ls$`@graph`[[3]]$name <- basename(FNAMEShiny)
														JSON_ls$`@graph`[[3]]$contentSize <- file.size(FNAMEShiny)
														JSON_ls$`@graph`[[3]]$encodingFormat <- "application/RData"
														JSON_ls$`@graph`[[3]]$`@id` <- basename(FNAMEShiny)
														
														JSON_ls$`@graph`[[5]]$instrument$`@id` <- "https://github.com/BioDT/uc-CWR"
														
														con <- file(file.path(Dir.Species, paste0("ModGP-", spec_name, ".json")))
														writeLines(jsonlite::toJSON(JSON_ls, pretty = TRUE), con)
														close(con)
														
														# REPORTING BACK TO LIST ----
														return_ls
													})
	saveObj(SDMModel_ls, file = FNAME)
	SDMModel_ls
}
