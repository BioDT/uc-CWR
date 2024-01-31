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
	if(!is.null(parallel)){ # parallelisation
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
		# SDMData_iter <- occ_ls[[83]]
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
		### Returning to outside of apply ----
		list(PA = PA_df)
		
	})
	if(!is.null(parallel)){
		stopCluster(parallel)
		closeAllConnections()
	}
	
	### Returning Object to Disk and Environment ----
	saveObj(return_ls, file = FNAME)
	return_ls
}

# SDM SPECIES PRE-SELECTION ------------------------------------------------
FUN.PreSelect <- function(Input_ls, # list of sf objects of presences and absences
													Occurrences = 40, # how may unique occurrences to have at least for retaining a species
													Locations = 40, # how may unique locations to have at least for retaining a species
													BV_ras = BV_ras, # stack of environmental variables
													parallel = 1 # an integer, 1 = sequential
){
	message("Limiting species for which models witll be run by their spread and available data.")
	Globe_sf <- gisco_get_countries()
	
	### Parallel Set-Up ----
	if(parallel == 1){parallel <- NULL} # no parallelisation
	
	### This needs to be commented back in when wanting to run code below directly
	if(!is.null(parallel)){ # parallelisation
		message("Registering cluster for parallel processing")
		print("Registering cluster")
		parallel <- parallel::makeCluster(parallel)
		on.exit(stopCluster(parallel))
		print("R Objects loading to cluster")
		parallel::clusterExport(parallel, varlist = c(
			"package_vec", "install.load.package",
			"Globe_sf", "BV_ras"
		), envir = environment())
		print("R Packages loading on cluster")
		clusterpacks <- clusterCall(parallel, function() sapply(package_vec, install.load.package))
	}
	
	useablespec_ls <- pblapply(Input_ls, 
														 cl = parallel,
														 function(SDMModel_Iter){
														 	
														 	## Loading covariate data
														 	cov <- rast(readAll(BV_ras)[[1]])
														 	cov_crs <- st_crs(cov)
														 	
														 	## make df object
														 	Occ_df <- SDMModel_Iter$PA # SDMInput_ls$`Lathyrus vernus`$PA
														 	spec_name <- unique(Occ_df$species[Occ_df$PRESENCE == 1])
														 	Occ_df$modelSpec <- spec_name
														 	# print(spec_name)
														 	
														 	## identify useable data
														 	if(nrow(Occ_df) == 0){
														 		useableocc <- 0
														 		uniquecells <- 0
														 	}else{
														 		## assign correct crs
														 		Occ_sf <- st_as_sf(Occ_df, crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
														 		Occ_sf <- st_transform(Occ_sf, crs = cov_crs)
														 		## filter by land area of polygon
														 		Occ_sf <- st_filter(Occ_sf, Globe_sf)
														 		## filter additionally by covariate data
														 		valsext <- terra::extract(y = Occ_sf[Occ_sf$PRESENCE == 1, ], x = cov)
														 		useableocc <- sum(!is.na(valsext$BIO1))
														 		## identify unique cells filled by occurrences
														 		uniquecells <- sum(values(rasterize(Occ_sf, cov, field = "PRESENCE"))[,1] == 1, na.rm = TRUE)
														 	}
														 	## report back
														 	data.frame(locs = useableocc, cells = uniquecells)
														 })
	if(!is.null(parallel)){
		stopCluster(parallel)
		closeAllConnections()
	}
	useablespec_df <- do.call(rbind, useablespec_ls)
	
	### Selection and Cutoffs ----
	Input_ls <- Input_ls[which(useablespec_df$locs > Occurrences & useablespec_df$cells > Locations)]
	
	### Returning Object to Environment ----
	return(Input_ls)
}

FUN.ExecSDM <- function(SDMData_ls = NULL, # list of presences/absences per species in sf objects
												BV_ras = NULL, # stack of environmental variables
												Dir = NULL, # where to save output
												Force = FALSE, # whether to force re-running
												KeepModels = TRUE, # whether to retain the ISDM model objects
												Drivers = NULL, # which drivers to make PNG response curve plots for
												parallel = 1 # an integer, 1 = sequential
){
	GenName <- strsplit(names(SDMData_ls)[1], split = " ")[[1]][1]
	FNAME <- file.path(Dir, GenName, "_SDMData.RData")
	
	if(file.exists(FNAME) & !Force){
		SDMModel_ls <- loadObj(FNAME)
		warning("SDM data have already been prepared with these specifications previously. They have been loaded from the disk. If you wish to override the present data, please specify Force = TRUE")
		return(SDMModel_ls)
	}
	
	### Parallel Set-Up ----
	if(parallel == 1){parallel <- NULL} # no parallelisation
	
	### This needs to be commented back in when wanting to run code below directly
	if(!is.null(parallel)){ # parallelisation
		message("Registering cluster for parallel processing")
		print("Registering cluster")
		parallel <- parallel::makeCluster(parallel)
		on.exit(stopCluster(parallel))
		print("R Objects loading to cluster")
		parallel::clusterExport(parallel, varlist = c(
			"package_vec", "install.load.package",
			"BV_ras", "Drivers", "KeepModels", "Force", "Dir",
			"GenName", "FUN.Viz", "FUN.ShinyPrep", "Plot_BC", "%nin%",
			"parallel", "Dir.Data.ModGP")
		, envir = environment())
		print("R Packages loading on cluster")
		clusterpacks <- clusterCall(parallel, function() sapply(package_vec, install.load.package))
	}
	
	SDMModel_ls <- pblapply(SDMData_ls, 
													cl = parallel,
													FUN = function(SDMModel_Iter){
		# SDMModel_Iter <- SDMData_ls[[1]]
														
														setwd(Dir)
														if(!is.null(parallel)){
															inla.setOption(num.threads = 1)
															on.exit(inla.setOption(num.threads = parallel::detectCores()))
															}
														
														# LOADING RASTERS INTO MEMORY -------
														## Loading covariate data
														BV_ras <- readAll(BV_ras)
														Drivers <- rast(readAll(Drivers))
														
														# LOADING RASTERS INTO MEMORY -------
														Occ_df <- SDMModel_Iter$PA
														spec_name <- unique(Occ_df$species[Occ_df$PRESENCE == 1])
														Occ_df$modelSpec <- spec_name
														Dir_spec <- file.path(Dir, GenName, str_replace(spec_name, " ", "_"))
														if(!dir.exists(Dir_spec)){dir.create(Dir_spec)}
														# print(spec_name)
														Occ_sf <- st_as_sf(Occ_df, crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

														# COVARIATES -------
														cov <- rast(BV_ras)
														vars <- colnames(Occ_df)[startsWith(colnames(Occ_df), prefix = "BIO")]
														cov <- cov[[as.numeric(gsub(".*?([0-9]+).*", "\\1", vars))]]
														names(cov) <- vars
														
														Occ_sf <- st_transform(Occ_sf, crs = st_crs(cov))

														if(file.exists(file.path(getwd(), GenName, gsub(spec_name, pattern = " ", replacement = "_"), "Predictions.rds")) &
															 !Force){
															message(paste("ISDM already compiled for", spec_name, "with these specifications previously. They have been loaded from the disk. If you wish to override the present data, please specify Force = TRUE"))
														}else{
															message(paste("Compiling ISDM for", spec_name))
															start_time <- Sys.time()
															
															# SDM WORKFLOW -------
															## Workflow Setup ----
															### base ----
															workflow <- startWorkflow(
																Projection = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
																Species = unique(Occ_df$modelSpec),
																saveOptions = list(projectName =  GenName), Save = TRUE, Quiet = TRUE
															)
															
															### area ----
															Globe_sf <- gisco_get_countries()
															workflow$addArea(Object = Globe_sf)
															
															### presence/absence ----
															workflow$addStructured(dataStructured = Occ_sf, datasetType = 'PA',
																										 responseName = 'PRESENCE',
																										 speciesName = 'modelSpec',
																										 coordinateNames = c('lon', 'lat'))
															# workflow$plot(Species = TRUE)
															
															### covariates ----
															workflow$addCovariates(Object = cov)
															
															### mesh ----
															load(file.path(Dir.Data.ModGP, "GlobalAreaCRS.RData"))
															workflow$.__enclos_env__$private$Area <- st_transform(workflow$.__enclos_env__$private$Area, crsto)
															workflow$addMesh(cutoff = 20000,
																							 max.edge = c(60000, 80000),
																							 offset= 100000)
															
															### priors ----
															workflow$specifySpatial(prior.range = c(300000, 0.05),
																											prior.sigma = c(50, 0.2))
															
															### cross-validation ----
															# workflow$crossValidation(Method = 'Loo')
															
															### outputs ----
															workflow$workflowOutput(c("Model", "Predictions"))
															
															### prediction data ----
															pred_spsf <- as(stack(cov), "SpatialPixelsDataFrame")
															
															### INLA options ----
															workflow$modelOptions(INLA = list(control.inla=list(int.strategy = 'eb',
																																									cmin = 0),
																																safe = TRUE,
																																inla.mode = 'experimental'))
															
															## Execution ----
															Model_iSDM <- sdmWorkflow(workflow,
																												predictionDim = c(pred_spsf@grid@cells.dim[1],
																																					pred_spsf@grid@cells.dim[2]),
																												predictionData = pred_spsf)
															
															end_time <- Sys.time()
															
															sink(file.path(Dir_spec, "ExecutionDuration.txt"))
															print(end_time - start_time)
															sink()
															
														}
														
														sink(file.path(Dir_spec, "Progress.txt"))
														print("6 Reading output back in")
														sink()
														
														## Output ----
														### reading back in ----
														Predictions <- readRDS(file.path(getwd(), GenName, gsub(spec_name, pattern = " ", replacement = "_"), "Predictions.rds"))
														intModel <- readRDS(file.path(getwd(), GenName, gsub(spec_name, pattern = " ", replacement = "_"), "intModel.rds"))
														
														### output rasters ----
														#### predicted suitability ----
														preds <- cbind(
															Predictions$predictions@coords,
															Predictions$predictions$mean
														)
														colnames(preds) <- c("x", "y", "z")
														preds <- rasterFromXYZ(as.data.frame(preds)[, c("x", "y", "z")])
														preds <- rast(preds)
														suitability_ras <- # exp(
															preds
														#) # exp() can produce serious outliers
														
														#### binarising suitability
														Occ_ras <- rasterize(Occ_sf, cov, field = "PRESENCE")
														RemoveNA <- data.frame(Suit = values(suitability_ras), 
																									 Knowns = values(Occ_ras))
														colnames(RemoveNA) <- c("Suitability", "Observation")
														RemoveNA <- na.omit(RemoveNA)
														
														png(file.path(getwd(), GenName, gsub(spec_name, pattern = " ", replacement = "_"), "ROC.png"), width = 16, height = 16, units = "cm", res = 100)
														ROC <- Epi::ROC(test = RemoveNA$Suitability, stat = RemoveNA$Observation)
														dev.off()
														
														# sensitivity value at maximizing cutoff point (sensitivity + specifcity = MAX)
														Bin_thresh <- which.max(rowSums(ROC$res[, c("sens", "spec")]))
														binarised_ras <- suitability_ras > as.numeric(names(Bin_thresh))
														
														#### combining model output rasters ----
														modelled_ras <- c(suitability_ras, binarised_ras)
														names(modelled_ras) <- c("Suitability", "Predicted Presence/Absence")
														try(writeCDF(modelled_ras, 
																				 file.path(Dir_spec, paste0(gsub(spec_name, pattern = " ", replacement = "_"), "-Outputs.nc")),
																				 overwrite = TRUE
														)
														)
														
														if(length(list.files(Dir_spec, pattern = "RESPCURV")) == nlyr(Drivers)){
															message("Shiny data and plots already produced.")
														}else{
															# MAKING SHINY OUTPUTS ----
															Shiny_ls <- FUN.ShinyPrep(SDMModel_Iter, Dir_spec = Dir_spec)
															
															# MAKING PNGs FOR SHINY AND PRESENTATIONS ----
															Plots_ls <- FUN.Viz(Shiny_ls, Model_ras = modelled_ras, 
																									BV_ras, Covariates = Drivers, 
																									Dir_spec = Dir_spec)
														}
														
														# REPORTING BACK TO LIST ----
														list(Outputs = modelled_ras,
																 ISDM = intModel)
													})
	if(!is.null(parallel)){
		stopCluster(parallel)
		closeAllConnections()
	}
	saveObj(SDMModel_ls, file = FNAME)
	if(!KeepModels){unlink(file.path(Dir, GenName), recursive = TRUE)}
	setwd(Dir.Base)
	SDMModel_ls
}