#' ####################################################################### #
#' PROJECT: [BioDT CWR] 
#' CONTENTS: 
#'  - SDM Functionality
#'  	- Data Preparation
#'  	- Model Execution
#'  	- Model Prediction and Desired Summaries
#'  DEPENDENCIES:
#'  - None
#' AUTHOR: [Erik Kusch]
#' ####################################################################### #

# PACKAGES -----------------------------------------------------------------
install.load.package <- function(x) {
	if (!require(x, character.only = TRUE))
		install.packages(x, repos='http://cran.us.r-project.org')
	require(x, character.only = TRUE)
}
package_vec <- c(
	"sdm", # for sdm functionality
	"usdm", # for vifcor
	"raster", # for spatial object handling
	"sf", # for spatialfeatures
	"pbapply" # for parallelised apply functions and estimators
)
sapply(package_vec, install.load.package)

# SDM DATA PREPARATION FUNCTION -------------------------------------------
FUN.PrepSDMData <- function(occ_ls = NULL, # list of occurrences per species in sf objects
														BV_ras = NULL, # stack of environmental variables
														Dir = NULL, # where to save output
														Force = FALSE # whether to force re-running
														){
	
	FNAME <- file.path(Dir, paste0(strsplit(names(occ_ls)[1], split = " ")[[1]][1], ".RData"))
	
	if(file.exists(FNAME) & !Force){
		load(FNAME)
		warning("SDM data have already been prepared with these specifications previously. They have been loaded from the disk. If you wish to override the present data, please specify Force = TRUE")
		return(return_ls)
	}
	
	Species_sf <- do.call(rbind, Species_ls$occs)
	return_ls <- pblapply(occ_ls, FUN = function(SDMData_iter){
		# SDMData_iter <- occ_ls[[1]]
		
		### Training Region Limiting ----
		Presences_sf <- SDMData_iter["species"]
		# print(unique(SDMData_iter["species"]$species))
		buffer_sf <- st_union(st_buffer(Presences_sf, 15)) # 15 degree buffer around points
		
		# plot(crop(Land_sp, extent(Presences_sf)+c(-30,30,-30, 30)))
		# plot(st_union(buffer_sf), col = "red", add = TRUE)
		# plot(Presences_sf, add = TRUE, col = "green", cex = 0.5, pch = 4)
		
		### Environmental Data Colinearity ----
		BV_iter <- crop(BV_ras, extent(st_bbox(buffer_sf)))
		BV_iter <- mask(BV_iter, as(buffer_sf, "Spatial"))
		v <- usdm::vifcor(terra::rast(BV_iter), th = 0.7) # variable inflation factor
		biomod <- exclude(BV_iter, v) # now exclude those with high cor and vif
		
		### Pseudoabsences ----
		#' to generate pseudo-absence points. 1e5 randomly selected points where the model target is absent but other species are present this can be further modified - for example limiting it by minimum convex that encompasses only 90% of the occurrence data 10% of the distant occurrence points won't be considered 
		#' Absences
		SpeciesNon_sf <- Species_sf[Species_sf$species != unique(SDMData_iter$species), ] # select non-target species records
		SpeciesNon_sf <- st_filter(SpeciesNon_sf, buffer_sf) # select only those in buffered area
		set.seed(42) # setting seed for reproducibly random process
		Absences_sf <- SpeciesNon_sf[sample(1:nrow(SpeciesNon_sf), 
																				size = ifelse(nrow(SpeciesNon_sf)>1e5, 1e5, nrow(SpeciesNon_sf))), ] # select absences
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
		PA_df <- na.omit(PA_df)
		
		### Creating SDM Input Object ----
		if(nrow(PA_df) == 0){
			data_SDM <- NA
		}else{
			data_SDM <- sdmData(PRESENCE~.+coords(lon+lat), 
													train = PA_df[
														, c("lon","lat","PRESENCE", colnames(Predictors))
													])  
		}
		
		### Returning to outside of apply ----
		list(PA = PA_df,
				 SDMData = data_SDM)
	})	
	save(return_ls, file = FNAME)
	return_ls
}

# SDM EXECUTION FUNCTION ---------------------------------------------------
FUN.ExecSDM <- function(SDMData_ls = NULL, # list of occurrences per species in sf objects
														BV_ras = NULL, # stack of environmental variables
														Dir = NULL, # where to save output
														Force = FALSE # whether to force re-running
){
	
	FNAME <- file.path(Dir, paste0(strsplit(names(SDMData_ls)[1], split = " ")[[1]][1], ".RData"))
	Dir.Temp <- file.path(Dir, paste("TEMP", strsplit(names(SDMData_ls)[1], split = " ")[[1]][1], sep = "_"))
	
	if(file.exists(FNAME) & !Force){
		load(FNAME)
		warning("Models have already been executed with these specifications previously. They have been loaded from the disk. If you wish to override the present data, please specify Force = TRUE")
		return(SDMModel_ls)
	}
	
	SDMModel_ls <- pblapply(SDMData_ls, FUN = function(SDMModel_Iter){
		PA_df <- SDMModel_iter$PA
		species_iden <- unique(PA_df$species[PA_df$PRESENCE == 1])
		FNAMEInner <- file.path(Dir.Temp, paste0("SDM_", species_iden,".RData"))
		print(species_iden)
		
		if(file.exists(FNAMEInner)){
			load(FNAMEInner)
		}else{
			SDMModel_Iter <- SDMData_ls[[1]]
			data_SDM <- SDMModel_Iter$SDMData
			PA_df <- SDMModel_Iter$PA
			if(sum(PA_df$PRESENCE) < 41){
				model_SDM <- NA
				ensemble_SDM <- NA
				eval_SDM <- NA
			}else{
				model_SDM <- sdm(~., data_SDM, methods = c ("maxent","gbm","GAM","RF"), 
												 replications = c("sub", "boot"), test.p = 25, n = 3,
												 parallelSetting = list(ncore = 4, method = "parallel"))
				ensemble_SDM <- ensemble(model_SDM, BV_ras, filename = "enpr.img", 
																 setting = list(method = "weighted", 
																 							 stat = "tss", opt = 2), 
																 overwrite = TRUE)
				eval_SDM <- getEvaluation(model_SDM, stat = c("TSS", "threshold"))
			}
			return_ls <- list(model = model_SDM,
												ensemble_SDM = ensemble_SDM,
												evalalutation = eval_SDM)
			save(return_ls, file = FNAMEInner)
		}
		return_ls
	})
	save(SDMModel_ls, file = FNAME)
	unlink(Dir.Temp)
	SDMModel_ls
}

# SDM EVALUATION FUNCTION --------------------------------------------------