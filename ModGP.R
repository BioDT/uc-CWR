#' ####################################################################### #
#' PROJECT: [BioDT CWR - ModGP] 
#' CONTENTS: 
#'  - Purpose of this code document
#'  DEPENDENCIES:
#'  - Code documents needed to execute this document
#'  - Data files
#' AUTHOR: [Erik Kusch]
#' ####################################################################### #

# PREAMBLE ================================================================
rm(list=ls()) # clearing the work environment
set.seed(42) # making things reproducibly random

## Directories ------------------------------------------------------------
### Define dicrectories in relation to project directory
Dir.Base <- getwd()
Dir.Data <- file.path(Dir.Base, "Data")
Dir.Exports <- file.path(Dir.Base, "Exports")
Dir.Data.GBIF <- file.path(Dir.Data, "GBIF")
Dir.Data.Envir <- file.path(Dir.Data, "Environment")
### Create directories which aren't present yet
Dirs <- grep(ls(), pattern = "Dir.", value = TRUE)
CreateDir <- sapply(Dirs, function(x){
	x <- eval(parse(text=x))
	if(!dir.exists(x)) dir.create(x)})

## Packages ---------------------------------------------------------------
### CRAN PACKAGES ----
install.load.package <- function(x) {
	if (!require(x, character.only = TRUE))
		install.packages(x, repos='http://cran.us.r-project.org')
	require(x, character.only = TRUE)
}
package_vec <- c(
	"raster", # for raster object handling
	"terra", # for alternative raster handling
	"rgbif", # for gbif access
	"sdm", # for sdm models
	"usdm", # for vifcor
	"sp", # for spatialpoints
	"data.table", # for rbindlist
	"parallel", # for parallel sdm runs
	"rnaturalearth" # for landmask
)
sapply(package_vec, install.load.package)

### NON-CRAN PACKAGES ----
if("flexsdm" %in% rownames(installed.packages()) == FALSE){ # flexsdm check
	Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
	devtools::install_github("sjevelazco/flexsdm")
}
library(flexsdm)

if("KrigR" %in% rownames(installed.packages()) == FALSE){ # KrigR check
	Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
	devtools::install_github("ErikKusch/KrigR")
}
library(KrigR)

## Functionality ----------------------------------------------------------
source("X - Data-GBIF.R")
`%nin%` <- Negate(`%in%`) # a function for negation of %in% function

# DATA ====================================================================
## GBIF Data --------------------------------------------------------------
## species of interest
Species_ls <- FUN.DownGBIF(
	species = "Lathyrus sativus",
	Dir = Dir.Data.GBIF,
	Force = FALSE)
Species_sf <- do.call(rbind, Species_ls$occs)

## Environmental Data -----------------------------------------------------
### Climatology time settings ----
T_Start <- 1985
T_End <- 2015
Month_seq <- seq(as.Date(paste0(T_Start, "-01-01")), as.Date(paste0(T_End, "-12-31")), by = "month")
Month_seq <- strsplit(x = as.character(Month_seq), split = "-")



### Raw soil moisture level data ----
#' We download raw soil moisture data for layers 1 (0-7cm) and 2 (7-28cm) separately. These are then summed up and used in the bioclimatic variable computation of KrigR
if(length(list.files(Dir.Data.Envir, pattern = "volumetric_soil_water_layer_1")) != length(Month_seq)){
	#### Downloading ####
	Qsoil1_ras <- download_ERA(
		Variable = "volumetric_soil_water_layer_1",
		DataSet = "era5",
		DateStart = paste0(T_Start, "-01-01"),
		DateStop = paste0(T_End, "-12-31"),
		Dir = Dir.Data.Envir,
		FileName = "Qsoil1",
		API_User = API_User,
		API_Key = API_Key,
		SingularDL = TRUE,
		TimeOut = Inf
	)
	
	Qsoil2_ras <- download_ERA(
		Variable = "volumetric_soil_water_layer_2",
		DataSet = "era5",
		DateStart = paste0(T_Start, "-01-01"),
		DateStop = paste0(T_End, "-12-31"),
		Dir = Dir.Data.Envir,
		FileName = "Qsoil2",
		API_User = API_User,
		API_Key = API_Key,
		SingularDL = TRUE
	)
	
	#### Combining ####
	QSoilCombin_ras <- rast(Qsoil1_ras)+rast(Qsoil2_ras)
	
	#### Saving ####
	for(MonthSave_iter in 1:nlyr(QSoilCombin_ras)){
		FNAME <- file.path(Dir.Data.Envir, paste0("volumetric_soil_water_layer_1-mean-", Month_seq[[MonthSave_iter]][1], "_", Month_seq[[MonthSave_iter]][2], "MonthlyBC.nc"))
		terra::writeCDF(QSoilCombin_ras[[MonthSave_iter]], filename = FNAME, overwrite = TRUE)
	}
	
	### Deleting unnecessary files ####
	unlink(list.files(Dir.Data.Envir, pattern = "Qsoil", full.names = TRUE))
}

### Bioclimatic data ----
if(file.exists(file.path(Dir.Data.Envir, "Qsoil_BC.nc"))){
	BV_ras <- stack(file.path(Dir.Data.Envir, "Qsoil_BC.nc"))
}else{
	BV_ras <- BioClim(
		DataSet = "era5",
		Water_Var = "volumetric_soil_water_layer_1",
		Y_start = T_Start,
		Y_end = T_End,
		Dir = Dir.Data.Envir,
		Keep_Monthly = TRUE,
		FileName = "Qsoil_BC.nc",
		API_User = API_User,
		API_Key = API_Key,
		Cores = numberOfCores,
		TimeOut = Inf,
		SingularDL = FALSE
	)
}
names(BV_ras) <- paste0("BIO", 1:19)

### Masking ####
Land_sp <- ne_countries(type = "countries", scale = "medium")
BV_ras <- crop(BV_ras, extent(Land_sp))
BV_ras <- mask(BV_ras, Land_sp)

## SDM Data Preparations --------------------------------------------------
SDMInput_ls <- pblapply(Species_ls$occs, FUN = function(SDMData_iter){
	# SDMData_iter <- Species_ls$occs[["Lathyrus magellanicus"]]
	
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
names(SDMInput_ls)
save(SDMInput_ls, file = file.path(Dir.Data, 
											 paste0(strsplit(names(Species_ls$occs)[1], split = " ")[[1]][1], 
											 			 ".RData")))

# ANALYSIS ================================================================
if("maxent" %nin% unlist(getmethodNames())){sdm::installAll()} # install methods for sdm package

SDMModel_ls <- pblapply(SDMInput_ls, FUN = function(SDMModel_Iter){
	species_iden <- unique(PA_df$species[PA_df$PRESENCE == 1])
	FNAME <- file.path(Dir.Exports, paste0("SDM_", species_iden,".RData"))
	print(species_iden)
	
	if(file.exists(FNAME)){
		load(FNAME)
	}else{
		SDMModel_Iter <- SDMInput_ls[[1]]
		data_SDM <- SDMModel_Iter$SDMData
		PA_df <- SDMModel_Iter$PA
		if(sum(PA_df$PRESENCE) < 41){
			model_SDM <- NA
			ensemble_SDM <- NA
			eval_SDM <- NA
			prediction_SDM <- NA
		}else{
			model_SDM <- sdm(~., data_SDM, methods = c ("maxent","gbm","GAM","RF"), 
											 replications = c("sub", "boot"), test.p = 25, n = 3,
											 parallelSetting = list(ncore = 4, method = "parallel"))
			ensemble_SDM <- ensemble(model_SDM, BV_ras, filename = "enpr.img", 
															 setting = list(method = "weighted", 
															 							 stat = "tss", opt = 2), 
															 overwrite = TRUE)
			eval_SDM <- getEvaluation(model_SDM, stat = c("TSS", "threshold"))
			prediction_SDM <- predict(model_SDM, BV_ras)
		}
		return_ls <- list(model = model_SDM,
											ensemble_SDM = ensemble_SDM,
											evalalutation = eval_SDM,
											prediction = prediction_SDM)
		save(return_ls, file = FNAME)
	}
	return_ls
})


# EXPORT ==================================================================
## Results ----------------------------------------------------------------
save(p1, file = file.path(Dir.Data, paste0(spec_name, "_pred.Rdata")))
save(ev, file = file.path(Dir.Data, paste0(spec_name, "_eval.Rdata")))

## Plotting ---------------------------------------------------------------
## Data -------------------------------------------------------------------