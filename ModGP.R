#' ####################################################################### #
#' PROJECT: [BioDT CWR - ModGP] 
#' CONTENTS: 
#'  - Purpose of this code document
#'  DEPENDENCIES:
#'  - Code documents needed to execute this document
#'  - Data files
#' AUTHOR: [Desalegn Chala Gelete, Erik Kusch]
#' ####################################################################### #

# PREAMBLE ================================================================
rm(list=ls()) # clearing the work environment
set.seed(42) # making things reproducibly random

## Directories ------------------------------------------------------------
### Define dicrectories in relation to project directory
Dir.Base <- getwd()
Dir.Data <- file.path(Dir.Base, "Data")
Dir.Exports <- file.path(Dir.Base, "Exports")
### Create directories which aren't present yet
Dirs <- grep(ls(), pattern = "Dir.", value = TRUE)
CreateDir <- sapply(Dirs, function(x){
	x <- eval(parse(text=x))
	if(!dir.exists(x)) dir.create(x)})

## Packages ---------------------------------------------------------------
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
	"parallel" # for parallel sdm runs
)
sapply(package_vec, install.load.package)

## Functionality ----------------------------------------------------------
`%nin%` <- Negate(`%in%`) # a function for negation of %in% function

# DATA ====================================================================
## GBIF Data --------------------------------------------------------------
## species of interest
spec_name <- "Lathyrus sativus"
### register API credentials
try(source("X - PersonalSettings.R")) 
if(as.character(options("gbif_user")) == "NULL" ){
	options(gbif_user=rstudioapi::askForPassword("my gbif username"))}
if(as.character(options("gbif_email")) == "NULL" ){
	options(gbif_email=rstudioapi::askForPassword("my registred gbif e-mail"))}
if(as.character(options("gbif_pwd")) == "NULL" ){
	options(gbif_pwd=rstudioapi::askForPassword("my gbif password"))}
## find data in GBIF backbone
tax_ID <- name_backbone(name = strsplit(spec_name, " ")[[1]][1], 
												rank = "genus", kingdom = "plante")$usageKey # to obtain taxon key
occ_count(taxonKey = tax_ID) # number of occurrences at genus level

### Download ######################################
spec_down <- occ_download(pred("taxonKey", tax_ID), 
														 pred("hasCoordinate", TRUE),
														 format = "SIMPLE_CSV")
curlopts=list(http_version=2) # needed on Mac to avoid HTTP issues in next line (see here: https://github.com/ropensci/rgbif/issues/579)
spec_meta <- occ_download_wait(spec_down, status_ping = 30, 
															 curlopts = list(), quiet = FALSE) # wait for download to finish
spec_get <- occ_download_get(spec_down, path = Dir.Data) # download data

### Loading ######################################
spec_occ <- occ_download_import(spec_get) # import downloaded data

### Manipulation ######################################
spec_uniloca <- spec_occ[ ,c("species","decimalLatitude", "decimalLongitude")]
spec_uniloca <- spec_uniloca[!duplicated(spec_uniloca), ]
spec_uniloca$presence <- 1

#### Pseudo-Absences
#' to generate pseudo-absence points. 1e5 randomly selected points where the model target is absent but other species are present this can be further modified - for example limiting it by minimum convex that encompasses only 90% of the occurrence data 10% of the distant occurrence points won't be considered 
#' Absences
Abs_ls <- lapply(unique(spec_uniloca$species), FUN = function(x){
	NotSpecies_df <- spec_uniloca[spec_uniloca$species != x, ]
	SampledAbs <- sample(1:nrow(NotSpecies_df), size = 1e5, replace = FALSE)
	Report_df <- NotSpecies_df[SampledAbs,c("decimalLongitude", "decimalLatitude")]
})
names(Abs_ls) <- unique(spec_uniloca$species)
Abs_ls_presence <- mapply(cbind, Abs_ls, "presence" = 0, SIMPLIFY = FALSE)
spec_absen <- Abs_ls_presence[[which(names(Abs_ls_presence) == spec_name)]]
spec_absen$species <- spec_name
colnames(spec_absen) <- c("lon", "lat", "presence", "species")
#' Presences
spec_uniloca_presence <- cbind(spec_uniloca, "presence" = 1)
spec_uniloca_presence <- spec_uniloca_presence[,c(2:4,1)]
colnames(spec_uniloca_presence) <- c("lon","lat","presence", "species")
spec_presen <- spec_uniloca_presence[spec_uniloca_presence$species == spec_name, ]
spec_PA <- rbind(spec_presen,spec_absen)

### Saving ######################################
saveRDS(spec_occ, file.path(Dir.Data, paste0(spec_name, "_raw.rds"))) # store as data file
unlink(strsplit(spec_get, "<<gbif downloaded get>> Path:")[[1]][1]) # remove zip file
saveRDS(spec_uniloca, file.path(Dir.Data, paste0(spec_name, "_UniLoca.rds")))
saveRDS(spec_PA, file.path(Dir.Data, paste0(spec_name, "_PA.rds")))

## Climate Data -----------------------------------------------------------
### Download ######################################
bio2.5 <- raster::getData('worldclim', var = 'bio', res = 2.5, path = Dir.Data) 
unlink(list.files(path = file.path(Dir.Data, "wc2-5"), pattern = ".zip", full.names = TRUE))

### Loading ######################################
bio2.5_stack <- list.files(path = file.path(Dir.Data, "wc2-5"), pattern = "bil", full.names = TRUE)
bio2.5_stack <- raster::stack(bio2.5_stack)
add_ras <- stack(list(pH = raster("https://files.isric.org/soilgrids/latest/data/phh2o/phh2o_15-30cm_mean.vrt"), 
		 nitrogen = raster("https://files.isric.org/soilgrids/latest/data/nitrogen/nitrogen_15-30cm_mean.vrt")
		 ))

### Manipulation ######################################
#' This does not work for some reason, need to troubleshoot more
# add_ras2 <- projectRaster(from = add_ras, 
# 													to = bio2.5_ras
#													# crs = projection(bio2.5_stack)
# 													)
abio_stack <- bio2.5_stack
# abio_stack <- stack(bio2.5_stack, add_ras2)

### Saving ######################################
save(abio_stack, file = file.path(Dir.Data, paste0("Abio_stack.RData")))

# ANALYSIS ================================================================
if("maxent" %nin% unlist(getmethodNames())){sdm::installAll()} # install methods for sdm package

## Multicollinearity ------------------------------------------------------
v <- usdm::vifcor(terra::rast(abio_stack), th = 0.7) # variable inflation factor
biomod <- exclude(abio_stack,v) # now exclude those with high cor and vif
#' individual point climate 
spec_PA_sp <- spec_PA
coordinates(spec_PA_sp) <- c("lon", "lat")
# bg <- spec_PA_sp[spec_PA_sp$presence == 0, ]
ex <- raster::extract(abio_stack, spec_PA_sp)

## SDM Inputs -------------------------------------------------------------
pred <- ex[ ,names(biomod)]
pred <- cbind(spec_PA, pred)

d <- sdmData(~.+coords(lon+lat), train = pred)  

## SDM Execution -----------------------------------------------------------
m <- sdm(~., d, methods = c ("maxent","gbm","GAM","RF"), 
				replications = c("sub", "boot"), test.p = 25, n = 3,
				parallelSetting = list(ncore = 4, method = "parallel"))

en <- ensemble(m , bio, filename = "enpr.img", 
							 setting = list(method = "weighted", stat = "tss", opt = 2), 
							 overwrite = TRUE)
ev <- getEvaluation(m, stat = c("TSS","threshold"))

## SDM Prediction ----------------------------------------------------------
p1 <- predict(m, biomod)

# EXPORT ==================================================================
## Results ----------------------------------------------------------------
save(p1, file = file.path(Dir.Data, paste0(spec_name, "_pred.Rdata")))
save(ev, file = file.path(Dir.Data, paste0(spec_name, "_eval.Rdata")))

## Plotting ---------------------------------------------------------------
## Data -------------------------------------------------------------------