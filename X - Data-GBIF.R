#' ####################################################################### #
#' PROJECT: [BioDT CWR] 
#' CONTENTS: 
#'  - GBIF Data Download Functionality
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
	"terra", # for alternative raster handling
	"rgbif", # for gbif access
	"sf", # for spatialfeatures
	"parallel", # for parallel runs
	"pbapply" # for parallelised apply functions and estimators
)
sapply(package_vec, install.load.package)

`%nin%` <- Negate(`%in%`) # a function for negation of %in% function

# REGISTER API CREDENTIALS -------------------------------------------------
try(source("X - PersonalSettings.R")) 
if(as.character(options("gbif_user")) == "NULL" ){
	options(gbif_user=rstudioapi::askForPassword("my gbif username"))}
if(as.character(options("gbif_email")) == "NULL" ){
	options(gbif_email=rstudioapi::askForPassword("my registred gbif e-mail"))}
if(as.character(options("gbif_pwd")) == "NULL" ){
	options(gbif_pwd=rstudioapi::askForPassword("my gbif password"))}

# GBIF DOWNLOAD FUNCTION --------------------------------------------------
# queries download from GBIF, handles and cleans data, returns SF MULTIPOINT object
FUN.DownGBIF <- function(species = NULL, # species name as character for whose genus data is to be downloaded
												Dir = getwd(), # where to store the data
												Force = FALSE # whether the download should be forced despite local data already existing
												){
	## Preparing species name identifiers
	input_species <- species
	species <- strsplit(input_species, " ")[[1]]
	
	## Filename and data presence check
	FNAME <- file.path(Dir, paste0(input_species, ".RData"))
		if(!Force & file.exists(FNAME)){
			load(FNAME)
			warning("Data has already been downloaded with these specifications previously. It has been loaded from the disk. If you wish to override the present data, please specify Force = TRUE")
			return(save_ls)
			}
	
	## Function start
	message("Starting GBIF data retrieval")
	## input validation
	if(length(species) < 2){stop("The species name supplied is likely malformed. It should contain at least two words separated by a space.")}
	
	## GBIF ID Query ----
	## GBIF query
	message(paste("## Resolving", species[1], "at genus level"))
	GBIF_match <- name_backbone(name = species[1], 
															rank = tolower("GENUS"), kingdom = "plante")
	## Extracting taxonkey
	tax_ID <- ifelse(GBIF_match$rank != "GENUS", NA, 
									 GBIF_match$usageKey[GBIF_match$rank == "GENUS"])
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
	occ_occ <- occ_occ[occ_occ$basisOfRecord %nin% c("PRESERVED_SPECIMEN", "MATERIAL_CITATION"), ]
	## removing highly uncertain locations, i.e., anything more than 1km in uncertainty
	occ_occ <- occ_occ[occ_occ$coordinateUncertaintyInMeters <= 1000, ]
	## removing rounded coordinates
	occ_occ <- occ_occ[-grep(occ_occ$issue, pattern = "COORDINATE_ROUNDED"), ]
	## removing empty species rows
	occ_occ <- occ_occ[occ_occ$species != "" & !is.na(occ_occ$species), ]
	
	### Making SF for species ----
	message("Extracting species-level data into MULTIPOINT objects")
	GBIF_specs <- unique(occ_occ$species)
	## Making a list of spatialfeatures MULTIPOINT objects denoting unique locations of presence per species
	specs_ls <- pblapply(GBIF_specs, FUN = function(x){
		spec_df <- occ_occ[occ_occ$species == x, ]
		spec_uniloca <- occ_occ[occ_occ$species == x, c("species", "decimalLatitude", "decimalLongitude")]
		spec_df <- spec_df[!duplicated(spec_uniloca), 
											 c("gbifID", "datasetKey", "occurrenceID", "species", "scientificName", "speciesKey",
											 	"decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters",
											 	"eventDate", "basisOfRecord", "recordNumber", "issue")
											 ]
		spec_df$presence <- 1
		st_as_sf(spec_df, coords = c("decimalLongitude", "decimalLatitude"))
	})
	names(specs_ls) <- GBIF_specs
	save_ls <- list(meta = occ_meta,
									 occs = specs_ls)
	save(save_ls, file = FNAME)
	unlink(occ_get) # removes .zip file
	save_ls
}


