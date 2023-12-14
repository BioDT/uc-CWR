rm(list =ls())

library(intSDM)
library(giscoR)

source_lines <- function(file, lines){
	source(textConnection(readLines(file)[lines]))
}
source_lines("ModGP.R", 1:90)

# PRESENCE/ABSENCE -------
Occ_df <- SDMInput_ls$`Lathyrus vernus`$PA
spec_name <- unique(Occ_df$species[Occ_df$PRESENCE == 1])
Occ_df$modelSpec <- spec_name

# COVARIATES -------
cov <- rast('Data/Environment/BV_1985-2015.nc')
vars <- colnames(Occ_df)[startsWith(colnames(Occ_df), prefix = "BIO")]
cov <- cov[[as.numeric(gsub(".*?([0-9]+).*", "\\1", vars))]]
names(cov) <- vars

# SDM WORKFLOW -------

## Workflow Setup ----
### base ----
workflow <- startWorkflow(
	Projection = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
	Species = unique(Occ_df$modelSpec),
	saveOptions = list(projectName =  "ISDMs"), Save = TRUE
)

### area ----
Globe_sf <- gisco_get_countries()
workflow$addArea(Object = Globe_sf)

### presence/absence ----
Occ_sf <- st_as_sf(Occ_df, crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
Occ_sf <- st_transform(Occ_sf, crs = st_crs(cov))
workflow$addStructured(dataStructured = Occ_sf, datasetType = 'PA',
											 responseName = 'PRESENCE',
											 speciesName = 'modelSpec',
											 coordinateNames = c('lon', 'lat'))
workflow$plot(Species = TRUE)

### covariates ----
workflow$addCovariates(Object = cov)

### mesh ----
load("GlobalAreaCRS.RData")
workflow$.__enclos_env__$private$Area <- st_transform(workflow$.__enclos_env__$private$Area, crsto)

workflow$addMesh(cutoff = 20000,
								 max.edge = c(60000, 80000),
								 offset= 100000)

### priors ----
workflow$specifySpatial(prior.range = c(300000, 0.05),
												prior.sigma = c(50, 0.2))

### cross-validation ----
workflow$crossValidation(Method = 'Loo')

### outputs ----
workflow$workflowOutput(c("Model", "Predictions", 'Maps'))

### INLA options ----
workflow$modelOptions(INLA = list(control.inla=list(int.strategy = 'eb',
																										cmin = 0),
																	safe = TRUE,
																	inla.mode = 'experimental'))

## Execution ----
Model_iSDM <- sdmWorkflow(workflow)

## Output ----
Model_iSDM # should this be NULL?

Predictions <- readRDS(file.path(getwd(), "ISDMs", gsub(spec_name, pattern = " ", replacement = "_"), "Predictions.rds"))
intModel <- readRDS(file.path(getwd(), "ISDMs", gsub(spec_name, pattern = " ", replacement = "_"), "intModel.rds"))

test <- Predictions$predictions
class(test) <- class(test)[-1]

plot(exp(rasterize(test, cov[[1]], field = "mean")))


# extracting trial data for Tomasz ----
library(raster)
library(stars)
library(tidyterra)
library(cowplot)

## Model output rasters ----
preds <- exp(rast((st_rasterize(test %>% dplyr::select(mean, geometry)))))
preds <- resample(preds, cov[[1]])
preds_filled <- focal(preds, 21, "modal", na.policy="only")
preds_masked <- mask(preds_filled, cov[[1]])

suitability_ras <- preds_masked 
binarised_ras <- preds_masked > 1

modelled_ras <- c(suitability_ras, binarised_ras)
names(modelled_ras) <- c("Suitability", "Predicted Presence/Absence")

## Driver rasters ----
PH_nutrient <- rast("https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq1.asc")
PH_toxicity <- rast("https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq6.asc")
drivers_ras <- c(PH_nutrient, PH_toxicity)
names(drivers_ras) <- c("Nutrient", "Toxicity")

drivers_ras <- resample(drivers_ras, modelled_ras)

## Model inputs ----
presences_df <- Occ_df[Occ_df$PRESENCE == 1, ]
presences_sf <- st_as_sf(presences_df)
absences_df <- Occ_df[Occ_df$PRESENCE == 0, ]
absences_sf <- st_as_sf(absences_df)

buffer_sf <- st_union(st_buffer(presences_sf["species"], 15)) # 15 degree buffer around points
st_crs(buffer_sf) <- st_crs(modelled_ras)

## Plotting ----
background_ras <- !is.na(modelled_ras[[1]])
background_ras <- terra::crop(background_ras, st_bbox(buffer_sf)+c(-5, -5, 5, 5))
Occ_df$PRESENCE[Occ_df$PRESENCE == 0] <- "Absence"
Occ_df$PRESENCE[Occ_df$PRESENCE == 1] <- "Presence"

IN_gg <- ggplot() + geom_spatraster(data = background_ras) + 
	scale_fill_manual(values = c("#000a63", "#333333")) + 
	geom_point(aes(x = lon, y = lat, color = factor(PRESENCE)), data = Occ_df, size = 0.1, pch = 4, alpha = 0.2) +
	scale_color_manual(values = c("red", "white"), name = "Presence / Absence") + 
	geom_sf(data = buffer_sf, colour = 'black', size = 0.2, fill = 'green', alpha = .05) + 
	labs(title = "ModGP Observation Inputs", x = "Longitude [°]", y = "Latitude [°]") + 
	# theme_bw() +
	guides(fill = "none")
	
SM_gg <- ggplot() + geom_spatraster(data = modelled_ras$Suitability) + 
	scale_fill_viridis_c(name = "Suitability") + 
	labs(title = "Modelled Habitat Suitability", x = "Longitude [°]", y = "Latitude [°]") + 
	theme_bw()

BN_gg <- ggplot() + geom_spatraster(data = modelled_ras$`Predicted Presence/Absence`) + 
	scale_fill_viridis_d(name = "Presence/Absence") + 
	labs(title = "Modelled Species Presence", x = "Longitude [°]", y = "Latitude [°]") + 
	theme_bw()

cowplot::plot_grid(IN_gg,
									 cowplot::plot_grid(SM_gg, BN_gg, ncol = 2),
									 ncol = 1
)
	
save(Occ_df, presences_sf, absences_sf, buffer_sf,
		 modelled_ras, drivers_ras, file = "Tomasz.RData")
