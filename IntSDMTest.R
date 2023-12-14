rm(list =ls())

library(intSDM)
library(giscoR)

# PRESENCE/ABSENCE -------
load('Philip.RData')
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
load(file.path("Data", "GlobalAreaCRS.RData"))
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

## Model output rasters ----
preds <- exp(rast((st_rasterize(test %>% dplyr::select(mean, geometry)))))
preds <- resample(preds, cov[[1]])
preds_filled <- focal(preds, 21, "modal", na.policy="only")
preds_masked <- mask(preds_filled, cov[[1]])

suitability_ras <- preds_masked 
binarised_ras <- preds_masked > 0.5

modelled_ras <- c(suitability_ras, binarised_ras)
names(modelled_ras) <- c("Suitability", "Predicted Presence/Absence")

## Driver rasters ----
PH_nutrient <- rast("https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq1.asc")
PH_toxicity <- rast("https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq6.asc")
drivers_ras <- c(PH_nutrient, PH_toxicity)
names(drivers_ras) <- c("Nutrient", "Toxicity")

drivers_ras <- resample(drivers_ras, presence_ras)

## Model inputs ----
presences_df <- Occ_df[Occ_df$PRESENCE == 1, ]
presences_sf <- st_as_sf(presences_df, crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

absences_df <- Occ_df[Occ_df$PRESENCE == 0, ]
absences_sf <- st_as_sf(absences_df, crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

buffer_sf <- st_union(st_buffer(st_as_sf(presences_sf), 15)) # 15 degree buffer around points

## Plotting ----

ggplot() + 
	geom_spatraster(
		data = modelled_ras) + 
	facet_wrap(~lyr, ncol = 2) 

	geom_point(aes(x = lon, y = lat, color = factor(PRESENCE)), data = Occ_df, size = 1, pch = 4) +
	scale_color_manual(values = c("black", "white")) +
	geom_polygon(aes(x = long, y = lat, group = id), data = fortify(as_Spatial(buffer_sf)),
							 colour = 'black', size = 0.2, fill = 'black', alpha = .1)

	
save(Occ_df, presences_sf, absences_sf, buffer_sf,
		 modelled_ras, drivers_ras)