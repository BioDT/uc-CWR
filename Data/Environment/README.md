# Environmental data

downloaded with functions run in capfitogen_master.R and defined in SHARED-Data.R. 

## Bioclimatic variables (FUN.DownBV)

A set of 19 bioclimatic variables, downloaded and processed with the KrigR package.

## Capfitogen's set of environmental variables (FUN.DownCAPFITOGEN)

A collection of publicly available environmental data: bioclimatic, edaphic, and geophysical variables from e.g. WorldClim, SoildGrids and other sources collected in a google drive. The function downloads the data and collects it in a NetCDF (.nc) file. 

### Drafted download functions not currently in use: 

**Edaphic variables (EV)**

Soil data downloaded from SoilGrids. Each map occupies ~ 5 GB. "SoilGrids is a system for global digital soil mapping that uses state-of-the-art machine learning methods to map the spatial distribution of soil properties across the globe. SoilGrids prediction models are fitted using over 230 000 soil profile observations from the WoSIS database and a series of environmental covariates. Covariates were selected from a pool of over 400 environmental layers from Earth observation derived products and other environmental information including climate, land cover and terrain morphology. The outputs of SoilGrids are global soil property maps at six standard depth intervals (according to the GlobalSoilMap IUSS working group and its specifications) at a spatial resolution of 250 meters. Prediction uncertainty is quantified by the lower and upper limits of a 90% prediction interval. The SoilGrids maps are publicly available under the [CC-BY 4.0 License](https://creativecommons.org/licenses/by/4.0/). Maps of the following soil properties are available: pH, soil organic carbon content, bulk density, coarse fragments content, sand content, silt content, clay content, cation exchange capacity (CEC), total nitrogen as well as soil organic carbon density and soil organic carbon stock." See [SoilGrids FAQ](https://www.isric.org/explore/soilgrids/faq-soilgrids).

**Geophysical variables (GV)**

