SoilGrids Preview
-----------------

SoilGrids is a system for global digital soil mapping that uses state-of-the-art machine learning methods to map the spatial distribution of soil properties across the globe. SoilGrids prediction models are fitted using over 230 000 soil profile observations from the WoSIS database and a series of environmental covariates. Covariates were selected from a pool of over 400 environmental layers from Earth observation derived products and other environmental information including climate, land cover and terrain morphology. The outputs of SoilGrids are global soil property maps at six standard depth intervals (according to the GlobalSoilMap IUSS working group and its specifications) at a spatial resolution of 250 meters. Prediction uncertainty is quantified by the lower and upper limits of a 90% prediction interval. The SoilGrids maps are publicly available under the [CC-BY 4.0 License](https://creativecommons.org/licenses/by/4.0/).

Maps of the following soil properties are available: pH, soil organic carbon content, bulk density, coarse fragments content, sand content, silt content, clay content, cation exchange capacity (CEC), total nitrogen as well as soil organic carbon density and soil organic carbon stock.

Main improvements
-----------------

Relative to the previous, the following improvements can be highlighted:

- Wider selection of soil observations: more profile observations, increased quality assessment and improved and consistent standardisation across the different point datasets.

- Quantification of prediction uncertainty at pixel level with the 90% prediction interval using Quantile Random Forest.

- Improved model calibration and cross-validation procedure to better take into account the uneven spatial distribution of data points across the world.

- Improved covariates selection and model parameter tuning.

- Texture fractions modelled and mapped not independently from each other, but as compositional data with the sum of the fractions constrained to 100%.

How to download
---------------

In this upcoming SoilGrids release maps are in the [VRT format](https://gdal.org/drivers/raster/vrt.html). Each map is composed by three elements:

- a master VRT file;
- an OVR file with overviews for swift visualisation;
- a folder with GeoTIFF tiles.

All elements have the same name, a triplet separated by underscores: `property_depthInterval_quantile` For instance, to download the the 5%-quantile prediction of coarse fragments in the 5 cm to 15 cm depth interval the user must fetch the files `cfvo_5-15cm_Q05.vrt` and `cfvo_5-15cm_Q05.ovr` plus the `cfvo_5-15cm_Q05` folder. 

Each map occupies circa 5 GB. The full collection of maps for a single property, with six 
standard depth intervals and four quantiles per depth requires about 120 GB.

Web Services
------------

The SoilGrids maps are also available through OGC web services. These are preferrable for users interested only on mapping or on a restricted area of the globe. Visit the [OGC services catalogue](https://maps.isric.org) page to explore further.

Questions
---------

If questions arise, start by visiting the [SoilGrids FAQ](https://www.isric.org/explore/soilgrids/faq-soilgrids). If it does not answer your question, you may post a question to [GIS.StackExchange](https://gis.stackexchange.com/) under the tag [soilgrids](https://gis.stackexchange.com/tags/soilgrids). ISRIC staff are subscribed to this tag and will be automatically notified of any new question.


Contact
-------

Soilgrids team: soilgrids@isric.org
