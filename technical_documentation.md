# Technical documentation use-case Crop Wild Relatives

> NB! This document is under construction.

Scientific usefulness, written in R to be run on an hpc for a crop species or genus.

## ModGP

Main characteristics, script and workflow structure

### Inputs

### Outputs

## Capfitogen

Main characteristics, script and workflow structure.

![Figure: Illustration of scripts and data for running Capfitogen tools](capfitogen_master_illustration.drawio.svg)

### Inputs

- Species occurrences, taken from download by ModGP if available
- Environmental variables downloaded from CAPFITOGEN's google drive: 177 bioclimatic, edaphic (soil), and geophysical predictors. These are narrowed down using Variable Inflation Factor.
- protected areas, vector map, by the [The World Database on Protected Areas (WDPA)](https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA)

### Outputs

- ELC maps created with CAPFITOGEN
- Complementa analysis created with CAPFITOGEN
