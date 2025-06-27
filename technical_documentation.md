# Technical documentation use-case Crop Wild Relatives

> NB! This document is under construction.

Scientific usefulness, written in R to be run on an hpc for a crop species or genus.

## ModGP

Main characteristics, script and workflow structure

|File | Description |
| --- | ----------- |

### Inputs

### Outputs

----------------------

## Capfitogen

Main characteristics, script and workflow structure.

> Disclaimer: CAPFITOGEN is separate software that was created by others. It is not owned by BioDT, but this prototype digital twin uses a subset of available scripts and functionality from CAPFITOGEN. See Parra-Quijano et al. 2021, <https://repositorio.unal.edu.co/handle/unal/85787> and [CAPFITOGEN.net](https://www.capfitogen.net/en/).

Some files are shared between ModGP and capfitogen scripts (R_scripts/SHARED-Data.R and ModGP-commonlines.R). These files are specific only to capfitogen code:

|File | Description |
| --- | ----------- |
| Capfitogen | the Capfitogen code repository. A submodule (repository within repository). |
| capfitogen_master.R | Main code for setting up environment, downloading data, and executing capfitogen tools. |
| submit_capfitogen_prep_lumi.sh | bash script to initialise capfitogen submodule and add a workaround fo memory issue. |
| submit_capfitogen_exec_lumi.sh | bash script to execute (run) capfitogen_master.R on LUMI. |

![Figure: Illustration of scripts and data for running Capfitogen tools](capfitogen_master_illustration.drawio.svg)
*Illustration of scripts and data for running the Capfitogen pipeline. The master script connects to data and other scripts through download functions and sourcing. To run on LUMI, the master script is executed through bash scripts.*

### Inputs

- Species occurrences, taken from download by ModGP if available
- Environmental variables downloaded from CAPFITOGEN's google drive: 177 bioclimatic, edaphic (soil), and geophysical predictors. These are narrowed down using Variable Inflation Factor.
- protected areas, vector map, by the [The World Database on Protected Areas (WDPA)](https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA)

### Outputs

- ELC maps created with CAPFITOGEN
- Complementa analysis created with CAPFITOGEN
