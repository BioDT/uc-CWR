# Use case Crop Wild Relatives (uc-CWR)

This repository hosts code for a [Biodiversity Digital Twin](https://biodt.eu/) use case: the prototype Digital Twin for [Crop Wild Relatives](https://biodt.eu/use-cases/crop-wild-relatives). The prototype Digital Twin can be accessed through a grapical user interface made with R shiny and hosted on Lifewatch: [prototype digital twins GUI](http://app.biodt.lifewatch.eu/)

> *"The Prototype Biodiversity Digital Twin (pDT) for Crop Wild Relatives is an advanced tool designed to aid in the identification and use of crop wild relatives (CWR) genetic resources to enhance crop resilience against climate-driven stresses"* [BioDT.eu/use-cases/crop-wild-relatives](https://biodt.eu/use-cases/crop-wild-relatives)

For technical documentation, see a separate [markdown file](technical_documentation.md). Below we also outline quick instructions for running the ModGP and Capfitogen tools in R and on the LUMI supercomputer. The prototype Digital Twin is also presented in a 'Research ideas and outcomes' paper: [Chala et al. 2024](https://doi.org/10.3897/rio.10.e125192). The core functionality of the digital twin is ModGP (Modelling the GermPlasm of interest), but two of Capfitogen's tools have since been added to extend the prototype Digital Twin's usefulness.

> *"MoDGP leverages species distribution modelling, relying on occurrence data of CWR to produce habitat suitability maps, establish mathematical correlations between adaptive traits, such as tolerance to drought and pathogens and environmental factors and facilitates mapping geographic areas where populations possessing genetic resources for resilience against various biotic and abiotic stresses are potentially growing."* [Chala et al. 2024](https://doi.org/10.3897/rio.10.e125192)

---------------------------------

## Initial setup

1. Create a file `R_scripts/SHARED-APICredentials.R`. You can use [the example file](R_scripts/SHARED-APICredentials.example.R) as a template. Insert your [CDS API key](https://cds.climate.copernicus.eu/how-to-api) and [GBIF credentials](https://www.gbif.org/) in this file.

## ModGP on Rstudio

1. Source `ModGP_MASTER.R` and change `SPECIES` argument at line 19 to execute ModGP pipeline for a specific genus. NB! ModGP should be run on a supercomputer. The environmental data download has very large interim files (>40GB per year per variable, >200 GB overall), and the distribution modelling also requires a long time to run.

## ModGP on LUMI with Hyperqueue

1. Install HyperQueue:

       wget https://github.com/It4innovations/hyperqueue/releases/download/v0.18.0/hq-v0.18.0-linux-x64.tar.gz
       tar -xvf hq-v0.18.0-linux-x64.tar.gz
       rm hq-v0.18.0-linux-x64.tar.gz

2. Fetch the container:

       singularity pull --disable-cache docker://ghcr.io/biodt/cwr:0.5.3

3. Submit calculation job for a desired species (e.g. Lathyrus):

       sbatch submit_modgp_prep_lumi.sh Lathyrus
       sbatch submit_modgp_exec_lumi_HQ.sh Lathyrus


## CAPFITOGEN

As an addition to ModGP, you can run two of [Capfitogen](https://www.capfitogen.net/en/)'s most useful tools: [ecogeographic land characterization (ELC) maps](https://www.capfitogen.net/en/tools/elc-mapas/) and [Complementa](https://www.capfitogen.net/en/tools/complementa/) maps to visualise overlap with protected areas.
Because a lot of variables will be downloaded and processed, the total memory requirements may be too large for most personal computers. Try with a subset of the data if necessary. 

NB! After cloning this repository, you need to clone Capfitogen (a submodule) as well with `git submodule update --init`. 

Alternative ways of running the capfitogen capabilities:

- To run our version of CAPFITOGEN in [RStudio](https://posit.co/downloads/), open `capfitogen_master.R` and execute the code, changing inputs like species name and other parameters. The script guides you through the whole process. After changing the species name, you can run the whole script as a background job if desired.

- To run on LUMI (assumes access to LUMI and the project): 

1. Fetch the container: `singularity pull --disable-cache docker://ghcr.io/biodt/cwr:0.6.0`
2. then submit the job for a desired species (e.g. Lathyrus):

       sbatch submit_capfitogen_prep_lumi.sh Lathyrus
       sbatch submit_capfitogen_exec_lumi.sh Lathyrus

# Related publication
Chala D, Kusch E, Weiland C, Andrew C, Grieb J, Rossi T, Martinovic T, Endresen D (2024) Prototype biodiversity digital twin: crop wild relatives genetic resources for food security. Research Ideas and Outcomes 10: e125192. https://doi.org/10.3897/rio.10.e125192

# Archived at Zenodo

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15696971.svg)](https://doi.org/10.5281/zenodo.15696971)
