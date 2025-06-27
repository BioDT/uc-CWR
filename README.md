# uc-CWR

## Initial setup

1. Create a file `R_scripts/SHARED-APICredentials.R`. You can use [the example file](R_scripts/SHARED-APICredentials.example.R) as a template. Insert your [CDS API key](https://cds.climate.copernicus.eu/how-to-api) and [GBIF credentials](https://www.gbif.org/) in this file.

## ModGP on Rstudio

1. Source `ModGP_MASTER.R` and change `SPECIES` argument at line 19 to execute ModGP pipeline for a specific genus.

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


## CAPFITOGEN demo

See [documentation](https://www.capfitogen.net/en).

1. Download `CAPFITOGEN3.zip` from
   [here](https://drive.google.com/file/d/1EJw-XcC1NRVFS7mwzlg1VpQBpRCdfWRd/view?usp=sharing)
   and extract it to the project root.

2. Download `rdatamaps/world/20x20` directory from
   [here](https://drive.google.com/drive/folders/19bqG_Z3aFhzrCWQp1yWvMbsLivsCicHh)
   and extract it to `CAPFITOGEN3/rdatamaps/world/20x20`.

3. Run on LUMI: obtain interactive session:
   `srun -p small --nodes=1 --ntasks-per-node=1 --mem=8G -t 4:00:00 --pty bash`
   and execute the workflow:
   `singularity run --bind $PWD cwr_0.2.0.sif capfitogen.R`

# Related publication
Chala D, Kusch E, Weiland C, Andrew C, Grieb J, Rossi T, Martinovic T, Endresen D (2024) Prototype biodiversity digital twin: crop wild relatives genetic resources for food security. Research Ideas and Outcomes 10: e125192. https://doi.org/10.3897/rio.10.e125192

# Archived at Zenodo

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15696971.svg)](https://doi.org/10.5281/zenodo.15696971)
