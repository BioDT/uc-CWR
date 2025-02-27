# uc-CWR

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

As an addition to ModGP, you can run two of Capfitogen's most useful tools: ELC maps and Complementa maps to visualise overlap with protected areas.

- To run our version of CAPFITOGEN in [RStudio](https://posit.co/downloads/), open `capfitogen_master.R` and execute the code, changing inputs like species name and other parameters. The script guides you through the whole process.

- To run on LUMI: obtain interactive session:
   `srun -p small --nodes=1 --ntasks-per-node=1 --mem=8G -t 4:00:00 --pty bash`
   and execute the workflow:
   `singularity run --bind $PWD cwr_0.2.0.sif capfitogen_master.R`

