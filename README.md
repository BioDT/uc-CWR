# uc-CWR

## ModGP on Rstudio

1. Source `ModGP MASTER.R` and change `SPECIES` argument at line 19 to execute ModGP pipeline for a specific genus.


## ModGP on LUMI with Hyperqueue

1. Install HyperQueue:

       wget https://github.com/It4innovations/hyperqueue/releases/download/v0.18.0/hq-v0.18.0-linux-x64.tar.gz
       tar -xvf hq-v0.18.0-linux-x64.tar.gz
       rm hq-v0.18.0-linux-x64.tar.gz

2. Submit calculation job for a desired species (e.g. Lathyrus):

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

