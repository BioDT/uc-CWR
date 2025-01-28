#!/bin/bash -l
#SBATCH -J ModGP
#SBATCH -o modgp-%j.out
#SBATCH --account=project_465000915
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=128
#SBATCH --time=04:00:00
#SBATCH --partition=largemem
##SBATCH --partition=lumid
##SBATCH --partition=small --mem-per-cpu=2G
##SBATCH --partition=standard --exclusive --mem=0
##SBATCH --partition=debug --exclusive --mem=0 --time=0:30:00

# SDM calculation needs about 32 GB per task.

SPECIES="${1:-Lathyrus}"

# Workaround for memory issues in terra
export R_TERRA_MAX_RAM_MB=$((100 * 1024))

singularity run --bind $PWD cwr_0.4.4.sif "ModGP-run_prep.R" "$SPECIES"
