#!/bin/bash -l
#SBATCH -J modgp
#SBATCH -o modgp-%j.out
#SBATCH --nodes=1
#SBATCH --tasks-per-node=8
#SBATCH --cpus-per-task=16
#SBATCH --time=04:00:00
##SBATCH --partition=largemem
##SBATCH --partition=lumid
##SBATCH --partition=small --mem-per-cpu=2G
#SBATCH --partition=standard --exclusive --mem=0
##SBATCH --partition=debug --exclusive --mem=0 --time=0:30:00

# SDM calculation needs about 32 GB per task.

SPECIES="${1:-Lathyrus}"

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export CWR_ON_LUMI=1

singularity run --bind $PWD cwr_0.3.0.sif "ModGP MASTER.R" "$SPECIES"

