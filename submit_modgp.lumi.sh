#!/bin/bash -l
#SBATCH -J modgp
#SBATCH -o modgp-%j.out
##SBATCH --partition=largemem
#SBATCH --partition=lumid
#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=4G
#SBATCH --time=04:00:00

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

singularity run --bind $PWD cwr_0.2.0.sif 'ModGP MASTER.R'

