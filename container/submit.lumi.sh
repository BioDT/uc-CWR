#!/bin/bash -l
#SBATCH -J test
#SBATCH -o test.out
##SBATCH --partition=standard
#SBATCH --partition=largemem
#SBATCH --nodes=1
#SBATCH --ntasks=128
#SBATCH --cpus-per-task=1
#SBATCH --time=00:15:00

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

singularity run --bind $PWD cwr.sif test.R
