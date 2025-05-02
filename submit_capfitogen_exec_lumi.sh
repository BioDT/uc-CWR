#!/bin/bash -l
#SBATCH -J capfitogen
#SBATCH -o capfitogen-%j.out
#SBATCH --account=project_465000915
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=128
#SBATCH --time=10:00:00
#SBATCH --partition=largemem
##SBATCH --partition=lumid
##SBATCH --partition=small --mem-per-cpu=2G
##SBATCH --partition=standard --exclusive --mem=0
##SBATCH --partition=debug --exclusive --mem=0 --time=0:30:00

singularity run --bind $PWD cwr_0.6.0.sif "capfitogen_master.R"