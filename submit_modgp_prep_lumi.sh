#!/bin/bash -l
#SBATCH -J ModGP
#SBATCH -o modgp-%j.out
#SBATCH --account=project_465000915
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=8
#SBATCH --time=24:00:00
#SBATCH --partition=small --mem=64G
##SBATCH --partition=standard --exclusive --mem=0
##SBATCH --partition=debug --exclusive --mem=0 --time=0:30:00

SPECIES="${1:-Lathyrus}"

# Workaround for memory issues in terra
# Max memory for terra
R_TERRA_MAX_RAM_MB=$((100 * 1024))
# Limit max memory further if available memory is less than above
if [[ ${SLURM_MEM_PER_NODE:-0} > 0 ]]; then
    R_TERRA_MAX_RAM_MB=$(( $R_TERRA_MAX_RAM_MB < $SLURM_MEM_PER_NODE ? $R_TERRA_MAX_RAM_MB : $SLURM_MEM_PER_NODE ))
fi
export R_TERRA_MAX_RAM_MB
# End of workaround

singularity run --bind $PWD cwr_0.5.1.sif "ModGP-run_prep.R" "$SPECIES"
