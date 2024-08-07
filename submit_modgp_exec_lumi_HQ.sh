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

# Add hq to path
export PATH=$PATH:$(pwd)

# Start the server in the background
hq server start &

# Wait until the server has started
until hq job list &> /dev/null ; do sleep 1 ; done

# Start the workers in the background.
srun --overlap --cpu-bind=none --mpi=none hq worker start \
    --manager slurm \
    --on-server-lost finish-running \
    --cpus="$SLURM_CPUS_PER_TASK" &


# Wait until all workers have started
hq worker wait "$SLURM_NTASKS"

# Submit different subspecies of the given species in parallel to the exec function in parallel
hq submit --each-line species_list.txt bash -c 'singularity run --bind $PWD cwr_0.3.0.sif "ModGP-run_exec.R" "$HQ_ENTRY"'

# Wait for all tasks to finish
hq job wait all

# Shut down the workers and server
hq worker stop all
hq server stop