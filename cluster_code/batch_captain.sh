#!/bin/bash

#SBATCH --array=1-100
#SBATCH --mem-per-cpu=10G
#SBATCH --partition=long
#SBATCH --job-name="captain-batch"
#SBATCH --mail-user=k.dhanjal-adams@kew.org
#SBATCH --mail-type=END,FAIL

source activate captain_env

let repetition=$SLURM_ARRAY_TASK_ID

python "batch_captain.py" ${repetition} 
