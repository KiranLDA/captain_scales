#!/bin/bash

#SBATCH --array=1-100
#SBATCH --mem-per-cpu=8G
#SBATCH --partition=long
#SBATCH --job-name="captain-batch-90"
#SBATCH --mail-user=k.dhanjal-adams@kew.org
#SBATCH --mail-type=END,FAIL

source activate captain_env5

let repetition=$SLURM_ARRAY_TASK_ID

python "batch_captain.py" ${repetition} 
