#!/bin/bash

#SBATCH --array=3-100
#SBATCH --mem-per-cpu=2G
#SBATCH --partition=long
#SBATCH --job-name="connectivity_test_2"
#SBATCH --mail-user=k.dhanjal-adams@kew.org
#SBATCH --mail-type=END,FAIL

source activate migflow_env

let repetition=$SLURM_ARRAY_TASK_ID

Rscript "connectivity_batch.R" ${repetition}