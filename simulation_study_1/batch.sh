#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

module load r/4.3.2

source env.sh

Rscript $SIMULATION_STUDY_SCRIPT_PATH
