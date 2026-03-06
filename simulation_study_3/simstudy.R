#
# Simulation Study
#
dyn.load("/gpfs/share/apps/gcc/11.2.0/lib64/libstdc++.so.6")

library(tidyverse)

root <- rprojroot::is_git_root                                                                         
basepath <- root$find_file("simulation_study_2")

source(glue::glue("{basepath}/simulate.R"))
source(glue::glue("{basepath}/wrapper.R"))
source(glue::glue("{basepath}/env.R"))

cache_path <- Sys.getenv("SIMULATION_CACHE_PATH")
task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")

if(cache_path == "") stop("Please set SIMULATION_CACHE_PATH environment variable.")
if(task_id == "") stop("Task id not set. Please set SLURM_ARRAY_TASK_ID, or run simulations through a Slurm job array, which will set this environment variable for you.")

N_simulations <- 1

simulations <- expand_grid(
  index = as.numeric(task_id),
  #N = c(2e3, 1e3, 500),
  #tau = seq(2, 14, 2),
  #estimator = c("tmle", "riesz")
  N = 500,
  tau = 2,
  estimator = c("tmle", "riesz")
) |>
  mutate(
    seed = index * 5e3 + tau + N,
    data = pmap(list(seed, N, tau), simulate_data),
    path = glue::glue("{cache_path}/{N}/{index}_{estimator}_{tau}.rds"),
    fit = pmap(list(index, seed, tau, N, estimator, data, path), wrapper)
  )
