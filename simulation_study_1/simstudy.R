#
# Simulation Study
#
dyn.load("/gpfs/share/apps/gcc/11.2.0/lib64/libstdc++.so.6")

library(tidyverse)

root <- rprojroot::is_git_root                                                                         
basepath <- root$find_file("simulation_study_1")  

source(glue::glue("{basepath}/simulate.R"))
source(glue::glue("{basepath}/wrapper.R"))
source(glue::glue("{basepath}/env.R"))

cache_path <- Sys.getenv("SIMULATION_CACHE_PATH")
task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")

if(cache_path == "") stop("Please set SIMULATION_CACHE_PATH environment variable.")
if(task_id == "") stop("Task id not set. Please set SLURM_ARRAY_TASK_ID, or run simulations through a Slurm job array, which will set this environment variable for you.")

simulations <- expand_grid(
  index = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),
  tau = 4,
  N = c(250, 500, 1e3, 1.5e3, 2e3),
  alpha = c(0, 2.5, 5),
  estimator = "tmle"
) |>
  mutate(
    seed = index * 1e4 + tau + N,
    data = pmap(list(seed, N, alpha, tau), simulate_data),
    path = glue::glue("{cache_path}/{N}/{index}_{estimator}_{alpha}_{tau}_{N}.rds"),
    fit = pmap(list(index, seed, alpha, tau, N, estimator, data, path), wrapper)
  )
