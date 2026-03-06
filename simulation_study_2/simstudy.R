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
  #scenario = 1:4,
  #a = c(0:5),
  #N = c(500, 1e3, 2.5e3, 5e3),
  scenario = 1,
  a = 0,
  N = 500,
  #estimator = c("tmle", "ipw", "sub")
  estimator = "tmle"
) |>
  mutate(
    seed = index * 1e4 + tau + N,
    data = pmap(list(seed, N, tau), simulate_data),
    path = glue::glue("{cache_path}/{N}/{index}_{estimator}_{tau}_{a}_{N}_{scenario}.rds"),
    fit = pmap(list(index, seed, tau, a, N, scenario, estimator, data, path), wrapper)
  )
