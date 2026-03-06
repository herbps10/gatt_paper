library(tidyverse)

root <- rprojroot::is_git_root                                                                         
basepath <- root$find_file("simulation_study_1")  

source(glue::glue("{basepath}/env.R"))
source(glue::glue("{basepath}/simulate.R"))


results_path <- Sys.getenv("SIMULATION_RESULTS_PATH")
if(results_path == "") stop("Please set SIMULATION_RESULTS_PATH environment variable.")

simulations <- read_rds(glue::glue("{results_path}/simulation_results.rds"))


theta0 <- expand_grid(
  #alpha = unique(simulations$alpha),
  alpha = c(0, 2.5, 5),
  tau = unique(simulations$tau)
) |> 
  mutate(theta0 = map2_dbl(alpha, tau, \(alpha, tau) {
    dat <- simulate_data(10016, 1e6, alpha, tau, intervene = TRUE)
    mean(dat$Y[dat$A_1 > 0])
  }))

simulation_results <- simulations |>
  left_join(theta0) |>
  mutate(bias = theta0 - theta, covered = low < theta0 & high > theta0)

simulation_table <- simulation_results |>
  group_by(alpha, tau, N) |>
  summarize(n = n(), mae = mean(abs(bias)), me = mean(bias), mse = mean(bias^2), coverage = mean(covered))

write_rds(simulation_table, glue::glue("{basepath}/results/longitudinal_simulation_table.rds"))
