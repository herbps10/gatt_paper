dyn.load("/gpfs/share/apps/gcc/11.2.0/lib64/libstdc++.so.6")

library(tidyverse)
library(progressr)
library(future)
library(furrr)
library(lmtp)

plan(sequential)

source("simulate_longitudinal.R")

BASE_PATH <- Sys.getenv("BASE_PATH")

print(Sys.getenv("SLURM_ARRAY_TASK_ID"))

wrapper <- function(f, p) {
  function(index, seed, tau, N, ...) {
    params <- list(...)
    cache <- glue::glue("{BASE_PATH}/cache/{N}/{params$riesz}_{tau}_{seed}.rds")
    if(file.exists(cache)) {
      return(read_rds(cache))
    }
    else {
      file <- glue::glue("{BASE_PATH}/logs/log-{Sys.getpid()}.txt")
      start <- Sys.time()
      options(mc.cores = 1)
      write(glue::glue("{Sys.time()}: starting index={index} seed={seed} tau={tau} N={N} riesz={params$riesz} cores={parallel::detectCores()}"), file=file, append = TRUE)
      res <- f(...)
      write(glue::glue("{Sys.time()}: ending index={index} seed={seed} tau={tau} N={N} duration={hms::as.hms(Sys.time()-start)} riesz={params$riesz}"), file=file, append = TRUE)
      p()
      write_rds(tibble(seed = seed, tau = tau, N = N, riesz = params$riesz, fit = list(res)), cache)
      return(res)
    }
  }
}

learners <- c("mean", "glm")

fopts <- furrr:::furrr_options(seed = TRUE)
with_progress({
  simulations <- expand_grid(
    index = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),
    N = c(2e3, 1e3, 500),
    tau = seq(2, 14, 2)
  ) %>%
    mutate(
      seed = index * 5e3 + tau + N,
      data = pmap(list(seed, N, tau), simulate_data),
      trt = map(tau, \(tau) paste0("A_", 1:tau)),
      time_vary = map(tau, \(tau) as.list(paste0("L_", 1:tau))),
      outcome = "Y"
    )

  for(N in unique(simulations$N)) {
    dir <- glue::glue("{BASE_PATH}/cache/{N}")
    if(!dir.exists(dir)) {
    	dir.create(dir)
    }
  }
  
  p <- progressor(steps = nrow(simulations) * 2)
  
  simulations <- simulations %>% mutate(
    fit_tmle = future_pmap(list(index, seed, tau, N, data, trt, outcome, time_vary), wrapper(lmtp_tmle, p), 
                           baseline = NULL,
                           shift = mtp,
                           learners_outcome = learners,
                           learners_trt = learners,
                           outcome_type = "continuous",
			                     riesz = FALSE,
                           mtp = TRUE,
			                     folds = 5,
                           control = lmtp_control(.trim = 1, .learners_outcome_folds = 5, .learners_trt_folds = 5),
                           .options = fopts),
    fit_riesz = future_pmap(list(index, seed, tau, N, data, trt, outcome, time_vary), wrapper(lmtp_tmle, p), 
                            baseline = NULL,
                            shift = mtp,
                            learners_outcome = learners,
                            learners_trt = list(list("nn", learning_rate = 1e-3, epochs = 500, hidden = 25, dropout = 0.20, layers = 1)),
                            #learners_trt = list(list("glm", interactions = 1, constrain_positive = TRUE, lambda = 0)),
                            outcome_type = "continuous",
                            mtp = TRUE,
        		                folds = 5,
                            riesz = TRUE,
                            control = lmtp_control(.trim = 1, .learners_outcome_folds = 5, .learners_trt_folds = 1),
                            .options = fopts)
  )
})

