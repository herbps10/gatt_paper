dyn.load("/gpfs/share/apps/gcc/11.2.0/lib64/libstdc++.so.6")
library(tidyverse)
library(lmtp)
library(mlr3extralearners)

source("simulate_conditional.R")

BASE_PATH <- Sys.getenv("BASE_PATH")

wrapper <- function(f, estimator) {
  function(index, seed, tau, a, N, scenario, ...) {
    print(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))
    print(seed)
    params <- list(...)
    params$riesz <- TRUE

    right_trt <- list(
      list("glm", constrain_positive = TRUE, interactions = 1, lambda = 0)
    )
    wrong_trt <- list(
      list("constant", constant = 1)
    )

    right <- list(list("mean"), list("glm"), list("ranger"))
    wrong <- list(list("mean"))

    if(scenario == 1) {
      params$learners_trt <- right_trt
      params$learners_outcome <- right
    }
    else if(scenario == 2) {
      params$learners_trt <- list(right_trt, right_trt, wrong_trt, wrong_trt)
      params$learners_outcome <- list(wrong, wrong, right, right)
    }
    else if(scenario == 3) {
      params$learners_trt <- list(wrong_trt, wrong_trt, wrong_trt, right_trt)
      params$learners_outcome <- list(right, right, right, wrong)
    }
    else if(scenario == 4) {
      params$learners_trt <- list(wrong_trt, wrong_trt, wrong_trt, wrong_trt)
      params$learners_outcome <- list(wrong, wrong, wrong, wrong)
    }

    if(estimator == "ipw") {
      params$learners <- params$learners_trt
      params$learners_trt <- NULL
      params$learners_outcome <- NULL
    }
    else if(estimator == "sub") {
      params$learners <- params$learners_outcome
      params$learners_trt <- NULL
      params$learners_outcome <- NULL
      params$riesz <- NULL
    }

    cache <- glue::glue("{BASE_PATH}/conditional_cache/{N}/{estimator}_{tau}_{a}_{seed}_{scenario}.rds")
    print(cache)
    if(file.exists(cache)) {
      return(NULL)
    }
    else {
      start <- Sys.time()
      res <- do.call(f, params)
      write_rds(tibble(estimator = estimator, seed = seed, tau = tau, a = a, N = N, scenario = scenario, fit = list(res)), cache)
      return(res)
    }
  }
}

simulations <- expand_grid(
  index = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),
  tau = 4,
  scenario = 1:4,
  a = c(0:5),
  N = c(500, 1e3, 2.5e3, 5e3)
) %>%
  mutate(
    seed = index * 1e4 + tau + N,
    data = pmap(list(seed, N, tau), simulate_data),
    trt = map(tau, \(tau) map(1:tau, \(t) paste0("A_", t, 0:5))),
    time_vary = map(tau, \(tau) {
      x <- as.list(paste0("L_", 1:tau))
      x[[1]] <- c("L_11", "L_12", "L_13")
      x
    }),
    outcome = "Y"
  )
  
  
simulations <- simulations %>% mutate(
  conditional = pmap(list(data, a, tau), \(data, a, tau) {
    x <- matrix(TRUE, ncol = tau, nrow = nrow(data))
    if(a >= 0) x[, tau] <- data[[paste0("A_", tau, a)]] == 1
    x
  }),
  fit = pmap(list(index, seed, tau, a, N, scenario, data, trt, outcome, time_vary, conditional), wrapper(lmtp_tmle, "tmle"), 
                  baseline = NULL,
		              shifted = NULL,
                  cens = NULL,
		              id = NULL,
		              bounds = NULL,
                  shift = mtp,
                  outcome_type = "binomial",
                  mtp = TRUE,
                  learners_conditional = list("mean", "ranger"),
                  folds = 5,
                  control = lmtp_control(.trim = 1, .learners_outcome_folds = 5, .learners_trt_folds = 1, .learners_conditional_folds = 5, .return_full_fits = FALSE)),
  fit_sub = pmap(list(index, seed, tau, a, N, scenario, data, trt, outcome, time_vary, conditional), wrapper(lmtp_sub, "sub"), 
                  baseline = NULL,
                  cens = NULL,
                  shift = mtp,
                  outcome_type = "binomial",
                  folds = 5,
                  control = lmtp_control(.trim = 1, .learners_outcome_folds = 5, .learners_trt_folds = 1)),
  fit_ipw = pmap(list(index, seed, tau, a, N, scenario, data, trt, outcome, time_vary, conditional), wrapper(lmtp_ipw, "ipw"), 
                  baseline = NULL,
                  shifted = NULL,
        	        k = Inf,
                  cens = NULL,
                  shift = mtp,
                  outcome_type = "binomial",
                  mtp = TRUE,
  	              id = NULL,
                  folds = 5,
                  learners_conditional = list("mean", "ranger"),
                  control = lmtp_control(.trim = 1, .learners_outcome_folds = 5, .learners_trt_folds = 1, .learners_conditional_folds = 5, .return_full_fits = FALSE))
  )
