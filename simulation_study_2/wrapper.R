dyn.load("/gpfs/share/apps/gcc/11.2.0/lib64/libstdc++.so.6")

library(tidyverse)
library(lmtp)
library(mlr3extralearners)

#
# The wrapper function runs the statistical analysis for each simulated dataset.
# 
wrapper <- function(index, seed, tau, a, N, scenario, estimator, data, path) {
  # Check cache
  if(!file.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  if(file.exists(path)) {
    return(read_rds(path))
  }

  right_trt <- list(
    list("glm", constrain_positive = TRUE, interactions = 1, lambda = 0)
  )

  wrong_trt <- list(
    list("constant", constant = 1)
  )

  right <- list(list("mean"), list("glm"), list("ranger"))
  wrong <- list(list("mean"))

  if(scenario == 1) {
    learners_trt <- list(right_trt, right_trt, right_trt, right_trt)
    learners_outcome <- list(right, right, right, right)
  }
  else if(scenario == 2) {
    learners_trt <- list(right_trt, right_trt, wrong_trt, wrong_trt)
    learners_outcome <- list(wrong, wrong, right, right)
  }
  else if(scenario == 3) {
    learners_trt <- list(wrong_trt, wrong_trt, wrong_trt, right_trt)
    learners_outcome <- list(right, right, right, wrong)
  }
  else if(scenario == 4) {
    learners_trt <- list(wrong_trt, wrong_trt, wrong_trt, wrong_trt)
    learners_outcome <- list(wrong, wrong, wrong, wrong)
  }

  trt <- map(1:tau, \(t) paste0("A_", t, 0:5))
  time_vary <- as.list(paste0("L_", 1:tau))
  time_vary[[1]] <- c("L_11", "L_12", "L_13")
  outcome <- "Y"

  conditional <- matrix(TRUE, ncol = tau, nrow = nrow(data))
  if(a >= 0) conditional[, tau] <- data[[paste0("A_", tau, a)]] == 1

  if(estimator == "tmle") {
    res <- lmtp_tmle(
      data, trt, outcome, time_vary, conditional,
      baseline = NULL,
      shifted = NULL,
      cens = NULL,
      id = NULL,
      bounds = NULL,
      shift = mtp,
      outcome_type = "binomial",
      riesz = TRUE,
      mtp = TRUE,
      learners_trt = learners_trt,
      learners_outcome = learners_outcome,
      learners_conditional = list("mean", "ranger"),
      folds = 5,
      control = lmtp_control(.trim = 1, .learners_outcome_folds = 5, .learners_trt_folds = 1, .learners_conditional_folds = 5, .return_full_fits = FALSE)
    )
  }
  else if(estimator == "ipw") {
    res <- lmtp_ipw(
      baseline = NULL,
      shifted = NULL,
      k = Inf,
      cens = NULL,
      shift = mtp,
      outcome_type = "binomial",
      learners = learners_trt,
      mtp = TRUE,
      id = NULL,
      folds = 5,
      riesz = TRUE,
      learners_conditional = list("mean", "ranger"),
      control = lmtp_control(.trim = 1, .learners_outcome_folds = 5, .learners_trt_folds = 1, .learners_conditional_folds = 5, .return_full_fits = FALSE)
    )
  }
  else if(estimator == "sub") {
    res <- lmtp_sub(
      data, trt, outcome, time_vary, conditional,
      learners = learners_outcome,
      baseline = NULL,
      cens = NULL,
      shift = mtp,
      outcome_type = "binomial",
      folds = 5,
      control = lmtp_control(.trim = 1, .learners_outcome_folds = 5, .learners_trt_folds = 1)
    )
  }

  write_rds(tibble(
    index,
    estimator = estimator, 
    seed = seed, 
    tau = tau, 
    a = a, 
    N = N, 
    scenario = scenario, 
    fit = list(res)
  ), path)

  return(res)
}
