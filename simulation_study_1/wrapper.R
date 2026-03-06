dyn.load("/gpfs/share/apps/gcc/11.2.0/lib64/libstdc++.so.6")

library(tidyverse)
library(lmtp)
library(mlr3extralearners)

#
# The wrapper function runs the statistical analysis for each simulated dataset.
# 
wrapper <- function(index, seed, alpha, tau, N, estimator, data, path) {
  # Check cache
  if(!file.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  if(file.exists(path)) {
    return(read_rds(path))
  }

  #learners_trt <- list(list("glm", constrain_positive = TRUE, interactions = 1, lambda = 0))
  learners_trt <- list(list("nn", learning_rate = 1e-3, epochs = 500, hidden = 25, dropout = 0.10, layers = 2, constrain_positive = TRUE))
  learners_outcome <- list(list("mean"), list("glm"), list("ranger"), list("lightgbm"))

  trt <- paste0("A_", 1:tau)
  time_vary <- as.list(paste0("L_", 1:tau))
  outcome <- "Y"
  baseline <- c("W1", "W2")

  conditional <- matrix(TRUE, ncol = tau, nrow = nrow(data))
  conditional[, 1] <- data$A_1 > 0

  res <- lmtp_tmle(
    data, trt, outcome, time_vary, conditional,
    baseline = baseline,
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
    learners_conditional = list("mean", "glm", "ranger", "lightgbm"),
    folds = 5,
    control = lmtp_control(.trim = 1, .learners_outcome_folds = 5, .learners_trt_folds = 1, .learners_conditional_folds = 5, .return_full_fits = FALSE)
  )

  write_rds(tibble(
    index,
    estimator = estimator, 
    seed  = seed, 
    alpha = alpha,
    tau   = tau, 
    N     = N, 
    theta = res$theta,
    low   = res$low,
    high  = res$high,
    standard_error = res$standard_error
  ), path)

  return(res)
}
