dyn.load("/gpfs/share/apps/gcc/11.2.0/lib64/libstdc++.so.6")

library(tidyverse)
library(lmtp)
library(mlr3extralearners)

#
# The wrapper function runs the statistical analysis for each simulated dataset.
# 
wrapper <- function(index, seed, tau, N, estimator, data, path) {
  # Check cache
  if(!file.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  if(file.exists(path)) {
    return(read_rds(path))
  }

  learners <- c("mean", "glm")

  trt <- paste0("A_", 1:tau)
  time_vary <- as.list(paste0("L_", 1:tau))
  outcome <- "Y"

  if(estimator == "tmle") {
    res <- lmtp_tmle(
      data,
      trt,
      outcome,
      time_vary,
      baseline = NULL,
      shift = mtp,
      learners_outcome = learners,
      learners_trt = learners,
      outcome_type = "continuous",
      riesz = FALSE,
      mtp = TRUE,
      folds = 5,
      control = lmtp_control(.trim = 1, .learners_outcome_folds = 5, .learners_trt_folds = 5)
    )
  }
  else if(estimator == "riesz") {
    res <- lmtp_tmle(
      data,
      trt,
      outcome,
      time_vary,
      baseline = NULL,
      shift = mtp,
      learners_outcome = learners,
      learners_trt = list(list("nn", learning_rate = 1e-3, epochs = 500, hidden = 25, dropout = 0.20, layers = 1)),
      outcome_type = "continuous",
      mtp = TRUE,
      riesz = TRUE,
      folds = 5,
      control = lmtp_control(.trim = 1, .learners_outcome_folds = 5, .learners_trt_folds = 5)
    )
  }

  write_rds(tibble(
    index = index,
    seed = seed, 
    tau = tau, 
    N = N, 
    estimator = estimator, 
    fit = list(res)
  ), path)

  return(res)
}
