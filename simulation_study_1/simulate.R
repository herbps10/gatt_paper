mtp <- function(data, trt) {
  a <- data[[trt]]
  ifelse(a < 0.1 | a + 0.1 > 1, a, a + 0.1)
}

simulate_data <- function(seed, N, alpha, tau, intervene = FALSE) {
  set.seed(seed)
  
  data <- tibble(id = 1:N)

  data$W1 <- rbinom(N, 1, 0.5)
  data$W2 <- rbinom(N, 1, 0.5)
  
  for(t in 1:tau) {
    Lt <- paste0("L_", t)
    
    At <- paste0("A_", t)
    
    Lt1 <- paste0("L_", t - 1)
    
    At1 <- paste0("A_", t - 1)

    if(t == 1) {
      data[[Lt]]  <- rbinom(N, size = 5, prob = 0.2 + 0.1 * data$W1 - 0.1 * data$W2)
    } 
    else {
      data[[Lt]]  <- data[[Lt1]] + rbinom(N, 1, 1 - data[[At1]]) 
    }

    data[[At]] <- truncnorm::rtruncnorm(N, data[[Lt]]  / (4 + t), 0.2, a = 0, b = 1)
    data[[At]] <- ifelse(data[[At]] < 0.1, 0, data[[At]])

    if(intervene == TRUE) data[, At] <- mtp(data, At)
  }

  data$Y  <- rbinom(N, size = 1, plogis(1 * data[[Lt]] - alpha * data[[At]] + 0.5 * data$W1 - 0.5 * data$W2))

  data
}


