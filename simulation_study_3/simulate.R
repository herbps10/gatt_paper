mtp <- function(data, trt) {
  a <- data[[trt]]
  a * 0 + 1
}

simulate_data <- function(seed, N, tau, sigma = 0.5) {
  set.seed(seed)
  
  data <- tibble(id = 1:N)
  
  for(t in 1:tau) {
    Lt <- paste0("L_", t)
    Ltd <- paste0("L_", t, "d")
    
    At <- paste0("A_", t)
    Atd <- paste0("A_", t, "d")
    
    Lt1 <- paste0("L_", t - 1)
    Lt1d <- paste0("L_", t - 1, "d")
    
    At1 <- paste0("A_", t - 1)
    At1d <- paste0("A_", t - 1, "d")
    
    if(t == 1) {
      data[[Lt]] <- runif(N, 0, 1)
      data[[Ltd]] <- data[[Lt]]
      
      data[[At]] <- rbinom(N, size = 1, prob = 0.5)
    } 
    else {
      data[[Lt]] <- rnorm(N, mean = 0.25 * data[[Lt1]], 0.5)
      data[[Ltd]] <- rnorm(N, mean = 0.25 * data[[Lt1d]], 0.5)
      
      data[[At]] <- rbinom(N, size = 1, prob = plogis(0.5 - 0.2 * data[[At1]] + 0.1 * data[[Lt1]]))
    }
    
    data[[Atd]] <- mtp(data, At)
  }
  data$Y  <- rnorm(N, data[[At]] + data[[Lt]], sigma)
  data$Yd <- rnorm(N, data[[Atd]] + data[[Ltd]], sigma)
  data
}

