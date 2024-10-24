prob_L  <- function(A, L) {
  plogis(-0.3 * L + 0.5 * A) 
}

prob_A <- function(t, tau, prev_A, L) {
  #(t < tau) * plogis(-2 + 1 / (1 + 2 * L + prev_A)) + 
    #(t == tau) * plogis(1 - 3 * prev_A + L)
  plogis(prev_A - 2.5 + 0.5 * L)
}

prob_Y <- function(A, L) {
  #plogis(-2 + 1 / (1 - 1.2 * A - 0.3 * L)) 
  plogis(0.5 * (A - 2) - L)
}

mtp_basic <- function(data, trt) {
  a <- data[[trt]]
  (a - 1) * (a - 1 >= 1) + a * (a - 1 < 1)
  #a * 0 + 1
}

mtp <- function(data, trt) {
  a <- data[, trt] 
  a[, 1] <- ifelse(a[, 1] == 1, 1, 0)
  a[, 2] <- ifelse(a[, 2] == 1 | a[, 3] == 1, 1, 0)
  a[, 3] <- ifelse(a[, 4] == 1, 1, 0)
  a[, 4] <- ifelse(a[, 5] == 1, 1, 0)
  a[, 5] <- ifelse(a[, 6] == 1, 1, 0)
  a[, 6] <- 0
  a
}

simulate_data <- function(seed, N, tau) {
  set.seed(seed)
  
  data <- tibble(id = 1:N)
  
  size = 5
  p = 0.4
  
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
      data[[Lt]] <- simcausal:::rcat.b1(N, probs = c(0.5, 0.25, 0.25))
      data[[Ltd]] <- data[[Lt]]
      
      data[[At]] <- rbinom(N, size = size, prob = prob_L(0, data[[Lt]]))
      data$tmp1 <- rbinom(N, size = size, prob = prob_L(0, data[[Lt]]))
      data[[Atd]] <- mtp_basic(data, "tmp1")
    } 
    else {
      data[[Lt]] <- rbinom(N, size = 1, prob = prob_L(data[[At1]], data[[Lt1]]))
      data[[Ltd]] <- rbinom(N, size = 1, prob = prob_L(data[[At1d]], data[[Lt1d]]))
      
      data[[At]] <- rbinom(N, size = size, prob = prob_A(t, tau, data[[At1]], data[[Lt]]))
      data$tmp <- rbinom(N, size = size, prob = prob_A(t, tau, data[[At1d]], data[[Ltd]]))

      data[[Atd]] <- mtp_basic(data, "tmp")
    }
  }


  data$Y  <- rbinom(N, size = 1, prob_Y(data[[At]], data[[Lt]]))
  data$Yd <- rbinom(N, size = 1, prob_Y(data[[Atd]], data[[Ltd]]))

  for(t in 1:tau) {
    At <- paste0("A_", t)
    Atd <- paste0("A_", t, "d")

    data[[At]] <- factor(data[[At]], levels = 0:size)
    data[[Atd]] <- factor(data[[Atd]], levels = 0:size)
  }

  data$L_1 <- factor(data$L_1, levels = 1:3)
  data$L_1d <- factor(data$L_1d, levels = 1:3)

  data <- as_tibble(model.matrix(~ -1 + ., data, contrasts.arg = lapply(data[, sapply(data, is.factor), drop = FALSE],
                         contrasts, contrasts = FALSE))) %>%
    mutate_all(as.numeric)


  data
}

#mtp <- function(data, trt) {
#  a <- data[[trt]]
#  a * 0
#}
#
#simulate_data <- function(seed, N, tau) {
#  set.seed(seed)
#  
#  data <- tibble(id = 1:N)
#  
#  for(t in 1:tau) {
#    Lt <- paste0("L_", t)
#    Ltd <- paste0("L_", t, "d")
#    
#    At <- paste0("A_", t)
#    Atd <- paste0("A_", t, "d")
#    
#    Lt1 <- paste0("L_", t - 1)
#    Lt1d <- paste0("L_", t - 1, "d")
#    
#    At1 <- paste0("A_", t - 1)
#    At1d <- paste0("A_", t - 1, "d")
#    
#    if(t == 1) {
#      data[[Lt]] <- runif(N)
#      data[[Ltd]] <- data[[Lt]]
#      
#      data[[At]] <- rbinom(N, size = 1, prob = 0.25)
#      data$tmp <- rbinom(N, size = 1, prob = 0.25)
#      data[[Atd]] <- mtp(data, "tmp")
#    } 
#    else {
#      data[[Lt]] <- runif(N)
#      data[[Ltd]] <- runif(N)
#      
#      data[[At]] <- rbinom(N, size = 1, prob = 0.25)
#      data$tmp <- rbinom(N, size = 1, prob = 0.25)
#      data[[Atd]] <- mtp(data, "tmp")
#    }
#  }
#  
#  data$Y  <- rbinom(N, size = 1, plogis(data[[Atd]]))
#  data$Yd <- rbinom(N, size = 1, plogis(data[[Atd]]))
#  data
#}
