if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  fixest,       # used for its clustering options
  matlib,       # for matrix operations
  fastDummies,  # for turning categorical variables into dummies
  rootSolve     # for gradient calculation
)

################################################################################
################################################################################

cars <- read.csv("C:/Users/user/Dropbox/תשפא/IO and Econometrics Workshop/IO-and-Econometrics-Workshop/Assignment 2 – Demand/cars.csv") %>%
  mutate(sj = qu / pop,) %>%    # market share of j
  group_by(ye, ma) %>%
  mutate(s0 = 1 - sum(sj),      # outside option share
         competition = n()) %>% # number of models in the market
  group_by(ye, co) %>%
  mutate(
    tempvar = n(),
    othermarket_princ = (sum(princ) - princ) / (tempvar - 1)   # average price of model in other markets, to be used as an instrument later
  ) %>%
  select(-tempvar) %>%
  ungroup() %>%
  filter(!is.na(sp), !is.na(othermarket_princ)) %>%        # removing observations with missing values
  dummy_cols(select_columns = c("cla", "brd")) # creating separate variables for each class and brand for convenience later on

################################################################################
################################################################################

set.seed(100)
nu <- cbind(
  rnorm(1000, 0, 1)
)

################################################################################
################################################################################

contraction_mapping <- function(sigma){
  
  
  # Multiplying the random coefficients by their sigmas
  sigmanu <- cbind(
    nu[,1] * sigma[1]
  )
  
  # Multiplying this by the product features to create mu for each "consumer"
  mu <- as.matrix(ungroup(cars) %>% select(princ)) %*% t(sigmanu)
  
  ##############################################################################
  
  # Summing mu by market (to be placed in the denominator)
  mu_groups <- cbind(
    cars %>% select(ye, ma),
    as_tibble(mu)
  ) %>%
    group_by(ye, ma)
  colnames(mu_groups) <- c("ye", "ma", rep(c("a"), 1000))
  
  # First "consumer"
  mu_all <- (mu_groups %>% select(1:2,3) %>%
               mutate(a = sum(a)))[,3]
  # Next 999 consumers
  for (i in 4:1002){
    mu_all <- cbind(
      mu_all,
      (mu_groups %>% select(1:2,i) %>%
         mutate(a = sum(a)))[,3]
    )
  }
  
  ##############################################################################
  
  # Now we add the deltas
  cars <- ungroup(cars) %>%
    mutate(delta = log(cars$sj) - log(cars$s0)) %>% # initial guess = logit model result
    group_by(ye, ma) %>%
    mutate(delta_all = sum(delta))  # sum of deltas in the market
  
  
  # Finally, we calculate the choice probabilities:
  numerator <- rowSums(exp(cars$delta + mu))
  denominator <- 1 + rowSums(exp(cars$delta_all + as.matrix(mu_all)))
  pi_delta <- numerator / (denominator * 1000)
  
  
  
  ##############################################################################
  # Contraction mapping
  dist <- 10  # temporary value to start loop
  
  while(dist > 1e-6){
    # Storing the previous delta value
    olddelta <- cars$delta
    
    # Step 2 in the contraction mapping
    cars <- ungroup(cars) %>%
      mutate(delta = cars$delta + log(cars$sj) - log(pi_delta)) %>%
      group_by(ye, ma) %>%
      mutate(delta_all = sum(delta))  # sum of deltas in the market, for new pi
    
    # Check if we have reached the tolerance level
    dist <- norm(as.matrix(cars$delta - olddelta))
    
    # Creating the new pi(delta, sigma)
    numerator <- rowSums(exp(cars$delta + mu))
    denominator <- 1 + rowSums(exp(cars$delta_all + as.matrix(mu_all)))
    pi_delta <- numerator / (denominator * 1000)
  }
  
  delta <- cars$delta
}

################################################################################
################################################################################

X <- as.matrix(cars %>% select(princ, hp, we, li, sp, cla_2, cla_3, cla_4, cla_5, home))  # including price

Z <- as.matrix(cars %>% select(hp, we, li, sp, cla_2, cla_3, cla_4, cla_5, home, competition, xexr, tax, othermarket_princ))

# Running IV, just for the starting guess of coefficients
IV <- feols(log(sj) - log(s0) ~ hp + we + li + sp + factor(cla) + home | princ ~ competition + xexr + tax + othermarket_princ, cars, cluster = c("ye", "brd"))


################################################################################
################################################################################

gmm_initial <- function(theta){
  # Creating delta(sigma)
  delta <- contraction_mapping(c(theta[1:ncol(nu)]))
  
  # Creating xi = delta - x*beta -p*alpha
  xi <- delta - X %*% c(theta[(ncol(nu) + 1):(ncol(X) + ncol(nu))])
  
  # Creating the moment equations
  moments <- sapply(as_tibble(Z) * xi, mean)  # multiplying each column of Z by xi, and then taking the means of these columns for the sample analogue
  
  # GMM minimization function, using W = I for first evaluation
  return(t(moments) %*% moments)
}

################################################################################
################################################################################

# Finding initial theta_1 values

# Creating constraint matrices for the values of the random coefficients
ui <- rbind(
  c(1, rep(0, ncol(X))),
  c(-1, rep(0, ncol(X)))
)
ci <- c(0, -IV$se[2] * 2)

theta_1 <- constrOptim(
  theta = c(IV$se[2], IV$coefficients[2:length(IV$coefficients)]),  # initial guess is IV parameters
  f = gmm_initial,
  ui = ui,
  ci = ci,
  method = "Nelder-Mead",
  control = list(maxit = 30)
)

# Creating delta(sigma) and xi of theta_1 for the next step
delta_1 <- contraction_mapping(c(theta_1$par[1:ncol(nu)]))
xi_1 <- delta_1 - X %*% c(theta_1$par[(ncol(nu) + 1):(ncol(X) + ncol(nu))])

# Creating V of theta_1 and then W as its inverse
V <- matrix(NA, nrow = ncol(Z), ncol = ncol(Z))
for(i in 1:ncol(Z)){
  for(j in 1:ncol(Z)){
    V[i, j] = mean((Z[,i] * xi_1) * (Z[,j] * xi_1))
  }
}
W <- inv(V)

################################################################################
################################################################################

gmm_updated <- function(theta){
  
  # Creating the moment equations
  moments <- sapply(as_tibble(Z) * (delta_1 - X %*% c(theta[(ncol(nu) + 1):(ncol(X) + ncol(nu))])), mean)  # multiplying each column of Z by xi, and then taking the means of these columns for the sample analogue
  
  # GMM minimization function
  return(t(moments) %*% W %*% moments)
  
}

################################################################################
################################################################################


theta <- constrOptim(
  theta = theta_1$par,  # initial guess is previous round's estimate
  f = gmm_updated,
  ui = ui,
  ci = ci,
  method = "Nelder-Mead"
)

################################################################################
################################################################################

# Replicating delta(sigma) (needed for creating V below)
delta <- contraction_mapping(c(theta$par[1:ncol(nu)]))

# Creating a function for the moment equations so that their gradients can be evaluated at the estimated theta
moments <- function(par){
  sapply(as_tibble(Z) * (contraction_mapping(c(par[1:ncol(nu)])) - X %*% c(par[(ncol(nu) + 1):(ncol(X) + ncol(nu))])), mean)
}

# Creating D
D <- gradient(moments, theta$par)

# Creating V for the estimated parameters
V <- matrix(NA, nrow = ncol(Z), ncol = ncol(Z))
for(i in 1:ncol(Z)){
  for(j in 1:ncol(Z)){
    V[i, j] = mean((Z[,i] * (delta - X %*% theta$par[(ncol(nu) + 1):(ncol(X) + ncol(nu))])) * (Z[,j] * (delta - X %*% theta$par[(ncol(nu) + 1):(ncol(X) + ncol(nu))])))
  }
}


# Estimating the standard errors
SE <- sqrt(diag(inv(t(D) %*% inv(V) %*% D)) / nrow(X))



# Results
tibble(coef = theta$par, SE = SE, t = coef/SE)
