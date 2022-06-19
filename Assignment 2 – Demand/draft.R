
sigma <- IV$se[c(2,5)]

sigmanu <- cbind(
  nu[,1] * sigma[1],
  nu[,2] * sigma[2]
)

mu <- as.matrix(ungroup(cars) %>% select(princ, li)) %*% t(sigmanu)

# exponent of delta + nu from each product for each consumer
cars$delta <- log(cars$sj) - log(cars$s0)
exp_ji <- exp(cars$delta + mu)

# summing the denominator components by market, and adding 1, for each consumer
market_denoms <- cbind(
  cars %>% select(ye, ma),
  as_tibble(exp_ji)
) %>%
  group_by(ye, ma) %>% 
  mutate(
    across(.cols = V1:V1000, ~ sum(.x) + 1)
  )

# dividing numerator by denominator and averaging to obtain the choice probabilities
pi_delta <- rowMeans(exp_ji / market_denoms[3:1002])


##############################################################################
# Contraction mapping
dist <- 10  # temporary value to start loop

while(dist > 1e-6){
  # Storing the previous delta value
  olddelta <- cars$delta
  
  # Step 2 in the contraction mapping
  cars$delta <- cars$delta + log(cars$sj) - log(pi_delta)
  
  # Check if we have reached the tolerance level
  dist <- norm(as.matrix(cars$delta - olddelta))
  
  # Creating the new pi(delta, sigma)
  exp_ji <- exp(cars$delta + mu)
  market_denoms <- cbind(
    cars %>% select(ye, ma),
    as_tibble(exp_ji)
  ) %>%
    group_by(ye, ma) %>% 
    mutate(
      across(.cols = V1:V1000, ~ sum(.x) + 1)
    )
  pi_delta <- rowMeans(exp_ji / market_denoms[3:1002])
}

delta <- cars$delta