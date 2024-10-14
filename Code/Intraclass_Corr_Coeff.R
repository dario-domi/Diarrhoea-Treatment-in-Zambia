################################################################################
#
# FUNCTIONS TO COMPUTE INTRACLASS CORRELATION COEFFICIENT VIA DIFFERENT APPROACHES
#
################################################################################
#
# Approaches described in:
# https://online.ucpress.edu/collabra/article/10/1/94263/200117/# Approaches-
# for-Quantifying-the-ICC-in-Multilevel
#
# The functions in this script are called in the main "Analyses.R" script
#
################################################################################

# All functions below take as input a multilevel logistic model ('mod'), as 
# returned by glmer (lme4 package).
# The simulation approach also needs an integer N, size of the Gaussian random sample


### Latent threshold approach
threshold_ICC <- function(mod){
  tau <- VarCorr(mod)$facility[1]
  tau/(tau + pi^2/3)
}


### Simulation Approach
simulation_ICC <- function(mod, N){
  
  # RE Variance and Intercept
  tau <- VarCorr(mod)$facility[1] 
  gamma00 <- fixef(mod)[1]
  
  # Sample N random effects from normal distribution
  U0j <- rnorm(N, 0, sqrt(tau))
  # Compute associated probabilities
  p_hat <- logodds2prob(gamma00 + U0j)
  # Compute associated Bernoulli variances
  var_L1 <- p_hat * (1-p_hat)
  
  # Compute and return ICC
  var(p_hat) / (var(p_hat) + mean(var_L1))
}


### Linearisation Approach
linearisation_ICC <- function(mod){
  
  # Model Intercept
  gamma00 <- as.numeric(fixef(mod))
  # Associated probability
  p_hat <- logodds2prob(gamma00)
  # Level-1, Bernoulli variance (intraclass)
  V1 <- p_hat * (1-p_hat)
  # Level 2
  tau <- VarCorr(mod)$facility[1]
  V2 <- tau * p_hat^2 / (1+exp(gamma00))^2
  
  # Compute and return ICC
  V2/(V1+V2)
}

