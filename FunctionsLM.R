# Generate n-dimensional response Y that follows linear regression model Y = Xbeta + epsilon, where epsilon is normal zero with variance sigma^2 independent across samples. Seed should be set at the beginning of the function
# X - design matrix
# beta - given parameter vector
# sigma - standard deviation of the noise
# seed  - starting seed value
generateY <- function(X, beta, sigma, seed = 5832652){
  #[ToDo] Set seed and generate Y following linear model
  set.seed(seed)
  #  epsilon for each dimension is N(0, sigma^2) , independent 
  epsilon <- rnorm(n, mean=0, sd=sigma)
  # linear regression model Y = Xbeta + epsilon
  Y <- X %*% beta + epsilon
  # Return Y
  return(Y)
}

# Calculate beta_LS - least-squares solution, do not use lm function
# X - design matrix
# Y -response
calculateBeta <- function(X, Y){
  # Calculate beta_LS
  beta_LS <- solve(crossprod(X),crossprod(X,Y))
  # pick b that minimizes the squared distance: ||Y - Xb||^2
  # take differentiation, we have the normal equation: X^T * X * b = X^T * Y
  # that is, b = (X^T * X)^-1 * X^T * Y 
  # Return beta
  return(beta_LS)
}

# Calculate MSE
calculateMSE <- function(beta, beta_LS){
  MSE <- norm(beta - beta_LS, "2")^2
  # Return MSE - error ||beta - beta_LS||_2^2
  return(MSE)
}