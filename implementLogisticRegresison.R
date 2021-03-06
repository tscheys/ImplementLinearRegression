#Logistic regression 
# test variables 
X <- matrix(c(5,4,6,7,8,29,1,1,1,1,1,1), nrow=6, ncol=2)
y <- c(0,1,1,1,0,1)
theta <- c(0,0)
alpha = 0.001

x <- matrix(c(5,4,6,7,8,29,1,1,1,1,1,1), nrow=6, ncol=2)
y <- c(0,1,1,1,0,1)

sigmoid <- function(theta, X) {
  return(1/(1+exp(-t(theta) %*% X)))
}

getCost <- function(X,y,theta,alpha) {
  # transpose X so that cols are observations and rows predictors
  X = t(X)
  # calculate cost 
  g = sigmoid(theta,X)
  return(log(g) %*% -y - (1 - g) %*% (1-y))
}

getGradient <- function(X,y,theta,alpha) {
  X = t(X)
  m = ncol(X)
  # calculate gradient 
  g = sigmoid(theta,X)
  grad = theta - (alpha/m) * t((t(t(g) - y)) %*% t(X))
  return(grad)
}

getTheta <- function(X,y,theta,alpha) {
  costs = rep(0, 100)
  for(iter in 1:100) {
    theta = getGradient(X,y,theta,alpha)
    costs[iter] = getCost(X,y,theta,alpha)
  }
  return(costs)
}

plot(1:100, getTheta(X,y,theta,alpha))

#to do list:
#refactor
#alpha tuning
#predictions, stats, plotting
#investigate: starting value of theta is important (for increase/decrease cost func)
