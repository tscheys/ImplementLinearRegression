# Implementation of Simple Linear Regression 

getBetas = function(x, y) {
    #B1 = somm((xi - avg x)(yi-avg y)) / somm(xi - avg x)^2
    B1 = getNumerator(x,y) / getDenumerator(x)
    #B0 = avg(y) - B1 * avg(x)
    B0 = mean(y) - B1 * mean(x)
    return(c(B0,B1))
}

getFittedValues <- function(coefficients, x) {
  fittedValues = rep(0, length(predictor))
  for(i in 1:length(x)) {
    #fitted.values = b0 + b1*x
    fittedValues[i] = coefficients[1] + coefficients[2] * x[i]
  }
  return(fittedValues)
}

getErrorStatistics = function(x,y) {
  #residuals = y - yhat
  residuals <- y - getFittedValues(getBetas(x,y), x)
  #RSS = (yi - y)^2 
  RSS = sum((residuals)^2)
  #RSE = sqrt(RSS/n-2)
  RSE = sqrt(RSS/(length(x) - 2))
  
  return(residuals, RSS, RSE)
}


getNumerator <- function(x,y) {
  numerator = 0
  for (i in 1:length(x)){
    #calculating B1
    #numerator = somm((xi - avg x)(yi-avg y)) 
    numerator = numerator + (x[i] - X) * (y[i] - Y)
  }
  return(numerator)
}
getDenumerator <- function(x) {
  denumerator = 0
  for (i in 1:length(x)){
    #denumerator = somm(xi - avg x)^2
    denumerator = denumerator + (x[i] - X)^2
  }
  return(denumerator)
}

#(SE B0) ^2 = o^2 (1/n +  [    (avg(x)^2)/somm(   (xi-avg(x))^2  ) ]    )
getStandardErrors <- function(x,y) {
  X = mean(x)
  Y = mean(y)
  denumerator = 0
  
  for(i in 1:length(x)) {
    denumerator = denumerator + (x[i] - X)^2
  }
  
  #estimated squared standard errors for B0 and B1
  SEB0 = sqrt(RSE^2 * (1/length(x) + X^2/denumerator))
  SEB1 = sqrt(RSE^2 / denumerator)
  return(c(SEB0, SEB1))
}

getConfidenceIntervals <- function(beta, se) {
  return(c(betas[1] - 2*se[1], betas[1] + 2*se[1] , betas[2] - 2*se[2], betas[2] + 2*se[2]))
}

#ook formule voor betrouwbaarheidsintervallen maken (95%)

#BI: B +- 2SE(B)

#formule om p waarde geassocieerd met Ho: B=0 te bepalen 

# TESTS 

# x and y variables to test code 
set.seed(100)
x <- rnorm(100)
y <- x + rnorm(100, mean=5, sd=2)

getConfidenceIntervals(getBetas(x,y), getStandardErrors(x,y))