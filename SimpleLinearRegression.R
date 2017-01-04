# Implementation of Simple Linear Regression 

getBetas = function(x, y) {
    #B1 = somm((xi - avg x)(yi-avg y)) / somm(xi - avg x)^2
    B1 = getNumerator(x,y) / getDenumerator(x)
    #B0 = avg(y) - B1 * avg(x)
    B0 = mean(y) - B1 * mean(x)
    return(c(B0,B1))
}

getFittedValues <- function(coefficients, x) {
  return(sapply(x, function(x) coefficients[1] + coefficients[2] * x))
}

getErrorStatistics = function(x,y) {
  #residuals = y - yhat
  residuals <- y - getFittedValues(getBetas(x,y), x)
  #RSS = (yi - y)^2 
  RSS = sum((residuals)^2)
  #RSE = sqrt(RSS/n-2)
  RSE = sqrt(RSS/(length(x) - 2))
  
  return(list(residuals=residuals, RSS=RSS, RSE=RSE))
}

getNumerator = function(x,y) {
  return(sum(mapply(function(xi,yi) (xi - mean(x)) * (yi - mean(y)), x, y)))
}

getDenumerator = function(x) {
  return(sum(sapply(x, function(xi) {(xi - mean(x))^2})))
}

#(SE B0) ^2 = o^2 (1/n +  [    (avg(x)^2)/somm(   (xi-avg(x))^2  ) ]    )
getStandardErrors <- function(x,y) {
  RSE = getErrorStatistics(x,y)$RSE
  
  #estimated squared standard errors for B0 and B1
  SEB0 = sqrt(RSE^2 * (1/length(x) + X^2/getDenumerator(x)))
  SEB1 = sqrt(RSE^2 / getDenumerator(x))
  return(c(SEB0, SEB1))
}

getConfidenceIntervals <- function(beta, se) {
  #BI: B +- 2SE(B)
  return(c(betas[1] - 2*se[1], betas[1] + 2*se[1] , betas[2] - 2*se[2], betas[2] + 2*se[2]))
}