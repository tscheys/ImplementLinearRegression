# Implementation of Simple Linear Regression 

getBetas = function(x, y) {
  #test if x and y are of equal length
  if(length(x) != length(y)) {
    tryCatch(simpleError("predictor and response are not of equal length!"))
  } else {
    X = mean(x)
    Y = mean(y)
    numerator = 0
    denumerator = 0
    
    for (i in 1:length(x)){
      #calculating B1
      #B1 = somm((xi - avg x)(yi-avg y)) / somm(xi - avg x)^2
      numerator = numerator + (x[i] - X) * (y[i] - Y)
      denumerator = denumerator + (x[i] - X)^2
    }
    B1 = numerator / denumerator
    #B0 = avg(y) - B1 * avg(x)
    B0 = Y - B1 * X
    
    return(c(B0,B1))
  }
}


getFittedValues <- function(coefficients, predictor) {
  fittedValues = rep(0, length(predictor))
  for(i in 1:length(predictor)) {
    #fitted.values = b0 + b1*x
    fittedValues[i] = coefficients[1] + coefficients[2] * predictor[i]
  }
  return(fittedValues)
}

#residuals = y - yhat
residuals <- y - getFittedValues(getBetas(x,y), x)
#RSS = (yi - y)^2 
RSS = sum((residuals)^2)
#RSE = sqrt(RSS/n-2)
RSE = sqrt(RSS/(length(x) - 2))

#ook formule voor SE parameters maken

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

getConfidenceIntervals <- function(betas, standarderrors) {
  return(c(betas[1] - 2*standarderrors[1], betas[1] + 2*standarderrors[1] , betas[2] - 2*standarderrors[2], betas[2] + 2*standarderrors[2]))
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