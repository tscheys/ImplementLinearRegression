# Implementation of Simple Linear Regression 

# x and y variables to test code 
set.seed(100)
x <- rnorm(100)
y <- x + rnorm(102, mean=5, sd=2)

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
    
    B0 = Y - B1 * X
    
    return(c(B1,B0))
  }
}

test <- getBetas(x,y)

#B1 = somm((xi - avg x)(yi-avg y)) / somm(xi - avg x)^2

#B0 = avg(y) - B1 * avg(x)

#formule met B0 en B1 invullen om parameters te bepalen 

#ook formule voor SE parameters maken

#(SE B0) ^2 = o^2 (1/n +  [    (avg(x)^2)/somm(   (xi-avg(x))^2  ) ]    )



#ook formule voor betrouwbaarheidsintervallen maken (95%)

#BI: B +- 2SE(B)

#formule om p waarde geassocieerd met Ho: B=0 te bepalen 
