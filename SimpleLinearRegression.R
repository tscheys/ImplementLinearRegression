# Implementation of Simple Linear Regression 

implementLinearRegression

vector met x waarden

# x and y variables to test code 
x <- rnorm(100)
y <- x + rnorm(100, mean=5, sd=2)

vector met y waarden 

getParams(vector x, vector y)

avg(x)

avg(y)

B1 = somm((xi - avg x)(yi-avg y)) / somm(xi - avg x)^2

B0 = avg(y) - B1 * avg(x)

formule met B0 en B1 invullen om parameters te bepalen 

ook formule voor SE parameters maken

(SE B0) ^2 = o^2 (1/n +  [    (avg(x)^2)/somm(   (xi-avg(x))^2  ) ]    )



ook formule voor betrouwbaarheidsintervallen maken (95%)

BI: B +- 2SE(B)

formule om p waarde geassocieerd met Ho: B=0 te bepalen 
