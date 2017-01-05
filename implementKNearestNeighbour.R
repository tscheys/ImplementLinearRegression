x <- rnorm(100)
y <- as.factor(rep(c(0,1),times=50))
x0 <- 0.4


KNN <- function(K, x, y, x0) {
  distances = sqrt((x-x0)^2)
  neighbours = sort.int(distances, decreasing=T, index.return = T)
  df = data.frame(distances,y)
  # order df and largest K values
  df = df[with(df, order(-distances)),][1:K,]

}