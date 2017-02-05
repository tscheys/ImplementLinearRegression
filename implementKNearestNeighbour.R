# set up test data set 
x1 <- rnorm(100)
x2 <- rnorm(100)
y <- as.factor(rep(c(0,1),times=50))
x <- cbind(x1,x2)

#calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

KNN <- function(K, X, y) {
  #predicts class labels with KNN method
  #K = number of neighbours, constant
  #X = predictors for y, matrix 
  #y = class labels for observations, vector
  
  #prediction vector 
  predictions = rep(0, length(y))
  
  #for all observations, select label that is most prevalant amongst its K nearest neighbours
  for(i in 1:nrow(x)) {
    #select one observation
    obs <- x[i,]
    #calculate euclidian distance between observation and all other observations
    euclidians <- t(t(x) - obs)^2
    #select K indexes with shortest euclidian distance 
    indexes <- order(rowSums(euclidians))[1:K]
    #get associated labels of K observations 
    labels <- y[indexes]
    #select most prevalant label as prediction
    predictions[i] <- Mode(labels)
  }
  #give predictions 0,1 coding
  return(predictions - 1)
}

#test function
preds <- KNN(10,x,y)
#check confusion matrix
table(preds, y)

#lessons learned: 
#order is sort for vector indexes
#cmd enter on function runs multiple lines 
#microbenchmark is a fast way to compare lines of code 
#diag() might be faster than t() on large datasets 
#to do: algorithm selects observation it needs to predict
