#Question 1
{
  #baseline 48
  num1 <- numeric(length = 1e3)
  #8000
  object.size(num1)
  
  num2 <- numeric(length = 1e6)
  #8000000
  object.size(num2)
  
  #baseline 216
  mat1 <- matrix(runif(100*1000), nrow = 100, ncol = 1000)
  #8*1e5
  object.size(mat1)
  
  mat2 <- matrix(0, nrow = 100, ncol = 1000)
  #8*1e5
  object.size(mat2)
  
  #baseline224
  arr <- array(0, dim = c(100,100,100))
  #8*1e6
  object.size(arr)
}

#Question 2
{
  n <- 1e5
  p <- 1e4
  # generating arbitrary data
  dat <- matrix(runif(n*p), nrow = n, ncol = p)
  # making csv file from data
  setwd("C:/Users/MTH/Desktop/MTH208/worksheet 15")
  write.csv(dat, file = "bigData.csv", row.names = FALSE)
  
  temp <- read.csv("bigData.csv")
  
  # save dat
  save(dat, file = "largeData.Rdata")
  object.size(dat)
  
  load("largeData.Rdata")
}

#Question 3
{
  benchmark(sqrt(sum(c(1:1e6)^2)),norm(c(1:1e6),"2"),replications=1e3)
}

#Question 5
{
  stirling <- function(n)
  {
    return (lfactorial(n)-(n*log(n)-n+log(2*pi*n)/2))
  }
  
  x<-numeric(length=1e6)
  y<-numeric(length=1e6)
  
  for (i in 1:1e6)
  {
    x[i] <- i
    y[i] <- exp(stirling(i))
  }
  
  plot(x,y,type="l")
  stirling(1000)
}

#Question 6
{
  func <- function(n = 1e3)
  {
    nums <- 1:(n^2)
    mat <- matrix(nums, nrow = n, ncol = n)
    means <- apply(mat, 2, mean)
    norm.means <- sqrt(sum(means^2))
    return(norm.means)
  }
  
  
  
  func2 <- function(n = 1e3)
  {
    return (sqrt(sum(colMeans(matrix(c(1:n^2),nrow=n,ncol=n))^2)))
  }
  
  library(rbenchmark)
  benchmark(func(1e4),func2(1e4),replications = 10)
}


