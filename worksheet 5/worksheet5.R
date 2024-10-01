#Question_1
{
  attempts<-function(age)
  {
    count <- 0
    remain <- age # age no. of candles remain in the beginning
    while(remain>0)
    {
      count<-count+1# randomly choose any number between 1 and remain
      blow_out <- sample(1:remain,size =1)
      remain <- remain- blow_out
    }
    return(count)
  }
  
  att_vec <- numeric(length =1e3)
  for(i in 1:1e3)
  {
    att_vec[i]<-attempts(25)
  }
  att_vec2 <- replicate(1000, attempts(25))
}

#Question_2
{
  library(rbenchmark)
  benchmark({
    att_vec <- numeric(length = 1e3)
    for(i in 1:1e3)
    {
      att_vec[i] <- attempts(25)
    }},
    replicate(1e3, attempts(25)), replications = 100)
}

#Question_3
{
  benchmark({
    att_vec <- numeric(length = 1e4)
    for(i in 1:1e4)
    {
      att_vec[i] <- attempts(25)
    }},
    replicate(1e4, attempts(25)), replications = 20)
}

#Question_4
{
  att_vec <- NULL
  for(i in 1:1e4)
  {
    att_vec <- c(att_vec, attempts(25))
  }
  benchmark({
    att_vec <- numeric(length = 1e4)
    for(i in 1:1e4)
    {
      att_vec[i] <- attempts(25)
    }},
    replicate(1e4, attempts(25)),
    {att_vec <- NULL
    for(i in 1:1e4)
    {
      att_vec <- c(att_vec, attempts(25))
    }},
    replications = 25)
}

#Question_5
{
  m <- as.integer( readline( prompt="Enter the number of rows"))
  n <- as.integer( readline( prompt="Enter the number of columns"))
  x <- matrix( runif(m*n), nrow = m, ncol = n)
  benchmark(colMeans(x),apply(x, 2, mean),replications = 1000)
}

{x<- runif(1e5)
  benchmark(norm(x,"2"),sqrt(sum(x^2)),replications=1000)}

#Question_6
{
  benchmark(
    {
      for ( i in 1:1e4)
      {
        runif(1)
      }
    },
    runif(1e4), replications = 1000
  )
}

#Question_7
{
  num1 <- numeric(length = 1e3)
  num2 <- numeric(length = 1e6)
  mat1 <- matrix(runif(100*1000), nrow = 100, ncol = 1000)
  mat2 <- matrix(0, nrow = 100, ncol = 1000)
  arr <- array(0, dim = c(100,100,100))
  object.size(arr)
}
