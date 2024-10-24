library(Rcpp)

# Consider the following R function
addR <- function(x, y)
{
  return(x + y)
}
library(Rcpp)
# cppFunction() is a function that takes a bunch of C++ codes (all in quotes)
# and then saves and compiles it.
# In C++, we have to declare the type of every object.
# The function we are making is addC() which accepts two integers
# and returns the sum of the two integers.
cppFunction('int addC(int x, int y) {
int sum = x + y;
return sum;
}')

addR(3, 4)

addC(3, 4)

# for Rcpp usage put
# cppFunction('
output_type name_of_func(input_type input1, input_type input2, ...){
  line1 ... ;
  line2 ... ;
  .
  .
  return output;
  # ')
}

# In R
EucR <- function(x, y)
{
  rtn <- sqrt(sum( (x-y)^2 ))
  return(rtn)
}
# In C++ using Rcpp
cppFunction('double EucC(NumericVector x, NumericVector y) {
double track = 0;
int n = x.size();
for(int i = 0; i < n; i++){
track = track + pow( (x[i] - y[i]), 2);
}
track = sqrt(track);
return track;
}
')

x <- 1:10
y <- 3:12
# all.equal checks whether result is the same
all.equal(EucR(x, y),EucC(x, y))

#Question_1
{
  x <- runif(1e4)
  y <- runif(1e4)
  library(rbenchmark)
  benchmark(EucR(x,y),EucC(x,y), replications = 1e5)
}

#Question_2
{
  func <- function(vec)
  {
    n <- length(vec)
    # for tracking sum and log
    sum.log <- 0
    log.of.vec <- numeric(length(n))
    # calculating logs and sum for each element
    for(i in 1:n)
    {
      log.of.vec[i] <- log(vec[i])
      sum.log <- sum.log + log.of.vec[i]
    }
    # fraction
    frac <- log.of.vec/sum.log
    return(frac)
  }
  
  funcR <- function(vec)
  {
    n <- length(vec)
    log.of.vec <- log(vec)
    sum.log <- sum(log.of.vec)
    frac <- log.of.vec/sum.log
    return(frac)
  }
  
  cppFunction('NumericVector funcC(NumericVector vec){
  
  int n = vec.size();
  int sum = 0;
  NumericVector logvec(n);
  NumericVector frac(n);
  for (int i = 0; i<n; i++){
  logvec[i] = log(vec[i]);
  sum += logvec[i];
  }
  frac=logvec/sum;
  return frac;
  }
  ')
  
  vec <- runif(1e2,min = 1, max = 1e10)
  benchmark(func(vec), funcR(vec), funcC(vec), replications = 1e6)
}


#Question_3
{
  cppFunction('NumericMatrix funcC(NumericMatrix a, NumericMatrix b){
  int r0 = a.nrow();
  int c0 = a.ncol();
  NumericMatrix c(r0,c0);
  
  for(int i = 0; i<r0; i++){
  for( int j = 0; j<c0; j++){
  c(i,j) = a(i,j) + b(i,j);
  }
  }
  return c;
  }
  
  ')
  a <- matrix(1,3,3)
  b <- matrix(2,3,3)
  funcC(a,b)
}

#Question_4
{
  cppFunction('NumericVector funcC(NumericMatrix a){
  int r0 = a.nrow();
  int c0 = a.ncol();
  NumericVector c(c0);
  
  for(int i = 0; i<c0; i++){
  for( int j = 0; j<r0; j++){
  c[i] += a(j,i);
  }
  }
  return c;
  }
  
  ')
  a <- matrix(1:9,3,3)
  funcC(a)
}

#Question_5
{
  cppFunction('NumericVector funcC(NumericVector a){
  int n = a.size();
  NumericVector b = 
  LogicalVector ;
              ')
  a <- matrix(1:9,3,3)
  funcC(a)
}