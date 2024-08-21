f <- function(n){
  return ( (1+1/n) ^n )
}

plot( x<- c(1:1000), y<- f(c(1:1000)), type='l', xlab="n", ylab="f(n)", main="Euler's Function")
abline(h=exp(1), col='red')