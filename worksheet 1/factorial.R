factorial <- function(n){
  fact <- 1
  for (i in 1:n)
  {fact <- fact*i}
  return (fact)
}

factorial( readline (prompt = "enter your number:"))
