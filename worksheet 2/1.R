factorial <- function(n){     #function to calculate factorials
  fact <- 1
  for (i in (1:n)){           #for loop to calculate the products
    fact <- fact*i
  }
  return (fact)
}

#fact2 <- function(n){ prod(1:n)}

#to take user input
factorial( readline( prompt = "Enter the number you want factorial for:"))
