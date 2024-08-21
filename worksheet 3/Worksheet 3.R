#Question 1
{
  # n = number of coin tosses
  # size = 1 (tells R we are tossing a coin)
  # prob = probability of success
  
  # 1 = success (heads), 0 = failure (tails)
  
  #Part_a
  {
    observation <- rbinom(n = 1000, size = 1, prob = 0.5)
    prop_heads <- sum(observation)/1000
    cat("1000 fair coin tosses were simulated and the proportion of heads was",prop_heads,".", sep=" ")
  }
  
  #Part_b
  {
    observation <- rbinom(n = 1000, size = 1, prob = 0.3)
    prop_heads <- sum(observation)/1000
    cat("1000 fair coin tosses were simulated and the proportion of heads was",prop_heads, ".", sep=" ")
  }
}

#Question 2
{
  #Part_a
  {
    cat( "The randomly chosen ball is of", sample( x = c("red","green","blue"), size = 1, prob = c(3/7, 2/7, 2/7)), "colour","." , sep=" ")
  }
  
  #Part_b
  {
    A <- matrix( c(3,4,-1,1,5,2,-2,3,-2), nrow=3, ncol=3, byrow=TRUE )
    

    #sum-sq is the ||A_i|| term for each column i
    probs <- numeric(length = ncol(A))
    
    # a for loop to calclate sum-sq
    for (i in 1:ncol(A)){
      Ai <- A[,i]
      probs[i] <- sqrt(sum(Ai^2))
    }
    
    #calculating probabilities pi for each column i
    probs <- probs/sum(probs)
    
    #choosing column
    choice <- sample ( x= 1:ncol(A), size = 1, prob = probs)
    cat("The chosen column is column", choice , "which has values", A[,choice], ".", sep=" ")
  }
  
  #Part_c
  {
    distance <- runif(n = 1, min = -2.5, max = 2.5)  #finding where the dart lands
    direction <- if (distance < 0){"left"} else{"right"}  #negative for left side and positive for the right side
    if (distance != 0) {cat ( "The dart falls", abs(distance) , "cm away from the centre of the thread towards the", direction , ".", sep=" " ) } else cat( "The dart falls at the centre of the thread." )
  }
}

#Question_3
{
  #Part_a
  {
  exceed <- function(){
    count <- 0
    sum <- 0
    while( sum <= 1 )
    {
      sum <- sum + runif (n=1, min=0, max=1)
      count <- count + 1
    }
    return(count)
  }
    
  }
  
  #Part_b
  {store <- numeric(length = 1000)
  for(r in 1:1000)
  {
    store[r] <- exceed()
  }
  }
  
  #Part_c
  {
  cat ("The average of the 1000 outputs is", mean(store), sep=" ")
  }
}

#Question_4
{
  #Part_a
  {
  fn <- function(age)
  {
    attempts <- 0
    cand_left <- age
    while (cand_left != 0)
    {
      blow <- round(runif(1, min = 1, max = cand_left),0)
      cand_left <- cand_left- blow
      attempts <- attempts + 1
    }
    return (attempts)
  }
  age <- as.integer(readline (prompt="Please enter your age: "))
  fn(age)
  }
    
  #Part_b
 {
  r <- numeric(length=1000)
  age <- as.integer(readline (prompt="Please enter your age: "))
  for (i in 1:1000){
    r[i] <- fn(age)
  }
  }
}

  #Part_c
  {
    cat("on average i need to blow at the cake", round(mean(r),0) , "times", sep=" ")
  }

#Part_d
{
  fn <- function(age)
  {
    attempts <- 0
    cand_left <- age
    while (cand_left != 0)
    {
      blow <- round(runif(1, min = 1, max = cand_left),0)
      cand_left <- cand_left- blow
      attempts <- attempts + 1
    }
    return (attempts)
  }

  r <- numeric(length=1000)
  for (i in 1:1000){
    r[i] <- fn(30)
  } 
  
  cat("on average i need to blow at the cake", round(mean(r),0) , "times on my 30th birthday", sep=" ")
}  






