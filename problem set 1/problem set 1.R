#Practice R Questions

#Question_1
{
  odd <- seq (1, 2000, 2)
}

#Question_2
{
  x <- integer(length=500)
  x[1] <- 0
  x[2] <- 1
  for (i in 3:500){
    x[i] <- x[i-1] + x[i-2]
  }
  x
}

#Question_3
{
  fn<-function(){
    x <- sample(1:6,size=1,prob=c(1,1,1,1,1,1))
    if (x%%2==0){return (1)}
  }
  fn()
}

#Question_4
{
  fn <- function(){
    x <- rbinom (n=1, size=15, prob=1/2)
    if (x<8){return ("lose")} else{return("win")}
  }
  fn()
}

#Question_5
{
  matrix(1, nrow = 5, ncol = 5)
}

#Question_6
{
  x <- matrix(0, nrow = 5, ncol = 5)
  for (i in 1:5){
    x[i,i] <- i
  }
  x
}

#Question_7
{
  matrix( data = sample( x = 1:6, size = 100, replace = TRUE), nrow = 10, ncol = 10)
}

#Question_8
{
  fn <- function( n, rho){
    x <- matrix ( rho, nrow = n, ncol = n)
    for (i in 1:nrow(x)){
      x[i,i] <- 1
    }
    return (x)
  }
  fn(5,2)
}

#Question_9
{
  fn <- function( n, rho){
    x <- matrix (nrow = n, ncol = n)
    for (i in 1:nrow(x)){
      for (j in 1:ncol(x)){
        x[i,j] <- rho^abs(i-j)
      }
    }
    return (x)
  }
  a <-fn(5,2)
}

#Question_10
{
  fn <- function (x){
    x1 <- matrix (nrow = nrow(x), 
                  ncol = if(ncol(x)%%2==0){ncol(x)/2} else{(ncol(x)+1)/2})
    for (i in 1:nrow(x1)){
      for (j in 1:ncol(x1)){                     
        x1[i,j] <- x[i,2*j-1]
      }
    }
    return (x1)
  }
}

{
  fn <- function (x){
    x1 <- x[, seq(1,ncol(x),2)]
    return(x1)
  }
  fn(x)
}

#Question_11
{
  a <- array( dim = c(10, 4, 6, 5))
}

#Worksheet based Questions

#Question_1
{
  fn <- function(r){
    area <- pi*r^2
    return (area)
  }
  r <- as.numeric( readline( prompt = "Enter the radius r of circle"))
  cat( "area of circle with radius", r, "is", fn(r), sep=" ")
}

#Question_2
{
  fn <- function(x,y){
    if ( x>y){ return (x)} else{ if( y>x){ return (y)} else {return ("equal")}}
  }
  fn(2,2)
}

#Question_3
{
  x <- sample( 1:6, size = 1000, replace = TRUE)
  sum( x%%2==0)
}

#Question_4
{
  x <- runif( n = 1000, min = 0, max = 1)
  prop <- sum (0.1<x & x<0.2)/1000
}

#Question_5
{
  fn <- function()
  {
    char <- c( "Harry", "Dumbledore", "Hermione", "Ron", "Neville", "Mcgonagall", "Dobby")
    packets <- c(0,0,0,0,0,0,0)
    probs <- c(0.25, 0.20, 0.20, 0.15, 0.10, 0.05, 0.05)
    while (min(packets)==0){
      toy <- sample(char, size = 1, prob = probs)
      index <- which(char==toy)
      packets[index] <- packets[ index] + 1
    }
    return (sum(packets))
  }
  
  n <- integer (length = 1000)
  for (i in 1:1000){
    n[i] <- fn()
  }
  mean (n)
}

#Question_6
{
  fn <- function(){
    full <- 100
    half <- 0
    day <- 1
    tab <- "full"
    while (tab!="half"){
      if (tab == "full"){
        full <- full - 1
        half <- half + 1}
      day <- day + 1
      tab <- sample ( c("full", "half"), size = 1, prob = c(full,half))
      }
    day
  }
  
  n <- integer( length = 1000)
  for (i in 1:1000){
    n[i] <- fn()
  }
  mean(n)
}

#Question_7
{
  #1
  MontyHall <- function(){
    choice <- sample( c("goat", "car"), size = 1, prob = c(2,1))
    if (choice == "goat"){
      return (1)
    }
    if (choice == "car"){
      return (0)
    }
  }
  MontyHall()
  
  #2
  n <- integer (length = 1000)
  for ( i in 1: 1000){
    n[i]<- MontyHall()
  }
  sum(n)/1000
}

#Question_8
{
  library( imager)
  prop.color <- function(img, col){
    count <- 0
    img.mat <- as.array(img[,,1,])
    for (i in 1:dim(img.mat)[1]){
      for (j in 1:dim(img.mat)[2]){
        dist <- norm(img.mat[i,j,]-col, type='2')
        if (dist<0.5){ count <- count + 1}
      }
    }
    prop <- count/(dim(img.mat)[1]*dim(img.mat)[2])
    return (prop)
  }
  img <- load.image("dog.jpeg")
  col <- c(1,1,0)
  prop.color(img,col)
}

#Question_9
{
  img <- load.image("dog.jpeg")
  img.mat <- as.array(img[,,1,])
  mirror.mat <- array( dim = c(dim(img.mat)[1],dim(img.mat)[2],3))
  x <- seq(dim(img.mat)[1],1,-1)
  for (i in 1:dim(img.mat)[1]){
    for (j in 1:dim(img.mat)[2]){
      mirror.mat[x[i],j,] <- img.mat[i,j,]
    }
  }
  mirror <- as.cimg(mirror.mat)
  plot (mirror)
}

#Question_10
{
  img <- load.image("col3.png")
  plot(img)
  img.mat <- as.array(img[,,1,])
  for (i in 1:dim(img.mat)[1]){
    for (j in 1:dim(img.mat)[2]){
      r <- img.mat[i,j,][1]
      g <- img.mat[i,j,][2]
      b <- img.mat[i,j,][3]
      if (r<0.10 & g<0.10 & b<0.10){
        img.mat[i,j,] <- c(0,0,0)
      }
    }
  }
  plot(as.cimg(img.mat))
}

#Question_11_incomplete
{
  A <- matrix( runif(1000*1000, min = 0, max = 1), nrow = 1000, ncol = 1000)
  norm_ <- numeric (length = 1000)
  for (i in 1:1000){
      norm_[i] <- norm(A[,i], type = '2')
  }
  sapply(A[,],norm)
}
