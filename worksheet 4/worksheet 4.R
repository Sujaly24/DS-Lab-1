install.packages('imager')
library(imager)
getwd()
setwd('C:/Users/Sujal Yadav/Desktop/IITK/MTH308/worksheet 4')
dog <- load.image("dog.jpeg")
dim(dog) # stored as RGB
plot(dog) # plot image
graydog <- grayscale(dog)
plot(graydog)
dim(graydog)
# Extract the black and white image as matrix
gray.mat <- as.matrix(graydog[,,1,1])
dim(gray.mat)
# Extracts the array will all three rgb channels
col.mat <- as.array(dog[, ,1, ])
dim(col.mat)
# Vertical cropping
cropped.mat <- col.mat[1:300, , ]
crop.dog <- as.cimg(cropped.mat)
plot(crop.dog)

as.array(dog[,,1,])

#Question_1
{
  dog <- load.image ("dog.jpeg")
  coord <- integer( length=2 )
  min <- 1
  dog.mat <- as.array(dog[,,1,])
  for ( i in 1:dim(dog.mat)[1] ){
    for ( j in 1:dim(dog.mat)[2] ){
      dist <- norm( dog.mat[i,j,]-c(0,1,0), type="2" )
      if (dist<min){
        coord <- c(i,j)
        min <- dist
      }
    }
  }
  plot(dog)
  points( x=coord[1], y=coord[2], type="p", col="green" )
}

#Question_2
{
  # for purest red color
  dog <- load.image ("dog.jpeg")
  coord <- integer( length=2 )
  min <- 1
  dog.mat <- as.array(dog[,,1,])
  for ( i in 1:dim(dog.mat)[1] ){
    for ( j in 1:dim(dog.mat)[2] ){
      dist <- norm( dog.mat[i,j,]-c(1,0,0), type="2" )
      if (dist<min){
        coord <- c(i,j)
        min <- dist
      }
    }
  }
  plot(dog)
  points( x=coord[1], y=coord[2], type="p", col="red" )
  
  # for purest blue color
  dog <- load.image ("dog.jpeg")
  coord <- integer( length=2 )
  min <- 1
  dog.mat <- as.array(dog[,,1,])
  for ( i in 1:dim(dog.mat)[1] ){
    for ( j in 1:dim(dog.mat)[2] ){
      dist <- norm( dog.mat[i,j,]-c(0,0,1), type="2" )
      if (dist<min){
        coord <- c(i,j)
        min <- dist
      }
    }
  }
  plot(dog)
  points( x=coord[1], y=coord[2], type="p", col="blue" )
}

#Question_3 incomplete
{
  #col1 <- load.image ("col1.png")[1,1,1,]
  col2 <- load.image ("col2.png")[1,1,1,]
  #col3 <- load.image ("col3.png")[1,1,1,]
  #coord1 <- integer( length=2 )
  coord2 <- integer( length=2 )
  #coord3 <- integer( length=2 )
  prim <- array(dim=c(3,3))
  prim[1,] <- c(1,0,0)
  prim[2,] <- c(0,1,0)
  prim[3,] <- c(0,0,1)
  #which.min( norm( col1- prim, type="2"))
  for (i in 1:3){
    
  }
  which.min( norm( col2- prim, type="2") )
  #which.min( norm( col3- prim, type="2") )
}

#Question_4
{
  land1 <- load.image ("land1.jpeg")
  land2 <- load.image ("land2.jpeg")
  count1 <- 0
  count2 <- 0
  for (i in 1:dim(land1)[1]){
    for (j in 1: dim(land1)[2]){
      dist <- norm( land1[i,j,1,]- c(1,1,1), type="2")
      if (dist<0.1){count1 <- count1 + 1}
    }
  }
  for (i in 1:dim(land2)[1]){
    for (j in 1: dim(land2)[2]){
      dist <- norm( land2[i,j,1,]- c(1,1,1), type="2")
      if (dist<0.1){count2 <- count2 + 1}
    }
  }
  prop1 <- count1/(dim(land1)[1]*dim(land1)[2])
  prop2 <- count2/(dim(land2)[1]*dim(land2)[2])
  if (prop1<prop2) {cat("land2 has a lot of snow")} else{cat("land1 has a lot of snow")}
}

#Question_5
{
  fn <- function(img){
    img.mat <- as.array(img[,,1,])
    x <- seq( dim(img)[1], 1, -1)
    y <- seq( dim(img)[2], 1, -1)
    img_180.mat <- img.mat
    for (i in 1:dim(img)[1]){
      for (j in 1: dim(img)[2]){
        img_180.mat[x[i],y[j],] <- img.mat[i,j,]
      }
    }
    img_180 <- as.cimg(img_180.mat)
    plot(img_180)
  }
  fn(land1)
}

#Question_6
{
  fn <- function(img){ 
    img.mat <- as.array(img[,,1,]) 
    y <- seq( 1, dim(img)[1]) 
    x <- seq( dim(img)[2], 1, -1) 
    img_90.mat <- array(dim=c(dim(img.mat)[2],dim(img.mat)[1],3)) 
    for (i in 1:dim(img)[1]){ 
      for (j in 1: dim(img)[2]){ 
        img_90.mat[x[j],y[i],] <- img.mat[i,j,] 
      } 
    } 
    img_90 <- as.cimg(img_90.mat) 
    plot(img_90) 
  } 
  fn(land1) 
}

#Question_7
{
  fn <- function(img){ 
    img.mat <- as.array(img[,,1,]) 
    y <- seq( dim(img)[1], 1, -1) 
    x <- seq( 1, dim(img)[2]) 
    img_90.mat <- array(dim=c(dim(img.mat)[2],dim(img.mat)[1],3)) 
    for (i in 1:dim(img)[1]){ 
      for (j in 1: dim(img)[2]){ 
        img_90.mat[x[j],y[i],] <- img.mat[i,j,] 
      } 
    } 
    img_90 <- as.cimg(img_90.mat) 
    plot(img_90) 
  } 
  fn(land1)
}

#Question_8 incomplete
{
  dog<-load.image("dog.jpeg")
  plot(dog)
  dog.mat <- as.array(dog[,,1,])
  cropped.mat <- dog.mat[1:600,1:600 , ]
  crop.dog <- as.cimg(cropped.mat)
  plot(crop.dog)
}

#Question_9
{
  
}
