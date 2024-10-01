getwd()
setwd('C:/Users/MTH/Desktop/MTH208/worksheet-4-Sujaly24-main')
library(imager)
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

#Question_1
{
  dog <- load.image("dog.jpeg")
  plot(dog)
  mat.dog<-as.array(dog[,,1,])
  min<-1
  for (i in 1:dim(mat.dog)[1]){
    for (j in 1:dim(mat.dog)[2]){
      dist<-norm(mat.dog[i,j,]-c(0,1,0),"2")
      if (dist<min){
        min<-dist
        coor<-c(i,j)
      }
    }
  }
  points(x=coor[1],y=coor[2],type='p',col='red',pch=16)
}

#Question_2
{
  #purest red
  dog <- load.image("dog.jpeg")
  plot(dog)
  mat.dog<-as.array(dog[,,1,])
  min<-1
  for (i in 1:dim(mat.dog)[1]){
    for (j in 1:dim(mat.dog)[2]){
      dist<-norm(mat.dog[i,j,]-c(1,0,0),"2")
      if (dist<min){
        min<-dist
        coor<-c(i,j)
      }
    }
  }
  points(x=coor[1],y=coor[2],type='p',col='red',pch=16)
  
  #purest blue
  dog <- load.image("dog.jpeg")
  plot(dog)
  mat.dog<-as.array(dog[,,1,])
  min<-1
  for (i in 1:dim(mat.dog)[1]){
    for (j in 1:dim(mat.dog)[2]){
      dist<-norm(mat.dog[i,j,]-c(0,0,1),"2")
      if (dist<min){
        min<-dist
        coor<-c(i,j)
      }
    }
  }
  points(x=coor[1],y=coor[2],type='p',col='red',pch=16)
}

#Question_3
#{
#  col1<-as.array(load.image("col1.png")[,,1,])
#  col2<-as.array(load.image("col2.png")[,,1,])
#  col3<-as.array(load.image("col3.png")[,,1,])
#  col1
#  red<-which(col1==1)
#}

#Question_4
{
  land1 <- load.image("land1.jpeg")
  mat.land1<-as.array(land1[,,1,])
  count1<-0
  for (i in 1:dim(mat.land1)[1]){
    for (j in 1:dim(mat.land1)[2]){
      if (norm(mat.land1[i,j,]-c(1,1,1),"2")<0.01){
        count1 <- count1+1
      }
    }
  }
  prop1<-count1/(dim(mat.land1)[1]*dim(mat.land1)[2])
  
  land2 <- load.image("land2.jpeg")
  mat.land2<-as.array(land2[,,1,])
  count2<-0
  for (i in 1:dim(mat.land2)[1]){
    for (j in 1:dim(mat.land2)[2]){
      if (norm(mat.land2[i,j,]-c(1,1,1),"2")<0.01){
        count2 <- count2+1
      }
    }
  }
  prop2<-count2/(dim(mat.land2)[1]*dim(mat.land2)[2])
}


#Question_5
plot(imrotate(dog,180))
