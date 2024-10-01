#Question_1
{
  getwd()
  setwd("C:/Users/MTH/Desktop/MTH208/worksheet 11")
  data <- read.csv("movie_unweighted.csv")
}

#Question_2
{
  ?hist()
  #Part_a
  {
    hist(data$ratings,main="Histogram of Ratings",xlab="Ratings")
  }
  #Part_b
  {
    hist(data$ratings,main="Histogram of Ratings",xlab="Ratings",col="white")
  }
  #Part_c
  {
    par(mfrow = c(1,2))
    hist(data$ratings,main="Histogram of Ratings",xlab="Ratings",xlim=c(7.5,10))
    abline(v=mean(data$ratings),col="red")
    abline(v=median(data$ratings),col="green")
    hist(data$unweighted,main="Histogram of Unweighted",xlab="Unweighted",xlim=c(7.5,10))
    abline(v=mean(data$unweighted),col="red")
    abline(v=median(data$unweighted),col="green")
    #Ratings are positively skewed while Unweighted are also positively skewed but less than Ratings
    #Modal Class is 8.0-8.5 for Ratings and Unweighted both.
  }
  #Part_d
  {
    var(data$ratings)
    var(data$unweighted)
  }
}

#Question_3
{
  ?boxplot
  #Part_a
  {
    par(mfrow=c(1,1))
    boxplot(data$ratings,main="Boxplot of Ratings")
  }
  #Part_b
  {
    par(mfrow=c(1,1))
    boxplot(data$ratings,main="Boxplot of Ratings",col="pink")
  }
  #Part_c
  {
    r1 <- range(data$ratings)
    r1[2]-r1[1]
    r2 <- range(data$unweighted)
    r2[2]-r2[1]
  }
  #Part_d
  {
    r1 <- quantile(data$ratings)
    IQR1 <- r1[4]-r1[2]
    r2 <- quantile(data$unweighted)
    IQR2 <- r2[4]-r2[2]
  }
}

#Question_4
{
  ?boxplot
  par(mfrow=c(1,2))
  boxplot(data$ratings,ylim=c(7.5,10))
  boxplot(data$unweighted,ylim=c(7.5,10))
}

#Question_5
{
  par(mfrow=c(1,1))
  hist(data$ratings,main="Histogram of Adjusted and Unweighted Ratings",xlab="Ratings",ylim=c(0,140),xlim=c(7.5,10),col = adjustcolor("red", alpha.f = .5) )
  par(new=TRUE)
  hist(data$unweighted,main="Histogram of Adjusted and Unweighted Ratings",xlab="Ratings",ylim=c(0,140),xlim=c(7.5,10),col = adjustcolor("red", alpha.f = .5) )
  legend("topright",legend=c("Weighted","Unweighted"),fill = c("red",adjustcolor("red", alpha.f = .5)) )
}

#Question_6
#The ratings are Right skewed in both the cases