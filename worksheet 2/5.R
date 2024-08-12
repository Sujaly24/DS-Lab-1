seat <- read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")

roll_number <- seat$Roll.No

count_BS <- 0
for (i in roll_number)
{if (99999<i && i<1000000)
  count_BS <- count_BS+1}

count_MSc <- 0
for (i in roll_number)
{if (99999999<i && i<1000000000)
  count_MSc <- count_MSc+1}

count_BS<-sum(roll_number<1e7)
count_MSc<-sum(roll_number>1e7)
