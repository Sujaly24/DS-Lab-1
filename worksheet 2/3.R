seat <- read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")

seat$Seat.Number[ which( seat$Roll.No == 241080101)]

myrow <- seat$Roll.No == 241080101
myseat <- seat[myrow, "Seat.Number"]
myseat