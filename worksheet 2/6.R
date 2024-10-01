cricket <- read.csv("battingbowling.csv")

#sub-setting the all rounders and creating a data frame
all_rounders <- subset( cricket, Batting>25, Bowling<40)
all_rounders <- data.frame( table( all_rounders$Team))

#team with the most and team with the least all-rounders
all_rounders$Var1 [which.max( all_rounders$Freq)]
all_rounders$Var1 [which.min( all_rounders$Freq)]