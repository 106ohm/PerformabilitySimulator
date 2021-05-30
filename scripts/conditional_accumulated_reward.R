library(e1071)
table = read.csv("results_ConditionalAccumulatedReward.csv")
selected = table[table$V==1,]
mean(selected$Y)
moment(selected$Y,order=2,center=FALSE)
moment(selected$Y,order=3,center=FALSE)
moment(selected$Y,order=4,center=FALSE)

