table = read.csv("results_ConditionalAccumulatedReward.csv")
selected = table[table$V==1,]
mean(selected$Y)
