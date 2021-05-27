table = read.csv("results_TwoAbsorbingStates.csv")
selected = table[table$V==1,]
mean(selected$Y)
