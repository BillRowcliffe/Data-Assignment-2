source('utils.R')
music=read.csv('music.csv')
str(music)

# install.packages('randomForest')
library(randomForest)
fit=randomForest(Decade~.,data=music)
confusion(music$Decade, predict(fit))
