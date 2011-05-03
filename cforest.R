source('utils.R')
music=read.csv('music.csv')

## install.packages('party')
cf.time=system.time({
library(party)
fit2=cforest(Decade~., data=music)
y.pred=predict(fit2)
print(confusion(music$Decade,y.pred))
})
save.image('cforest.RData')
