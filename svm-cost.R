source('utils.R')
music=read.csv('music.csv')
library(e1071)
tmp = list()
cost=c(.1,1,10,50)
sink('svm-cost.txt')
png('accuracy-cost.png',width=800,height=600)
plot(cost,rep(1,length(cost)),type='n',ylim=c(.1,1))
for (i in 1:length(cost)) {
    fit = svm(Decade~., data=music[,c("timbrecov48", "timbreav12",
                         "timbrecov36", "timbrecov59", "timbrecov46", "timbrecov35",
                         "timbrecov3", "timbrecov4", "timbrecov56", "timbrecov47",
                         "timbrecov55", "timbrecov40", "timbrecov34", "timbrecov9",
                         "timbrecov1", "timbreav10", "timbreav1", "timbrecov71",
                         "timbrecov61", "timbreav2", "timbreav3", "timbreav7",
                         "timbrecov67", "timbreav6", 'Decade')], gamma=.2,cost=cost[i], cross=2)
    tmp[[i]]=fit
    print(cost[i])
    print(fit$tot.accuracy)
    print(fit$accuracies)
    print((conf<-confusion(music$Decade, predict(fit))))
    points(cost[i],fit$tot.accuracy/100,pch=20)
    points(cost[i],conf$accuracy,pch=19,col='red')
    timestamp()
    cat('\n\n')
}
dev.off()
sink()
save(tmp,file='svm-cost.RData')
