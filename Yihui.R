source('utils.R')
music=read.csv('music.csv')
str(music)
nrow(music)

##################### Random forests ########################
# install.packages('randomForest')
library(randomForest)
fit1=randomForest(Decade~.,data=music, ntree=1000)
confusion(music$Decade, predict(fit1))
#############################################################


################### selecting variables with LASSO ##########
## install.packages('glmnet')
library(glmnet)
glmnet(music$Decade,as.matrix(music[,-91]),family='multinomial')
#############################################################



################### a long journey in SVM ###################
install.packages('e1071')
library(e1071)
fit = svm(Decade~.,data=music)
y=predict(fit)
confusion(music$Decade,y)

fit = svm(Decade~.,data=music,cross=10,kernel='linear')
y=predict(fit)
confusion(music$Decade,y)

fit = svm(Decade~.,data=music,cross=10,kernel='polynomial')
y=predict(fit)
confusion(music$Decade,y)

fit = svm(Decade~.,data=music,cross=10,kernel='sigmoid')
y=predict(fit)
confusion(music$Decade,y)


### tune the gamma paramter
source('utils.R')
music=read.csv('music.csv')
library(e1071)
tmp = list()
g=c(seq(.01,.1,.01),seq(.11,1,.05),seq(1,4,1))
sink('svm-tune.txt')
png('accuracy.png',width=800,height=600)
plot(g,rep(1,length(g)),type='n',ylim=c(.1,1))
for (i in 1:length(g)) {
    fit = svm(Decade~., data=music[,c("timbrecov48", "timbreav12",
                         "timbrecov36", "timbrecov59", "timbrecov46", "timbrecov35",
                         "timbrecov3", "timbrecov4", "timbrecov56", "timbrecov47",
                         "timbrecov55", "timbrecov40", "timbrecov34", "timbrecov9",
                         "timbrecov1", "timbreav10", "timbreav1", "timbrecov71",
                         "timbrecov61", "timbreav2", "timbreav3", "timbreav7",
                         "timbrecov67", "timbreav6", 'Decade')], gamma=g[i], cross=5)
    tmp[[i]]=fit
    print(g[i])
    print(fit$tot.accuracy)
    print(fit$accuracies)
    print((conf<-confusion(music$Decade, predict(fit))))
    points(g[i],fit$tot.accuracy,pch=20)
    points(g[i],conf$accuracy,pch=19,col='red')
    timestamp()
    cat('\n\n')
}
dev.off()
sink()
save(tmp,file='svm.RData')


load('~/Downloads/svm.RData')

library(e1071)
xx=cbind(g,t(sapply(tmp,function(x){
    c(x$tot.accuracy, 100*confusion(music$Decade, predict(x))$tot.accuracy)
})))

png('accuracy-gamma.png')
par(mar=c(4,4,.2,.2))
plot(xx[,1:2],ylim=c(20,100),xlim=c(0,1),xlab='gamma',ylab='accuracy',pch=19)
legend('topleft',c('whole dataset','test set'),pch=c(19,21))
points(xx[,1],xx[,3],pch=21)
grid()
dev.off()



### the cost parameter
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

###########################################################



############ An attempt on the bagging of LDA + TREE #######
library(MASS)
library(rpart)
fit.lda = lda(Decade~., data=music)
pred = predict(fit.lda)
confusion(music$Decade, pred$class)
## plot(pred$x, col = as.integer(music$Decade))
fit.tree = rpart(music$Decade ~ pred$x)
confusion(music$Decade, predict(fit.tree, type = 'class'))

## a single LDA+TREE model is not enough; let's try bagging
n = nrow(music)
N = 50  # number of bootstrap samples
bag.tree = list()  # a `bag' of trees
freq = c(prop.table(table(music$Decade)))
for (i in 1:N) {
    message('building model ', i)
    idx = sample(n, replace = TRUE)
    ## make sure all 4 categories are there in the bootstrap sample
    while (length(unique(music[idx, 'Decade'])) < 4) {
        idx = sample(n, replace = TRUE)
    }
    ## build an LDA model and get projections, then grow a tree
    fit.lda = lda(Decade ~ ., data = music[idx, ], prior = freq)
    bag.tree[[i]] = rpart(music$Decade ~ cbind(music[, -91], predict(fit.lda, music)$x))
}
## now we have got N trees, so we predict on each tree and average by majority voting
predict_lda_tree = function(trees, newdata = list()) {
    bag.pred = sapply(trees, function(tree) {
        predict(tree, newdata, type = 'class')
    })
    bag.pred
}
tmp = predict_lda_tree(bag.tree, newdata = music)
bag.pred = apply(tmp, 1, function(r) names(which.max(table(r))))
confusion(music$Decade, bag.pred)


fit.lda = lda(Decade ~ ., data = music)
fit.svm = svm(music$Decade ~ predict(fit.lda)$x)
confusion(music$Decade, predict(fit.svm))
## confusion(music$Decade, predict(fit.lda)$class)
#########################################################

