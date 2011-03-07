setwd("//tsclient/D/Assignment 2 for STAT503")
setwd("D:/Assignment 2 for STAT503")

# Data reading routines
music<-read.csv("music-UCI.csv")

# Standardize the variables
library(reshape)
music.std<-rescaler(music)

# Break into training test for all four types,total(1930s:243,1950s:3048,1970s:5000,1990s:5000)
indx.30<-sample(c(1:13291)[music.std[,91]=="1930s"],70)
indx.50<-sample(c(1:13291)[music.std[,91]=="1950s"],1000)
indx.70<-sample(c(1:13291)[music.std[,91]=="1970s"],1500)
indx.90<-sample(c(1:13291)[music.std[,91]=="1990s"],1500)
sort(indx.30)
sort(indx.50)
sort(indx.70)
sort(indx.90)
#test dataset
music.test<-music.std[sort(c(indx.30,indx.50,indx.70,indx.90)),]
#test_indices
test_indices<-sort(c(indx.30,indx.50,indx.70,indx.90))
test_indices
#trainging dataset
music.train<-music.std[-sort(c(indx.30,indx.50,indx.70,indx.90)),]
#training_indices
n<-length(music.std[,1])
training_indices<-(1:n)[-test_indices]
training_indices


#ANN
library(nnet)
music.nn<-nnet(music.std[,1:90],music.std[,91],size=0,skip=T,linout=T,decay=5e-4,range=0.6,maxit=1000)
summary(muisc.nn)
table(music[,91],round(predict(music.nn,music[,1:90])))

#random forest
library(randomForest)
memory.size(max = FALSE)
music.rf<-randomForest(factor(Decade)~.,data=music.std,importance=TRUE,proximity=TRUE,mtry=3)
print(music.rf)
head(music.rf$importance)
head(music.rf$votes)

#SVM
library(e1071)
music.svm<-svm(factor(Decade)~.,music.std,kernel="linear")   #kernel="polynomial", default kernel="radial"
table(music.std[,91],predict(music.svm,music.std))
#music.svm<-svm(factor(Decade)~.,music.train,kernel="linear")   #kernel="polynomial", default kernel="radial"
#table(music.train[,91],predict(music.svm,music.train))
#table(music.test[,91],predict(music.svm,music.test))
music.svm$SV
music.SVM$index
#to examine the boundary
library(classifly)
classifly(data=music.train,model=factor(Decade)~.,classifier=svm,probability= TRUE,kernel="linear",n=5000,method="random")


