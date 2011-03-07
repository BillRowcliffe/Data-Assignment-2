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

# LDA
library(MASS)
##build the lda model using all 90 variables
music.lda<-lda(music.train[,1:90],music.train[,91])
music.lda
##the training error from classification table
table(music.train[,91],
  predict(music.lda,music.train[,1:90],dimen=3)$class)
##the test error from classification table
table(music.test[,91],                               
  predict(music.lda,music.test[,1:90],dimen=3)$class)

# Variable importance
for (i in c(1:90))
  cat(cor(music.train[,i],
   predict(music.lda,music.train[,1:90],dimen=3)$x),"\n")


var(music.train[,1:90])  
    

# All variables are important, correlations are reasonably high for all.
# Coefficients suggest LAve is most important, but the training data has 
# lsightly higher variance for this variable, which leads to the higher coefficient.

# Calculate constant
apply(music.lda$means,2,mean)%*%music.lda$scaling



# QDA
##build the qda model using all 5 variables
music.qda<-qda(music.train[,1:90],music.train[,91])
##the training error from classification table
table(music.train[,91],
  predict(music.qda,music.train[,1:90])$class)
##the test error from classification table
table(music.test[,91],                               
  predict(music.qda,music.test[,1:90],dimen=1)$class)


# Logistic regression
library(nnet)
##build the logistic regression model using all 5 variables
music.log.reg<-multinom(Decade~.,
  data=music.train)
##model information
music.log.reg
##the training error from classification table
table(music.train[,91],predict(music.log.reg,music.train,type="class"))
##the test error from classification table
table(music.test[,91],predict(music.log.reg,music.test,type="class"))

 # tree
library(rpart)
##build the tree model using all 5 variables
music.rp<-rpart(music.train[,91]~.,music.train[,1:90],method='class',parms=list(split='information'))
##model information
music.rp
##the training error from classification table
table(music.train[,91],predict(music.rp,music.train[,1:90],type="class"))
##the test error from classification table
table(music.test[,91],predict(music.rp,music.test[,1:90],type="class"))

