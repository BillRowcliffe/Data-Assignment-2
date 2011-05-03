library(MASS)
library(randomForest)
library(reshape)
#Record Time
#system.time({
#Create datasets
##LDA Train
music<-read.csv("music.csv")
indx.30<-sample(c(1:13291)[music[,91]=="1930s"],60)
indx.50<-sample(c(1:13291)[music[,91]=="1950s"],726)
indx.70<-sample(c(1:13291)[music[,91]=="1970s"],1250)
indx.90<-sample(c(1:13291)[music[,91]=="1990s"],1250)
sort(indx.30)
sort(indx.50)
sort(indx.70)
sort(indx.90)
music.t<-music[-sort(c(indx.30,indx.50,indx.70,indx.90)),]
music.t.scale<-rescaler(music.t)
music.t.rd<-cbind(music.t[,1:12],music.t[91])
##Random Forest Train
music.scale<-rescaler(music)
music.rd<-cbind(music[,1:12],music[91])
#Build LDA models
music.30.lda<-lda(Decade=="1930s"~.,music.t,prior=c(.5,.5))
music.50.lda<-lda(Decade=="1950s"~.,music.t,prior=c(.5,.5))
music.70.lda<-lda(Decade=="1970s"~.,music.t,prior=c(.5,.5))
music.90.lda<-lda(Decade=="1990s"~.,music.t,prior=c(.5,.5))
music.split1.lda<-lda(Decade=="1990s"|Decade=="1970s"~.,music.t,prior=c(.5,.5))
music.split2.lda<-lda(Decade=="1990s"|Decade=="1930s"~.,music.t,prior=c(.5,.5))
music.scale.30.lda<-lda(Decade=="1930s"~.,music.t.scale,prior=c(.5,.5))
music.scale.50.lda<-lda(Decade=="1950s"~.,music.t.scale,prior=c(.5,.5))
music.scale.70.lda<-lda(Decade=="1970s"~.,music.t.scale,prior=c(.5,.5))
music.scale.90.lda<-lda(Decade=="1990s"~.,music.t.scale,prior=c(.5,.5))
music.scale.split1.lda<-lda(Decade=="1990s"|Decade=="1970s"~.,music.t.scale,prior=c(.5,.5))
music.scale.split2.lda<-lda(Decade=="1990s"|Decade=="1930s"~.,music.t.scale,prior=c(.5,.5))
music.rd.30.lda<-lda(Decade=="1930s"~.,music.t.rd,prior=c(.5,.5))
music.rd.50.lda<-lda(Decade=="1950s"~.,music.t.rd,prior=c(.5,.5))
music.rd.70.lda<-lda(Decade=="1970s"~.,music.t.rd,prior=c(.5,.5))
music.rd.90.lda<-lda(Decade=="1990s"~.,music.t.rd,prior=c(.5,.5))
music.rd.split1.lda<-lda(Decade=="1990s"|Decade=="1970s"~.,music.t.rd,prior=c(.5,.5))
music.rd.split2.lda<-lda(Decade=="1990s"|Decade=="1930s"~.,music.t.rd,prior=c(.5,.5))
#Combine LDA models
music.projections<-cbind(predict(music.split1.lda,music),predict(music.split2.lda,music),music[91])
music.projections<-cbind(predict(music.30.lda,music),predict(music.50.lda,music),music.projections)
music.projections<-cbind(predict(music.70.lda,music),predict(music.90.lda,music),music.projections)
music.projections<-cbind(predict(music.scale.30.lda,music.scale),predict(music.scale.50.lda,music.scale),music.projections)
music.projections<-cbind(predict(music.scale.70.lda,music.scale),predict(music.scale.90.lda,music.scale),music.projections)
music.projections<-cbind(predict(music.scale.split1.lda,music.scale),predict(music.scale.split2.lda,music.scale),music.projections)
music.projections<-cbind(predict(music.rd.split1.lda,music),predict(music.rd.split2.lda,music),music.projections)
music.projections<-cbind(predict(music.rd.30.lda,music),predict(music.rd.50.lda,music),music.projections)
music.projections<-cbind(predict(music.rd.70.lda,music),predict(music.rd.90.lda,music),music.projections)
#Select scores, timbreav, and class
music.data<-cbind(a=music.projections[,4], b=music.projections[,8], c=music.projections[,12], d=music.projections[,16], e=music.projections[,20], f=music.projections[,24], g=music.projections[,28], h=music.projections[,32], i=music.projections[,36], j=music.projections[,40], k=music.projections[,44], l=music.projections[,48], m=music.projections[,52], n=music.projections[,56], o=music.projections[,60], p=music.projections[,64], q=music.projections[,68], r=music.projections[,72], music[,1:12], music[91])
music.data<-music.data[-1,]
#randomForest
write.table(music.data, file="music.data.csv")
music.LDA.RF<-randomForest(Decade~., data=music.data, maxnodes=6, ntree=1000)
#End time record
#})