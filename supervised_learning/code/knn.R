red#knn

#data input
setwd("/Users/codehi/Documents/2nd\ Emory-GT/CS\ 4641/hw1/data/")
white=read.csv("winequality-white.csv" , header = TRUE, sep = ";", quote = "\"",
               dec = ".", fill = TRUE, comment.char = "")
red=read.csv("winequality-red.csv" , header = TRUE, sep = ";", quote = "\"",
             dec = ".", fill = TRUE, comment.char = "")
combine=read.csv("winequality-combine.csv",header=TRUE,sep=",",quote="\"",dec=".",fill=TRUE)




normalize <- function(x) {
  y <- (x - min(x))/(max(x) - min(x))
  y
}

#normalized

combineN=lapply(combine[, 2:12], normalize)
combineN=data.frame(combine[,1],combineN)
quality=combine$quality


train=sample(1:nrow(combine), 5000)
combine.train=combineN[train,]
combine.test=combineN[-train,]
quality.test=quality[-train]
quality.train=quality[train]

library(class)
sqrt(5000)


###knn model

knn_predict <- knn(combine.train, combine.test, quality.train, k = 70)
knn_predict

test=table(knn_predict, quality.test)
test#red and white wine combined quality KNN(k=70) prediction
accuracy3=sum(test[1,1]+test[2,2],test[3,3]+test[4,4],test[5,5],test[6,6])/sum(test)
accuracy3
mean((as.numeric(knn_predict)-quality.test)^2)

acc<- array(0,dim=c(500,0));
for(i in 1:500){
  knn_predict <- knn(combine.train, combine.test, quality.train, k = i);
  
  test=table(knn_predict, quality.test);
  #red and white wine combined quality KNN(k=70) prediction
  acc[i]=sum(test[1,1]+test[2,2],test[3,3]+test[4,4],test[5,5],test[6,6])/sum(test);
}

k=1:499;
plot(k,acc,type='l',col='blue')
title("Prediction accuracy of KNN with regards of cluster parameter k")





##white##########################



whiteN=lapply(white[, 2:12], normalize)
whiteN=data.frame(white[,1],whiteN)
quality=white$quality


train=sample(1:nrow(white), 4000)
white.train=whiteN[train,]
white.test=whiteN[-train,]
quality.test=quality[-train]
quality.train=quality[train]

library(class)
sqrt(5000)


###knn model



acc<- array(0,dim=c(500,0));
for(i in 1:500){
  knn_predict <- knn(white.train, white.test, quality.train, k = i);
  
  test=table(knn_predict, quality.test);
  #red and white wine whited quality KNN(k=70) prediction
  acc[i]=sum(test[1,1]+test[2,2],test[3,3]+test[4,4],test[5,5],test[6,6])/sum(test);
}

k=1:499;
plot(k,acc,type='l',col='purple')
title("Prediction accuracy of KNN on white wine with regards of cluster parameter k")







##red##########################



redN=lapply(red[, 2:12], normalize)
redN=data.frame(red[,1],redN)
quality=red$quality


train=sample(1:nrow(red), 1000)
red.train=redN[train,]
red.test=redN[-train,]
quality.test=quality[-train]
quality.train=quality[train]

library(class)
sqrt(5000)


###knn model



acc<- array(0,dim=c(500,0));
for(i in 1:500){
  knn_predict <- knn(red.train, red.test, quality.train, k = i);
  
  test=table(knn_predict, quality.test);
  #red and red wine redd quality KNN(k=70) prediction
  acc[i]=sum(test[1,1]+test[2,2],test[3,3]+test[4,4],test[5,5],test[6,6])/sum(test);
}

k=1:499;
plot(k,acc,type='l',col='red')
title("Prediction accuracy of KNN on red wine with regards of cluster parameter k")

