setwd("/Users/codehi/Documents/2nd\ Emory-GT/CS\ 4641/hw1/data/")
white=read.csv("winequality-white.csv" , header = TRUE, sep = ";", quote = "\"",
               dec = ".", fill = TRUE, comment.char = "")
red=read.csv("winequality-red.csv" , header = TRUE, sep = ";", quote = "\"",
             dec = ".", fill = TRUE, comment.char = "")
combine=read.csv("winequality-combine.csv",header=TRUE,sep=",",quote="\"",dec=".",fill=TRUE)
quality=combine$quality
train=sample(1:nrow(combine), 5000)
combine.test=combine[-train,]
quality.test=quality[-train]
#combine$quality=factor(combine$quality)
#combine$quality=as.character(combine$quality)
############
library(gbm)

####overall tree

set.seed(4)
#boost.quality=gbm(quality~.,data=combine[train,],distribution="multinomial",n.trees=5000,interaction.depth=4)
boost.quality=gbm(quality~.,data=combine[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)

summary(boost.quality)
par(mfrow=c(1,2))


plot(boost.quality,i='alcohol')
plot(boost.quality,i="volatile.acidity")
yhat.boost=predict(boost.quality,newdata=combine[-train,],n.trees=5000)
mean((yhat.boost-quality.test)^2)  #0.4820682668


#test
test3=table(round(yhat.boost),quality.test)
test##red and white wine combined quality boosting decesion tree prediction
accuracy3=sum(test3[1,3]+test3[2,4],test3[3,5])/sum(test3)
accuracy3
mean((yhat.boost-quality.test)^2)






boost.quality=gbm(quality~.,data=combine[train,],distribution="multinomial",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)

yhat.boost=predict(boost.quality,newdata=combine[-train,],n.trees=5000)
mean((yhat.boost-quality.test)^2)

################ white  wine

set.seed(4)

train=sample(1:nrow(white), 4000)
white.test=white[-train,]
quality.test=white$quality[-train]


boost.quality=gbm(quality~.,data=white[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)

summary(boost.quality)
par(mfrow=c(1,2))


plot(boost.quality,i='alcohol')
plot(boost.quality,i="volatile.acidity")
yhat.boost=predict(boost.quality,newdata=white[-train,],n.trees=5000)
mean((yhat.boost-quality.test)^2)  #0.4820682668


#test
test3=table(round(yhat.boost),quality.test)
test## white wine combined quality boosting decesion tree prediction
accuracy3=sum(test3[1,3]+test3[2,4],test3[3,5])/sum(test3)
accuracy3
mean((yhat.boost-quality.test)^2)



################ red  wine

set.seed(4)

train=sample(1:nrow(red), 1000)
red.test=red[-train,]
quality.test=red$quality[-train]


boost.quality=gbm(quality~.,data=red[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)

summary(boost.quality)
par(mfrow=c(1,2))


plot(boost.quality,i='alcohol')
plot(boost.quality,i="volatile.acidity")
yhat.boost=predict(boost.quality,newdata=red[-train,],n.trees=5000)
mean((yhat.boost-quality.test)^2)  #0.4820682668


#test
test3=table(round(yhat.boost),quality.test)
test## red wine combined quality boosting decesion tree prediction
accuracy3=sum(test3[1,3]+test3[2,4],test3[3,5])/sum(test3)
accuracy3
mean((yhat.boost-quality.test)^2)






# 
# #adabag
# library("adabag")
# data("iris")
# 
# quality=combine$quality
# train=sample(1:nrow(combine), 5000)
# combine.test=combine[-train,]
# quality.test=quality[-train]
# combine$quality=as.character(combine$quality)
# 
# #train <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
# combine.adaboost <- boosting(quality ~ ., data = combine[train, ], mfinal = 10,
#                              control = rpart.control(maxdepth = 1))
# combine.adaboost

