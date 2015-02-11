library(tree)

setwd("/Users/codehi/Documents/2nd\ Emory-GT/CS\ 4641/hw1/data/")
white=read.csv("winequality-white.csv" , header = TRUE, sep = ";", quote = "\"",
               dec = ".", fill = TRUE, comment.char = "")
red=read.csv("winequality-red.csv" , header = TRUE, sep = ";", quote = "\"",
               dec = ".", fill = TRUE, comment.char = "")

white$quality=factor(white$quality)

#decision tree

# Classification Tree with rpart

library(rpart)
#X1<-X[complete.cases(X),] 
# grow tree 

fit <- rpart(quality ~ .,
             method="class", data=white)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits


plot(fit, uniform=TRUE, 
     main="Classification Tree for white")
text(fit, use.n=TRUE, all=TRUE, cex=0.7)

par(mar=c(0.2,.2,.2,.2),oma=c(0.2,.2,.2,.2),cex.lab = .6)
plot(fit, uniform = FALSE, branch = 1, compress = TRUE,
     margin = .2, minbranch = 0.3)
text(fit, use.n=TRUE, all=TRUE, cex=0.7)


#########Tree with classes

# Classification Tree with rpart
# grow tree 
fit1 <- rpart(quality ~ .,
             method="class", data=white)

printcp(fit1) # display the results 
plotcp(fit1) # visualize cross-validation results 
summary(fit1) # detailed summary of splits

# plot tree 
plot(fit1, uniform=TRUE, 
     main="Classification Tree for wb")
text(fit1, use.n=TRUE, all=TRUE, cex=0.7)





###ISLR R package



tree.white=tree(quality~.,white)
summary(tree.white)
plot(tree.white)
text(tree.white,pretty=0)
tree.white


##supervised learning
white$quality=factor(white$quality)
quality=white$quality
train=sample(1:nrow(white), 4000)
white.test=white[-train,]
quality.test=quality[-train]
tree.white=tree(quality~.,white,subset=train,method="class")


tree.pred=predict(tree.white,white.test,type="class")
test=table(tree.pred,quality.test)
test
accuracy=sum(test[1,1]+test[2,2],test[3,3]+test[4,4],test[5,5],test[6,6],test[7,7])/sum(test)
accuracy


###prune the tree


set.seed(3)
cv.white=cv.tree(tree.white,FUN=prune.misclass)
names(cv.white)
cv.white
par(mfrow=c(1,2))
plot(cv.white$size,cv.white$dev,type="b")
plot(cv.white$k,cv.white$dev,type="b")
prune.white=prune.misclass(tree.white,best=3)
plot(prune.white)
text(prune.white,pretty=0)
tree.pred=predict(prune.white,white.test,type="class")
test2=table(tree.pred,quality.test)
test2
accuracy2=sum(test2[1,1]+test2[2,2],test2[3,3]+test2[4,4],test2[5,5],test2[6,6],test2[7,7])/sum(test2)
accuracy2




#####red




#####combined








