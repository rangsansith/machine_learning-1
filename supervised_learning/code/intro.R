####descriptive statistics
##main
setwd("/Users/codehi/Documents/2nd\ Emory-GT/CS\ 4641/hw1/data/")
white=read.csv("winequality-white.csv" , header = TRUE, sep = ";", quote = "\"",
               dec = ".", fill = TRUE, comment.char = "")
red=read.csv("winequality-red.csv" , header = TRUE, sep = ";", quote = "\"",
             dec = ".", fill = TRUE, comment.char = "")
combine=read.csv("winequality-combine.csv",header=TRUE,sep=",",quote="\"",dec=".",fill=TRUE)

crime=com
par(mfrow=c(4, 4))
colnames <- dimnames(crime.new)[[2]]
for (i in 2:8) {
  hist(crime[,i], xlim=c(0, 3500), breaks=seq(0, 3500, 100), main=colnames[i], probability=TRUE, col="gray", border="white")
  d <- density(crime[,i])
  lines(d, col="red")
}
##########################

par(mfrow=c(1,1),mar=c(1,1,1,1))
hist(combine$quality, probability=FALSE, col="gray", border="red")