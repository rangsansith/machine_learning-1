setwd("/Users/codehi/Documents/2nd\ Emory-GT/CS\ 4641/hw1/data/")
combine=read.csv("winequality-combine.csv",header=TRUE,sep=",",quote="\"",dec=".",fill=TRUE)

#combine$quality=factor(combine$quality)
quality=combine$quality

##ANN
train=sample(1:nrow(combine), 5000)
quality.train=combine[train,]
combine.test=combine[-train,]
quality.test=quality[-train]

# install, load package
install.packages(NeuralNetTools)
library(NeuralNetTools)

# create model
library(neuralnet)

summary(quality.train)

mod <- neuralnet(quality ~type + fixed.acidity	+volatile.acidity+	citric.acid+	residual.sugar+	chlorides	+free.sulfur.dioxide	+total.sulfur.dioxide+	density+	pH	+sulphates	+alcohol , data=quality.train,
                 hidden = c(6, 12, 8), rep = 20, err.fct = 'ce', linear.output = FALSE)

mod <- neuralnet(quality ~type + fixed.acidity  +volatile.acidity+	citric.acid+	residual.sugar+	chlorides	+free.sulfur.dioxide	+total.sulfur.dioxide+	density+	pH	+sulphates	+alcohol , data=quality.train,
                 hidden=10, threshold=0.01, linear.output = FALSE)

# plotnet
par(mar = numeric(4), family = 'serif')
plotnet(mod, alpha = 0.6)

###predict
DataPred = prediction(mod, combine.test)



main <- glm(quality ~type + fixed.acidity  +volatile.acidity+  citric.acid+	residual.sugar+	chlorides	+free.sulfur.dioxide	+total.sulfur.dioxide+	density+	pH	+sulphates	+alcohol , data=quality.train,
            , sum.data, family=poisson())
full <- glm(SUM~Var1*Var2*Var3, sum.data, family=poisson())
pred=prediction(mod, list.glm=list(main=main, full=full))

DataPred = prediction(mod, subset(combine.test,select=-quality))



ss=subset(combine.test,select=-quality)

















# create model
library(RSNNS)
#data(neuraldat)
x <- combine[, c('type', 'fixed.acidity ','citric.acid','residual.sugar','chlorides', 'free.sulfur.dioxide','total.sulfur.dioxide','density','pH','sulphates','alcohol')]
y <- combine[, 'quality']
mod <- mlp(x, y, size = 5)

# garson
garson(mod, 'quality')


# create model
library(nnet)
data(neuraldat)
mod <- nnet(quality ~ type + fixed.acidity  +volatile.acidity+	citric.acid+	residual.sugar+	chlorides	+free.sulfur.dioxide	+total.sulfur.dioxide+	density+	pH	+sulphates	+alcohol , data = quality.train, size = 40)

# lekprofile
lekprofile(mod)