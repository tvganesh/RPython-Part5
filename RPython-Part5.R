source('RFunctions-1.R')
library(dplyr)

library(ISLR)
fit=lm(wage~poly(age,4),data=Wage)

df=read.csv("auto_mpg.csv",stringsAsFactors = FALSE) # Data from UCI
df1 <- as.data.frame(sapply(df,as.numeric))
df2 <- df1 %>% dplyr::select(cylinder,displacement, horsepower,weight, acceleration, year,mpg)
auto <- df2[complete.cases(df2),]
ggplot(auto,aes(x=horsepower,y=mpg)) + geom_point() + xlab("Horsepower") + 
    ylab("Miles Per gallon") + ggtitle("Miles per Gallon vs Hosrsepower")

#################################
train_idx <- trainTestSplit(auto,trainPercent=75,seed=5)
train <- auto[train_idx, ]
test <- auto[-train_idx, ]

################# Rsquared is incorrect ###################################
fit=lm(mpg~poly(horsepower,4),data=train)
a=poly(test$horsepower,4)
yhat=predict(lmfit,horsepower=data.frame(a))
RSS <- sum((y - yhat)^2)
TSS <- sum((y - mean(y))^2)
1 - (RSS/TSS)
#############################

# Fit a 4th degree polynomial
fit=lm(mpg~poly(horsepower,4),data=auto)
#Display a summary of fit

#Get the range of horsepower
hp <- range(auto$horsepower)

#Create a sequence to be used for plotting
hpGrid <- seq(hp[1],hp[2],by=10)

#Predict for these values of horsepower. Set Standard error as TRUE
pred=predict(fit,newdata=list(horsepower=hpGrid),se=TRUE)

#Compute bands on either side that is 2xSE
seBands=cbind(pred$fit+2*pred$se.fit,pred$fit-2*pred$se.fit)

#Plot the fit with Standard Error bands
plot(auto$horsepower,auto$mpg,xlim=hp,cex=.5,col="black")
title("Degree-4 Polynomial",outer=T)
lines(hpGrid,pred$fit,lwd=2,col="blue")
matlines(hpGrid,seBands,lwd=2,col="blue",lty=3)

#Splines
library(splines)
fit=lm(mpg~bs(horsepower,df=6,knots=c(60,75,100,150)),data=auto)
pred=predict(fit,newdata=list(horsepower=hpGrid),se=T)
plot(auto$horsepower,auto$mpg,xlim=hp,cex=.5,col="black")
lines(hpGrid,pred$fit,lwd=2)
lines(hpGrid,pred$fit+2*pred$se,lty="dashed")
lines(hpGrid,pred$fit-2*pred$se,lty="dashed")
abline(v=c(60,75,100,150),lty=2,col="darkgreen")

#Natural spline
# There is no need to select the knots here. There is a smoothing parameter which
# can be specified by the degrees of freedom 'df' parameter. The natural spline

fit2=lm(mpg~ns(horsepower,df=4),data=auto)
pred=predict(fit2,newdata=list(horsepower=hpGrid),se=T)
plot(auto$horsepower,auto$mpg,xlim=hp,cex=.5,col="black")
lines(hpGrid,pred$fit,lwd=2)
lines(hpGrid,pred$fit+2*pred$se,lty="dashed")
lines(hpGrid,pred$fit-2*pred$se,lty="dashed")

# Smoothing spline has a smoothing parameter, the degrees of freedom
# This is too wiggly
fit=smooth.spline(auto$horsepower,auto$mpg,df=16)
lines(fit,col="red",lwd=2)

# We can use Cross Validation to allow the spline to pick the value of this smpopothing paramter
fit=smooth.spline(auto$horsepower,auto$mpg,cv=TRUE)
lines(fit,col="blue",lwd=2)

# Degrees of freedom is 5.78

#Natural spline
library(gam)
gam=gam(mpg~s(horsepower,4)+s(cylinder,5)+s(displacement,4)+s(year,4)+s(acceleration,5),data=auto)
summary(gam)
par(mfrow=c(2,3))
plot(gam,se=TRUE)



library(MASS)
library(tree)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
############################################

cancer <- read.csv("cancer.csv",stringsAsFactors = FALSE)
cancer <- cancer[,2:32]
cancer$target <- as.factor(cancer$target)
train_idx <- trainTestSplit(cancer,trainPercent=75,seed=5)
train <- cancer[train_idx, ]
test <- cancer[-train_idx, ]

# Create Decision Tree
cancerStatus=tree(target~.,train)
summary(cancerStatus)

# Plot decision tree with labels
plot(cancerStatus)
text(cancerStatus,pretty=0)

# Execute 10 fold cross validation
cvCancer=cv.tree(cancerStatus)
plot(cvCancer)
# Plot the 
plot(cvCancer$size,cvCancer$dev,type='b')
prunedCancer=prune.tree(cancerStatus,best=4)
plot(prunedCancer)
text(prunedCancer,pretty=0)

pred <- predict(prunedCancer,newdata=test,type="class")
confusionMatrix(pred,test$target)


################################################
df=read.csv("Boston.csv",stringsAsFactors = FALSE) # Data from MASS - SL

# Select specific columns
Boston <- df %>% dplyr::select("crimeRate","zone","indus","charles","nox","rooms","age",
                            "distances","highways","tax","teacherRatio","color","status","medianValue")
library(randomForest)


train_idx <- trainTestSplit(Boston,trainPercent=75,seed=5)
train <- Boston[train_idx, ]
test <- Boston[-train_idx, ]

bagBoston=randomForest(medianValue~.,data=medianValue,mtry=13,importance=TRUE)
bagBoston
yhatBag = predict(bagBoston,newdata=test)
plot(yhatBag, test$medianValue)
abline(0,1)
mean((yhatBag-test$medianValue)^2)

# Fit a Random Forest on the Boston training data
rfBoston=randomForest(medianValue~.,data=train)
# Display the summatu of the fit. It can be seen that the MSE is 10.88 and the percentage variane
# explained is 86.14%. About 4 variables were tried at each split for a maximum tree of 500.
# The MSE and percent variance is on Out of Bag trees
rfBoston
importance(rfBoston)
varImpPlot(rfBoston)

oobError <- NULL
testError <- NULL
# In the code below the number of variables to consider at each split is increased
# from 1 - 13 and the OOB error and the MSE is computed
for(i in 1:13){
    fitRF=randomForest(medianValue~.,data=train,mtry=i,ntree=400)
    oobError[i] <-fitRF$mse[400]
    pred <- predict(fitRF,newdata=test)
    testError[i] <- mean((pred-test$medianValue)^2)
}

# We can see the OOB and Test Error. It can be seen that the Random Forest performs
# best with the lowers MSE at mtry=6
matplot(1:13,cbind(testError,oobError),pch=19,col=c("red","blue"),
        type="b",xlab="mtry(no of varaibles at each split)", ylab="Mean Squared Error")


# Boosting
library(gbm)
# Perform gradient boosting on the Boston data set. The distribution is gaussian since we
# doing MSE. The interaction depth specifies the number of splits
boostBoston=gbm(medianValue~.,data=train,distribution="gaussian",n.trees=5000,
                shrinkage=0.01,interaction.depth=4)
#The summary gives the variable importance. The 2 most significant variables are
# number of rooms and lower status
summary(boostBoston)

# The plots below show how each variable relates to the median value of the home. As
# the number of roomd increase the median value increases and with increase in lower status
# the median value decreases
par(mfrow=c(1,2))
plot(boostBoston,i="rooms")
plot(boostBoston,i="status")


pred <- predict(boostBoston,newdata=test,n.trees=seq(100,5000,by=50))
testError <- mean((pred-test$medianValue)^2)
testError


cvBoost=gbm(medianValue~.,data=train,distribution="gaussian",n.trees=5000,
                shrinkage=0.01,interaction.depth=4,cv.folds=5)

cvError <- NULL
s <- c(.001,0.09,0.07,0.05,0.03,0.01,0.1)
for(i in seq_along(s)){
    cvBoost=gbm(medianValue~.,data=train,distribution="gaussian",n.trees=5000,
                shrinkage=s[i],interaction.depth=4,cv.folds=5)
    cvError[i] <- mean(cvBoost$cv.error)
}

# Create a data frame for plotting
a <- rbind(s,cvError)
b <- as.data.frame(t(a))
# It can be seen that a shrinkage parameter of 0,05 gives the lowes CV Error
ggplot(b,aes(s,cvError)) + geom_point() + geom_line(color="blue") + 
    xlab("Shrinkage") + ylab("Cross Validation Error") +
    ggtitle("Gradient boosted trees - Cross Validation error vs Shrinkage")
    

plot(s,cvError,pch=19,type="b",xlab="Shrinkage",ylab="Cross Validation Error",
     main="Gradient boosted trees - Cross Validation error vs Shrinkage",xlim=c(-1,1))


for(i in seq_along(s)){
    print(i)
    print(s[i])
}
