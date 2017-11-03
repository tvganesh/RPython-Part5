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
