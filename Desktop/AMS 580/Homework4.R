setwd("/Users/chaeeunshin/Desktop/AMS 580")
library(MASS)
library(tidyverse)
galton <- read.table("Galton.txt", header=T)
n <- nrow(galton)
midparent <- rep(0, times=nrow(galton))
for (i in 1:length(midparent)) {
  midparent[i] <- mean(c(galton$Father[i],galton$Mother[i]))
}
galton <- cbind(galton, midparent)
galton
#simple linear regression model for daughters
galton.daughter <- galton[which(galton$Gender=="F"),]
nd <- nrow(galton.daughter)
yd <- galton.daughter$Height
xd <- galton.daughter$midparent
dbeta1 <- sum((xd-mean(xd))*(yd-mean(yd)))/sum((xd-mean(xd))^2)
dbeta0 <- mean(yd)-dbeta1*mean(xd)
ydhat <- dbeta0+dbeta1*xd
plot(xd,yd)
lines(xd,ydhat)

##suggested solution
dmodel <- lm(galton.daughter$Height~galton.daughter$midparent)
summary(dmodel)
galton.daughter$pred <- dmodel %>% predict(galton.daughter)
head(galton.daughter)
sse.daughter <- sum((galton.daughter$Height-galton.daughter$pred)^2)
mse.daughter <- sse.daughter/(nrow(galton.daughter)-2)

#simple linear regression model for sons
galton.son <- galton[which(galton$Gender=="M"),]
ns <- nrow(galton.son)
xs <- galton.son$Height
ys <- galton.son$midparent
sbeta1 <- sum((xs-mean(xs))*(ys-mean(ys)))/sum((xs-mean(xs))^2)
sbeta0 <- mean(ys)-beta1*mean(xs)
yshat <- sbeta0+sbeta1*xs
plot(xs,ys)
lines(xs,yshat)

##suggested solution
smodel <- lm(galton.son$Height~galton.son$Height)
summary(smodel)
galton.son$pred <- smodel %>% predict(galton.son)
head(galton.son)
sse.son <- sum((galton.son$Height-galton.son$pred)^2)
mse.son <- sse.son/(nrow(galton.son)-2)


#general linear model
#predictor: mother's and father's height, child's gender
y <- galton$Height
x1 <- galton$Mother
x2 <- galton$Father
x3 <- galton$Gender
for(i in 1:length(x3)) {
  if (x3[i]=="M")
    x3[i] <- 1
  else
    x3[i] <- 0
}
x3 <- as.numeric(x3)
x <- rep(1, times=length(x1))
x <- cbind(x,x1,x2,x3)
beta <- solve(t(x)%*%x)%*%t(x)%*%y
beta
yhat <- beta[1,]+beta[2,]*x1+beta[3,]*x2+beta[4,]*x3

##suggested solution 
newgalton <- galton[,c("Father", "Mother","Gender","Height")]
newgalton$Gender <- ifelse(newgalton$Gender=="M",1,0)
head(newgalton)
newgalton$Gender <- factor(newgalton$Gender)
head(newgalton)
glmodel <- glm(Height~., data=newgalton)
summary(glmodel)
newgalton$pred <- glmodel %>% predict(newgalton)
newgalton

newgalton.daughters <- newgalton[which(newgalton$Gender==0),]
newgalton.sons <- newgalton[which(newgalton$Gender==1),]

sse.gl.daughters <- sum((newgalton.daughters$Height-newgalton.daughters$pred)^2)
sse.gl.sons <- sum((newgalton.sons$Height-newgalton.sons$pred)^2)

list(sse.daughter,sse.gl.daughters,sse.son,sse.gl.sons)


#As a result, generalized linear model is a better method to predict height of children. 