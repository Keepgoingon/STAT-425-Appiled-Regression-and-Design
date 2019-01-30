#STAT 425 HW2

#Question 1
set.seed(139)
data_x<-c(rep(1,10),rep(0,5),rep(1,5),2,-1,3,-2,2,1,0,0,-1,0,rep(-2,3),3,1,3,0,-1,0,1)
X<- matrix(data = data_x,ncol = 4,nrow = 10,byrow = F)
data_beta<-c(0,1,-1,2)
beta<-matrix(data = data_beta,ncol=1, nrow = 4)
epsilon<-matrix(data = rnorm(10,mean = 0,sd=1),ncol=1,nrow = 10)
Y<- X%*%beta + epsilon
#(a)
beta_estimated<-solve(t(X)%*%X)%*%t(X)%*%Y
beta_estimated

#(b)
True_var<-solve(t(X)%*%X)
print(True_var)
trace_var<-diag(True_var)
Var_beta123<-trace_var[2]+trace_var[3]+trace_var[4]
Var_beta123
#(c)
Y_estimated<-X%*%beta_estimated
residuals<- Y - Y_estimated
sum((residuals)^2)/(10-4)

#(d)
set.seed(139)
beta_estimated1<-matrix(0,4,500)
for(i in 1:500){
  Y1 <- Y + rnorm(10)
  beta_estimated1[,i] <- solve(t(X)%*%X) %*% (t(X)%*%Y1)
}
par(mfrow=c(1,2))
hist(beta_estimated1[1,], xlab=expression(beta[0]))
hist(beta_estimated1[2,]+beta_estimated1[3,]+beta_estimated1[4,], xlab=expression(beta[1]+beta[2]+beta[3]))

#variance
sample_var<-apply(beta_estimated1, 1, var)
sample_var1<-sample_var[1]
sample_var1
sample_var234<-sample_var[2]+sample_var[3]+sample_var[4]
sample_var234
#compared to (b)
True_var_b<-diag(True_var)
True_var_b1<-True_var_b[1]
True_var_b1
True_var_b234<-True_var_b[2]+True_var_b[3]+True_var_b[4]
True_var_b234

#(e)
set.seed(139)
beta_estimated2<-matrix(0,4,500)
for(i in 1:500){
  Y1 <- Y + rexp(10)-1
  beta_estimated2[,i] <- solve(t(X)%*%X) %*% (t(X)%*%Y1)
}
par(mfrow=c(1,2))
hist(beta_estimated2[1,], xlab=expression(beta[0]))
hist(beta_estimated2[2,]+beta_estimated2[3,]+beta_estimated2[4,], xlab=expression(beta[1]+beta[2]+beta[3]))
apply(beta_estimated2, 1, var)
sample_var
True_var_b

#Question 3 (a)
library(faraway)
unique(salmonella$dose)
length(unique(salmonella$dose))
salmonella$dose

#(b)
lm<-lm(colonies~dose,data = salmonella)
summary(lm)

#(c)
x = c(20, 40, 60, 80, 120, 150, 180, 300, 500)
salmonella_new<-cbind(salmonella,x)
lm_new<-lm(colonies~ x,data = salmonella_new)
summary(lm_new)
newx = seq(min(x),max(x),by = 0.05)
conf_interval <- predict(lm_new, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95)
plot(y = salmonella_new$colonies,x = salmonella_new$x,xlab = "Dose level",
     ylab = "Predicted Colonies",main="Regression")
abline(lm_new, col="red")
lines(newx, conf_interval[,2], col="blue", lty=2)
lines(newx, conf_interval[,3], col="blue", lty=2)

#Question 4 (a)
lm_p4<-lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45,data = prostate )
x <- data.frame(lcavol = 1.5, lweight = 3.6, age = 60,lbph=0.3,
                svi = 0, lcp = -0.8, gleason = 7, pgg45 = 15)
predict(lm_p4, x, interval="prediction")

#(b)
x1 <- data.frame(lcavol = 1.5, lweight = 3.6, age = 20,lbph=0.3,
                svi = 0, lcp = -0.8, gleason = 7, pgg45 = 15)
predict(lm_p4, x1, interval="prediction")

#(c)
summary(lm_p4)
lm_p4c<-lm(lpsa~lcavol+lweight+svi,data = prostate)
summary(lm_p4c)
predict(lm_p4c, x, interval="prediction")
predict(lm_p4, x, interval="prediction")
predict(lm_p4c, x1, interval="prediction")
predict(lm_p4, x1, interval="prediction")
anova(lm_p4c,lm_p4)
