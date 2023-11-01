install.packages("glmnet")
library(glmnet)
set.seed(100)

n<-100
p<-10
lasso_results<-list()
rmse_results<-list()
beta_results<-list()

for (i in 1:1000){
  X<- matrix(rnorm(n*p), nrow=n, ncol=p)
  beta<- rnorm(10)
  beta
  error<- rnorm(n)
  Y<- X%*%beta + error
  
  lasso_model<- cv.glmnet(X,Y, alpha=1)
  lambda_best<- lasso_model$lambda.min
  lasso_model_best<- glmnet(X,Y,alpha=1, lambda=lambda_best)
  
  lasso_results[[i]]<- lasso_model_best
  
  betahat<- coef(lasso_model_best)
  betahat<- as.matrix(betahat)
  
  beta_results[[i]]<- betahat
  
  b0<-betahat[1,1]
  yhat<- b0+X%*%betahat[2:11,]
  deviation<- Y-yhat
  deviation
  deviationsq<- deviation[,1]^2
  deviationsq
  
  rmse<- sqrt((1/n)*sum((deviationsq)))
  
  rmse_results[[i]]<- rmse
}

lasso_results
rmse_results
beta_results
