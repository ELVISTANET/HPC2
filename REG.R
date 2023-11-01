n<-100
p<-10
rmse_results<-list()
beta_results<-list()

for (i in 1:1000){
  X<- matrix(rnorm(n*p), nrow=n, ncol=p)
  beta<- rnorm(10)
  beta
  error<- rnorm(n)
  Y<- X%*%beta + error
  
  reg_model<- lm(Y~X)
  
  betahat<- coef(reg_model)
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


reg_results
rmse_results
beta_results


