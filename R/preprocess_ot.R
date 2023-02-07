#' @export
preprocess_ot<-function(dat,protect){
  modifiedDAT<-dat #the new dataframe that will be returned
  tomodify<-setdiff(1:ncol(dat),protect) #the columns in dat to modify
  z=dat[,protect] #the protected attribute
  n_quan=ceiling(nrow(dat)/150) #number of quantiles to use (20 points per regression)
  quans<-(seq(from=0,to=1,length.out = n_quan)) #quantiles of interest
  quans<-quantile(z,quans) #quantiles of z
  #loop through each feature we need to modify
  for(j in tomodify){
    x=dat[,j] #feature we will modifty
    newx<-x
    orderedCONDF<-sort(x) #sorted x
    for(quan in 2:n_quan){
      cur_obs<- (z<=quans[quan] & z>=quans[quan-1])
      x_curquan=x[cur_obs]
      z_curquan=z[cur_obs]
      if(sd(x_curquan)<1e-6) x_curquan<-x_curquan+rnorm(length(x_curquan),sd=sd(x)/length(x))
      if(sd(z_curquan)<1e-6) z_curquan<-z_curquan+rnorm(length(z_curquan),sd=sd(z)/length(z))
      mod<-lm(x_curquan~z_curquan)
      rv<-as.numeric(mod$residuals)
      condF<-rank(rv)/length(rv)
      newx[cur_obs]<-condF
    }
    modifiedDAT[,j]<-newx
  }
  modifiedDAT
}