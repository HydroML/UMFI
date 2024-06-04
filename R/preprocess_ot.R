#' Remove dependencies via optimal transport
#'
#' This function removes the dependencies between the protected attribute and the rest of the features via optimal transport.
#'
#' @param dat A dataframe or matrix of data
#' @param protect The column number of the protected attribute
#' @param n_quan The number of quantiles to use to estimate the conditional CDF (default is ceiling(nrow(dat)/150))
#' @param min_sd The minimum standard deviation that data points within a quantile can have before noise is added (default is 1e-6)
#' @return A dataframe or matrix of data with the dependencies between the protected attribute and the rest of the features removed
#' @export
preprocess_ot<-function(dat,protect,n_quan=ceiling(nrow(dat)/150),min_sd=1e-6){
  modifiedDAT<-dat #the new dataframe that will be returned
  tomodify<-setdiff(1:ncol(dat),protect) #the columns in dat to modify
  z=dat[,protect] #the protected attribute
  quans<-(seq(from=0,to=1,length.out = n_quan)) #quantiles of interest
  quans<-quantile(z,quans) #quantiles of z
  #loop through each feature we need to modify
  for(j in tomodify){
    x=dat[,j] #feature we will modifty
    newx<-x
    orderedCONDF<-sort(x) #sorted x
    for(quan in 2:max(n_quan,2)){
      cur_obs<- (z<=min(quans[quan],max(z),na.rm = T) & z>=quans[quan-1])
      x_curquan=x[cur_obs]
      z_curquan=z[cur_obs]
      if(sd(x_curquan)<min_sd) x_curquan<-x_curquan+rnorm(length(x_curquan),sd=sd(x)/length(x))
      if(sd(z_curquan)<min_sd) z_curquan<-z_curquan+rnorm(length(z_curquan),sd=sd(z)/length(z))
      mod<-lm(x_curquan~z_curquan)
      rv<-as.numeric(mod$residuals)
      condF<-rank(rv)/length(rv)
      newx[cur_obs]<-condF
    }
    modifiedDAT[,j]<-newx
  }
  modifiedDAT
}