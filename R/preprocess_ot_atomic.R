Remove dependencies via optimal transport on discrete data
#'
#' This function removes the dependencies between the protected attribute (discrete) and the rest of the features (all discrete) via optimal transport.
#'
#' @param dat A dataframe or matrix of discrete data 
#' @param protect The column number of the protected attribute
#' @param n_quan The number of quantiles to use to estimate the conditional CDF (default is ceiling(nrow(dat)/150))
#' @param min_sd The minimum standard deviation that data points within a quantile can have before noise is added (default is 1e-6)
#' @return A dataframe or matrix of data with the dependencies between the protected attribute and the rest of the features removed
#' @export
preprocess_ot_atomic<-function(dat,protect){
  modifiedDAT<-dat #the new dataframe that will be returned
  tomodify<-setdiff(1:ncol(dat),protect) #the columns in dat to modify
  z=dat[,protect] #the protected attribute
  #loop through each feature we need to modify
  for(j in tomodify){
    x=dat[,j] #feature we will modifty
    newx<-x
    orderedCONDF<-sort(x) #sorted x
    c_pmf<-table(x,z)
    c_cmf<-c_pmf
    for(r in 1:nrow(c_cmf)){
      for(c in 1:ncol(c_cmf)){
        c_cmf[r,c]<-sum(c_pmf[1:r,c])/sum(c_pmf[,c])
      }
    }
    
    for(i in 1:length(x)){
      xi_min= x[x<x[i]]
      if(length(xi_min)>0) xi_min<-max(xi_min)
      if(length(xi_min)==0) xi_min<- -1e12
      what<-as.numeric(rownames(c_cmf))==xi_min
      if(sum(what)>0) left= c_cmf[what,  as.numeric(colnames(c_cmf))==z[i]]
      if(sum(what)==0) left= 0
      what<-as.numeric(rownames(c_cmf))==x[i]
      if(sum(what)>0) right= c_cmf[what,  as.numeric(colnames(c_cmf))==z[i]]
      newx[i]<-runif(1,left,right)
    }
    modifiedDAT[,j]<-newx
  }
  modifiedDAT
}






