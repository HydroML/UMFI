#modify data functions
preprocess_lr<-function(dat,protect){
  #remove dependedence via linear regression
  modifiedDAT<-dat
  tomodify<-setdiff(1:ncol(dat),protect)
  if(sd(dat[,protect])<1e-6) dat[,protect]<-dat[,protect]+rnorm(nrow(dat))
  for(i in tomodify){
    if(sd(dat[,i])<1e-6) dat[,i]<-dat[,i]+rnorm(nrow(dat))
    mod<-lm(dat[,i]~dat[,protect])
    if(summary(mod)$coefficients[2,4]<0.01) modifiedDAT[,i]<- mod$residuals
    if(var(modifiedDAT[,i])==0) modifiedDAT[,i]<-rnorm(nrow(modifiedDAT))
  }
  modifiedDAT
}