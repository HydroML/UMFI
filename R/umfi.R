#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
umfi<- function(X,y,mod_meth="lr"){
  fi<-rep(0,ncol(X))
  for(i in 1:length(fi)){
    if(mod_meth=="ot") newX<-preprocess_ot(X,i)
    if(mod_meth=="lr") newX<-preprocess_lr(X,i)
    
    rfwith<-ranger::ranger(x=newX,y=y,num.trees = 100)
    rfwithout<-ranger::ranger(x=newX[,-c(i)],y=y,num.trees = 100)
    if(is.numeric(y)) fi[i]<-max(rfwith$r.squared,0)-max(rfwithout$r.squared,0)
    if(is.factor(y)) fi[i]<- max(1-rfwith$prediction.error,0.5)-max(1-rfwithout$prediction.error,0.5)
  }
  fi[fi<0]<-0
  fi
}