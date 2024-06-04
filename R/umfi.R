#' Calculate ultra-marginal feature importance
#'
#' This function calculates the UMFI of all features within X according to a specified fair representation learning method and random forests accuracy as an evaluation function.
#'
#' @param X A matrix or dataframe of explanatory features
#' @param y A numeric or factor vector
#' @param mod_meth A string indicating if you want to use linear regression ("lr") or optimal transport ("ot") for dependence removal
#' @return A numeric vector of feature importance scores
#' @export
umfi<- function(X,y,mod_meth="lr"){
  fi<-rep(0,ncol(X))
  for(i in 1:length(fi)){
    if(mod_meth=="ot") newX<-preprocess_ot(X,i)
    if(mod_meth=="lr") newX<-preprocess_lr(X,i)
    if(mod_meth=="ot_atomic") newX<-preprocess_ot_atomic(X,i)
    
    rfwith<-ranger::ranger(x=newX,y=y,num.trees = 100)
    rfwithout<-ranger::ranger(x=data.frame(matrix(newX[,-c(i)],nrow = nrow(newX))),y=y,num.trees = 100)
    if(is.numeric(y)) fi[i]<-max(rfwith$r.squared,0)-max(rfwithout$r.squared,0)
    if(is.factor(y)) fi[i]<- max(1-rfwith$prediction.error,0.5)-max(1-rfwithout$prediction.error,0.5)
  }
  fi[fi<0]<-0
  fi
}


#' Calculate ultra-marginal feature importance
#'
#' This function calculates the UMFI of all features within X in parallel according to a specified fair representation learning method and random forests accuracy as an evaluation function.
#'
#' @param X A matrix or dataframe of explanatory features
#' @param y A numeric or factor vector
#' @param mod_meth A string indicating if you want to use linear regression ("lr") or optimal transport ("ot") for dependence removal
#' @return A numeric vector of feature importance scores
#' @export
umfi_par<- function(X,y,mod_meth){
  fi<-foreach::foreach(i=1:ncol(X),  .inorder = FALSE, .export = c("preprocess_ot","preprocess_lr"),
              .packages = c("ranger", "doParallel"),.combine = 'c')%dopar%{
                if(mod_meth=="ot") newX<-preprocess_ot(X,i)
                if(mod_meth=="lr") newX<-preprocess_lr(X,i)
                if(mod_meth=="ot_atomic") newX<-preprocess_ot_atomic(X,i)
                rfwith<-ranger(x=newX,y=y,num.trees = 100)
                rfwithout<-ranger::ranger(x=data.frame(matrix(newX[,-c(i)],nrow = nrow(newX))),y=y,num.trees = 100)
                if(is.numeric(y)) return(max(rfwith$r.squared,0)-max(rfwithout$r.squared,0))
                if(is.factor(y)) return(max(1-rfwith$prediction.error,0.5)-max(1-rfwithout$prediction.error,0.5))
              }
  fi[fi<0]<-0
  fi
}
