# Ultra-marginal Feature Importance: Learning from Data with Causal Guarantees
## Contains:
1.  R code for reproducing the results of the UMFI paper (AISTATS 2023, https://arxiv.org/abs/2204.09938). 
2.  Easily downloadable R package for basic UMFI functions
3.  Python pacakge is currently in progress

## How to use R package
``` R
library(devtools)
install_github("HydroML/UMFI",upgrade = F)
library(UMFI)
data("BRCA")
X_dat<-BRCA[,2:51]
protected_col<-3

# try removing dependencies via linear regression
S<-preprocess_lr(dat=X_dat,protect = protected_col)
cor(S,S[,protected_col])

# try removing dependencies via linear regression
S<-preprocess_ot(dat=X_dat,protect = protected_col)
cor(S,S[,protected_col])

# try calculating UMFI values for the BRCA dataset (with linear regression)
BRCA$BRCA_Subtype_PAM50<-as.factor(BRCA$BRCA_Subtype_PAM50)
fi<-umfi(X_dat,BRCA$BRCA_Subtype_PAM50,mod_meth = "lr")

```

