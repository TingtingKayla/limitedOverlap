
###diagnostic checks


######data current dataset
####dataOR original dataset

###########absolute standardized mean difference between treated and control before and after matching, truncation, weighting
#data=simdatFit
#propenVarList-list of baseline covariates that I will check for balance

######weighted####################
absStdDiff= function(weight, xVar, treat) {
  
  out=NULL
  if(is.factor(xVar)){
    
    p1 = sum((xVar[treat==1]==1))/sum(treat==1)  ###proportion of treated A0==1 in the treated group A1=1
    p0 = sum((xVar[treat==0]==1))/sum(treat==0)  ##proportion of treated A0==1 in the control group A1=0
    
    var0 = p0*(1-p0)
    var1 = p1*(1-p1)
    
    out = abs( sum(weight * as.numeric(xVar==1) * (treat==1))/sum(weight * (treat==1)) - sum(weight * as.numeric(xVar==0) * (treat==0))/sum(weight * (treat==0)) )/
      sqrt( ( var0 + var1 ) / 2 ) 
    
  } else {
    
    out = abs( sum(weight * xVar * (treat==1))/sum(weight * (treat==1)) - sum(weight * xVar * (treat==0))/sum(weight * (treat==0)) )/
      sqrt( ( var(xVar[treat==1]) + var(xVar[treat==0]) ) / 2 )
    
  }
  
  
  return(out)
}



######unweighted##################
absStdDiffmatch= function(xVar, treat, inclusion) {
  
  xVarLeft = xVar[inclusion]
  treatLeft = treat[inclusion]
  
  out=NULL
  if(is.factor(xVar)){
    
    p1 = sum((xVarLeft[treatLeft==1]==1))/sum(treatLeft==1)  ###proportion of treated A0==1 in the treated group A1=1
    p0 = sum((xVarLeft[treatLeft==0]==1))/sum(treatLeft==0)  ##proportion of treated A0==1 in the control group A1=0
    
    p1_ = sum((xVar[treat==1]==1))/sum(treat==1)  ###proportion of treated A0==1 in the treated group A1=1
    p0_ = sum((xVar[treat==0]==1))/sum(treat==0)  ##proportion of treated A0==1 in the control group A1=0
    
    var0 = p0_*(1-p0_)
    var1 = p1_*(1-p1_)
    
    out = abs( p1 - p0)/ sqrt( ( var0 + var1 ) / 2 ) 
    
  } else {
    
    out = abs(mean(xVarLeft[treatLeft==1]) - mean(xVarLeft[treatLeft==0]))/
      sqrt( ( var(xVar[treat==1]) + var(xVar[treat==0]) ) / 2 )
    
  }
  
  return(out)
}



######unweighted##################
absStdDiffRaw = function(xVar, treat) {
  
  out=NULL
  if(is.factor(xVar)){
    
    p1 = sum((xVar[treat==1]==1))/sum(treat==1)  ###proportion of treated A0==1 in the treated group A1=1
    p0 = sum((xVar[treat==0]==1))/sum(treat==0)  ##proportion of treated A0==1 in the control group A1=0
    
    var0 = p0*(1-p0)
    var1 = p1*(1-p1)
    
    out = abs( p1 - p0)/ sqrt( ( var0 + var1 ) / 2 ) 
    
  } else {
    
    out = abs(mean(xVar[treat==1]) - mean(xVar[treat==0]))/
      sqrt( ( var(xVar[treat==1]) + var(xVar[treat==0]) ) / 2 )
    
  }
  
  return(out)
}


###absolute mean difference (unweighted)
before=sapply(1:length(varList), function(x) absStdDiffRaw(xVar=dataOR[, varList[x]], treat=treatInd)) ###raw before any adjustment
names(before) = varList




########## ATO weighted population #########
ATODiff = function(xVar, treat, probT){
  
  out=NULL
  if(is.factor(xVar)){
    
    p1 = sum((xVar[treat==1]==1))/sum(treat==1)  ###proportion of treated A0==1 in the treated group A1=1
    p0 = sum((xVar[treat==0]==1))/sum(treat==0)  ##proportion of treated A0==1 in the control group A1=0
    
    var0 = p0*(1-p0)
    var1 = p1*(1-p1)
    
    out = abs( sum((1-probT) * as.numeric(xVar==1) * (treat==1))/sum((1-probT) * (treat==1)) - sum(probT * as.numeric(xVar==1) * (treat==0))/sum(probT * (treat==0)) )/
      sqrt( ( var0 + var1 ) / 2 ) 
    
  } else {
    
    out =abs( sum(xVar[treat==1] * (1-probT)[treat==1])/sum( (1-probT)[treat==1] ) -
                sum(xVar[treat==0] * probT[treat==0])/sum( probT[treat==0] ) ) / sqrt( ( var(xVar[treat==1]) + var(xVar[treat==0]) ) / 2 )
    
  }
  
  
}



expit=function(x){
  exp(x)/(1+exp(x))
}



C=dataOR[, varList[x]]
treat=treatInd
pslogit=dataOR$pslogit


##############mean standardized residual difference after controlling for propensity, penalized spline on logit of propensity 
stdStdDiffReg <- function (C, treat, pslogit) {  ### 
  
  tempData=data.frame(id=rep(1, length(pslogit)), C=C, A=treat, pslogit=pslogit)
  
  if(is.factor(C)){
    
    diag1 <- gam(C ~ s(pslogit), data=tempData, family = "binomial")
    tempData$res=residuals(diag1)
    tempData$res = expit(tempData$res)
    
    
  } else {
    
    diag1 <- gam(C ~ s(pslogit), data=tempData)
    tempData$res=residuals(diag1)
    
  }
  
  
  
  ###################################################
  meanTreat=mean(tempData[tempData$A==1, "res"] ) 
  meanControl=mean(tempData[tempData$A==0, "res"] )  
  
  if(is.factor(C)){
    
    p1 = sum((C[treat==1]==1))/sum(treat==1)  ###proportion of treated A0==1 in the treated group A1=1
    p0 = sum((C[treat==0]==1))/sum(treat==0)  ##proportion of treated A0==1 in the control group A1=0
    
    s2_T = p1*(1-p1)
    s2_C = p0*(1-p0)
    
  } else {
    
    s2_T=var(C[tempData$A==1] )  
    s2_C=var(C[tempData$A==0] ) 
  }
  
  
  diff_after=abs((meanTreat - meanControl)/sqrt( (s2_T + s2_C)/2 ))
  
  
  par(mfrow=c(1,2))
  boxplot(tempData$C ~ as.factor(tempData$A), ylim=c(-5, 5))
  boxplot(tempData$res ~ as.factor(tempData$A), ylim=c(-5, 5))
  
  return(diff_after)
  
}

