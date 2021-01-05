
##dataFit, the data used to fit the model
##newdata, the incomplete dataset with missing outcomes
###outcome.varname, outcome variable name
##outcomeVarList0, list of covariables in the outcome model
###propen.score, logit of propensity score
###num.knot, number of knots

#dataFit=data0
#newdata=newData0
#propen.score=data0$pslogit
#outcomeVarList=outcomeVarList0

#dataFit=data1
#newdata=newData1
#outcomeVarList=outcomeVarList1


####imputedF2

imputeF=function(dataFit, newdata, outcome.varname, outcomeVarList) {
  
  num.knot=20
  
  space0b=(max(dataFit$pslogit)-min(dataFit$pslogit))/(num.knot+1)
  knots0b=(min(dataFit$pslogit)+space0b * (1:num.knot))
  
  ###assume a truncated linear basis
  linear0b=NULL
  linear0b =outer(dataFit$pslogit, knots0b, "-")
  linear0b =linear0b * (linear0b > 0)
  
  response=dataFit[, outcome.varname]
  covariateX=NULL
  
  if(length(outcomeVarList)!=0){  ###has predictors 
    
    for(i in 1:length(outcomeVarList)){
      covariateX=cbind(covariateX, dataFit[, outcomeVarList[i] ])
    }
    covariateX=cbind(rep(1,nrow(dataFit)), covariateX, dataFit$pslogit)
    
  } else {  ###no predictors
    
    covariateX=cbind(rep(1,nrow(dataFit)), dataFit$pslogit)
    
  }

  all=rep(1, dim(dataFit)[1])
  psppM=lme(response ~ covariateX-1, random=list(all=pdIdent(~0+linear0b)) ) 
  
  
  fixCoef=psppM$coefficients$fixed
  names(fixCoef)=c("intercept", outcomeVarList, "propen.score")
  randCoef=psppM$coefficients$random$all
  
  
  ###assume a truncated linear basis using the new dataset
  linear0b=NULL
  linear0b =outer(newdata$pslogit, knots0b, "-")
  linear0b =linear0b * (linear0b > 0)
  
  designM=NULL
  
  if(length(outcomeVarList)!=0){  ###has predictors 
    
    covariateX=NULL
    for(i in 1:length(outcomeVarList)){
      covariateX=cbind(covariateX, newdata[, outcomeVarList[i] ])
    }
    designM=cbind(rep(1,nrow(newdata)), covariateX, newdata$pslogit)
    
  } else {  ###no predictors 
    
    covariateX=NULL
    designM=cbind(rep(1,nrow(newdata)), covariateX, newdata$pslogit)
  }
  

  
  designM=as.matrix(designM)
  imputed_v2 = designM %*% fixCoef + as.matrix(linear0b) %*% as.vector(unlist(randCoef)) + rnorm(nrow(newdata), 0, psppM$sigma)
  
  
  return(list(modelOut=psppM, imputed=imputed_v2))
  
}

