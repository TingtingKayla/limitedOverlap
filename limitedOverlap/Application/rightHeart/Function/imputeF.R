
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

imputeF=function(dataFit, newdata, outcome.varname, outcomeVarList, num.knot=20) {
  
  
  space0b=(max(dataFit$pslogit)-min(dataFit$pslogit))/(num.knot+1)
  knots0b=(min(dataFit$pslogit)+space0b * (1:num.knot))
  
  ###assume a truncated linear basis
  linear0b=NULL
  linear0b =outer(dataFit$pslogit, knots0b, "-")
  linear0b =linear0b * (linear0b > 0)
  colnames(linear0b)=paste("basis", 1:num.knot, sep="")
  bootSample0b=data.frame(dataFit, linear0b)
  
  modely = as.formula(paste(outcome.varname, "~ ", paste(c(outcomeVarList, "pslogit"), collapse = "+")))
  
  
  if(num.knot==20){
    psppMod <- glmmPQL(modely, 
                       random=list(id=pdIdent(~0+basis1+basis2+basis3+basis4+basis5+basis6+basis7+basis8+basis9+
                                                basis10+basis11+basis12+basis13+basis14+basis15+basis16+basis17+basis18+
                                                basis19+basis20)),
                       family="binomial", data=bootSample0b)
    
  } else if (num.knot==30){
    
    psppMod <- glmmPQL(modely, 
                       random=list(id=pdIdent(~0+basis1+basis2+basis3+basis4+basis5+basis6+basis7+basis8+basis9+
                                                basis10+basis11+basis12+basis13+basis14+basis15+basis16+basis17+basis18+
                                                basis19+basis20+basis21+basis22+basis23+basis24+basis25+basis26+basis27+basis28+basis29+basis30)),
                       family="binomial", data=bootSample0b)
    
  } else if (num.knot==15){
    
    psppMod <- glmmPQL(modely, 
                       random=list(id=pdIdent(~0+basis1+basis2+basis3+basis4+basis5+basis6+basis7+basis8+basis9+
                                                basis10+basis11+basis12+basis13+basis14+basis15)),
                       family="binomial", data=bootSample0b)
    
  }
  
  
  
  ###assume a truncated linear basis
  linear0b=NULL
  linear0b =outer(newdata$pslogit, knots0b, "-")
  linear0b =linear0b * (linear0b > 0)
  colnames(linear0b)=paste("basis", 1:num.knot, sep="")
  newdata=data.frame(newdata, linear0b)
  
  
  fv_v2 <- predict(psppMod, newdata, se.fit=TRUE, type = "response")
  imputed_v2 = rbinom(nrow(newdata), 1, prob=fv_v2)  ###one realization
  imputed_prob_v2=fv_v2 ###imputed probability of death
  
  return(list(modelOut=psppMod, imputed=imputed_v2, imputed_prob=imputed_prob_v2))
  
}











