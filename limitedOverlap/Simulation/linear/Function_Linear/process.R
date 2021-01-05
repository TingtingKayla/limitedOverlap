##############################################################################################
##############################################################################################

##########################obtain the pencomp estimate ##############
#### repNum is the number of multiple imputations needed############

###index denotes the different ways of calculating the variance

processPENCOMP=function(resultIn, estimand="ATE"){
  
  output=NULL
  
  
  for(j in 1:length(estimand)){
    x=resultIn[1,estimand[j], ]
    y=resultIn[2,estimand[j],]
    x=x[!is.na(x)]
    y=y[!is.na(y)]
    
    numT=length(x)
    theta_d=mean(x) 
    Wd=mean(y)  ### within imputation variance
    Bd=(1/(numT-1))*(sum((x-mean(x))^2))###between imputation variance
    Td=Wd+(1+1/numT)*Bd ###total variability associated with mean
    v=(numT-1)*(1+(1/(1/numT+1))*(Wd/Bd))^2 ##degree of freedom
    
    output=rbind(output, c(theta_d[1], sqrt(Td[1]),theta_d[1] + c(-1,1)*qt(0.975,v[1])*sqrt(Td[1])))
  }
  
  row.names(output)=estimand
  colnames(output)=c("pointEst", "sd", "95CILower", "95CIupper")
  
  return( output  )
  
}



##########process pencomp estimates using bootstrap samples

process.boot=function(estOrg, estBoot, estimand="ATE"){
  
  
  output=NULL
  for(j in 1:length(estimand)){
    output=rbind(output, c(estOrg[estimand][j], sd(estBoot[, estimand[j], ], na.rm = T), estOrg[estimand[j]] + 1.96 * c(-1, 1) * sd(estBoot[, estimand[j], ], na.rm = T) ) )
  }
  
  row.names(output)=estimand
  colnames(output)=c("pointEst", "sd", "95CILower", "95CIupper")
  
  return( output )
  
  
}




