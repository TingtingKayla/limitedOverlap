##############################################################################################
##############################################################################################

##########################obtain the pencomp estimate ##############
#### repNum is the number of multiple imputations needed############

###index denotes the different ways of calculating the variance

processPENCOMP=function(resultIn, estimand="ATE"){
  
  output=NULL
  
  for(j in 1:length(estimand)){
    
    x=resultIn[1,estimand[j], ]
    
    y1=resultIn[2,estimand[j], ]
    y2=resultIn[3,estimand[j], ]
    y3=resultIn[4,estimand[j], ]
    
    
    x=x[!is.na(x)]
    y1=y1[!is.na(y1)]
    y2=y2[!is.na(y2)]
    y3=y3[!is.na(y3)]
    
    numT=length(x)
    theta_d=mean(x)
    
    Wd=c( mean(y1), mean(y2), mean(y3) )  ### within imputation variance
    
    Bd=(1/(numT-1))*(sum((x-mean(x))^2))###between imputation variance
    
    Td=Wd+(1+1/numT)*Bd ###total variability associated with mean
    
    v=(numT-1)*(1+(1/(1/numT+1))*(Wd/Bd))^2 ##degree of freedom
    
    output=rbind(output, c(theta_d[1], sqrt(Td[1]),theta_d[1] + c(-1,1)*qt(0.975,v[1])*sqrt(Td[1]), 
                           sqrt(Td[2]),theta_d[1] + c(-1,1)*qt(0.975,v[2])*sqrt(Td[2]),
                           sqrt(Td[3]),theta_d[1] + c(-1,1)*qt(0.975,v[3])*sqrt(Td[3]) ))
    
  }
  
  row.names(output)=estimand
  colnames(output)=c("pointEst", "sd1", "95CILower1", "95CIupper1", "sd2", "95CILower2", "95CIupper2","sd3", "95CILower3", "95CIupper3")
  
  
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




