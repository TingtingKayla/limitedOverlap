##############################################################################################
##############################################################################################
######functions for combining raw data ###############

processPENCOMP=function(resultIn){
  
  output=NULL
  
  x=resultIn[,1]
  
  y1=resultIn[,2]
  
  x=x[!is.na(x)]
  y1=y1[!is.na(y1)]
  
  numT=length(x)
  theta_d=mean(x)
  
  Wd=c( mean(y1) )  ### within imputation variance
  
  Bd=(1/(numT-1))*(sum((x-mean(x))^2))###between imputation variance
  
  Td=Wd+(1+1/numT)*Bd ###total variability associated with mean
  
  v=(numT-1)*(1+(1/(1/numT+1))*(Wd/Bd))^2 ##degree of freedom
  
  output=rbind(output, c(theta_d[1], sqrt(Td[1]),theta_d[1] + c(-1,1)*qt(0.975,v[1])*sqrt(Td[1]) ) )
  
  colnames(output)=c("pointEst", "sd", "95CILower", "95CIupper")
  return(output)
  
  
}


##########process pencomp estimates using bootstrap samples

process.boot=function(estOrg, estBoot){
  
  
  output=NULL
  output=rbind(output, c(mean(estBoot, na.rm = T), sd(estBoot, na.rm = T), 
                         estOrg + 1.96 * c(-1, 1) * sd(estBoot, na.rm = T) ) )
  colnames(output)=c("pointEst", "sd", "95CILower", "95CIupper")
  
  return( output )
  
  
}



########multiply each column by 100
modCol=function(input){
  input=input[,c(1,2)]
  colnames(input)=c("estimate", "sd")
  input = format(input*100, digits = 3)
  return(input)
}





########multiply each column by 100
modCol=function(input){
  input=input[,c(1,2)]
  colnames(input)=c("estimate", "sd")
  input = format(input*100, digits = 3)
  return(input)
}
