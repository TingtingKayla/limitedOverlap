####this script simulates the dataset by varying the factors
###########################################################

#varying the value of gammaV for different degrees of overlap
##for set the proportion of treated roughyly equal to 40%, set the values gamma and intercept of propensity score as follows
##when gammaV=0.4, interceptV is 0.4; gammaV=3, interceptV=0.3; gammaV=2, interceptV=0.1, gammaV=1, interceptV=-0.1

###homogenous treatment effects

#rm(list=ls())

library("MASS")


gammaV=4
interceptV=0.4



truth=function(gammaV=4, interceptV=0.4, sampleSize=1000000, set.num=1, modelSpec="misWeight"){
  
  
  set.seed(set.num)
  
  n <- sampleSize
  ###x1-x6 covariates
  muV=rep(0, 6)  ###mean 0
  varCov=matrix(c( c(1, rep(0.5, 5)),
                   c(rep(0.5, 1), 1, rep(0.5, 4)),
                   c(rep(0.5, 2), 1, rep(0.5, 3)),
                   c(rep(0.5, 3), 1, rep(0.5, 2)),
                   c(rep(0.5, 4), 1, rep(0.5, 1)),
                   c(rep(0.5, 5), 1) ), nrow=6, byrow=T)
  
  tempX=sapply(1:sampleSize, function(g) { mvrnorm(1, mu=muV, Sigma=varCov) })
  
  tempX=t(tempX)
  
  mydata=data.frame(x1=tempX[,1], x2=tempX[,2], x3=tempX[,3], x4=as.numeric(tempX[,4]<0), x5=as.numeric(tempX[,5]<0), x6=as.numeric(tempX[,6]<0))
  alphaV=c(0.15, 0.3, 0.3, -0.2, -0.25, -0.25)*gammaV
  betaV=c(0, -0.5, -0.5, -1.5, 0.8, 0.8, 1.0) ###beta0 intercept 0
  treatEff=0.75  ###treatment effect delta
  
  ###########################################################################
  ########################simulating treatment###############################
  propenDesign=cbind(rep(1, nrow(mydata)), mydata$x1, mydata$x2, mydata$x3, mydata$x4, mydata$x5, mydata$x6)
  a.lin = propenDesign %*% matrix(c(interceptV, alphaV), ncol=1)  ###
  pa <- exp(a.lin)/(1 + exp(a.lin)) ###treatment prob
  summary(pa)
  
  ###treatment assignment
  mydata$Z0 <- rbinom(n, 1, prob = as.vector(pa))###treatment status
  mean(mydata$Z0)
  
  
  ###################################################################################################
  ##########################
  outcomeY0Design=cbind(rep(1, nrow(mydata)), mydata$x1, mydata$x2, mydata$x3, mydata$x4, mydata$x5, mydata$x6)
  outcomeY1Design=cbind(rep(1, nrow(mydata)), mydata$x1, mydata$x2, mydata$x3, mydata$x4, mydata$x5, mydata$x6, rep(1, nrow(mydata)))
  
  
  ###strength of covariates not aligned
  ###outcome under treatment
  mydata$y0 = outcomeY0Design %*% matrix(betaV, ncol=1) + rnorm(nrow(mydata), 0, 1)
  mydata$y1 = outcomeY1Design %*% matrix(c(betaV, treatEff), ncol=1) + rnorm(nrow(mydata), 0, 1)
  
  
  mean(mydata$y1-mydata$y0)
  
  ##observed outcome
  mydata$y = mydata$Z0*mydata$y1 + (1-mydata$Z0)*mydata$y0   
  mydata$id=1
  mydata$pa = pa  ##true propensity score
  
  out=NULL
  
  #############################################################################################################################
  #############################################################################################################################
  

  ##################################
  if(modelSpec != "misWeight"){   ###if propensity score model is correctly specified

  ATE=mean(mydata$y1 - mydata$y0)
  ATE
  

  ############ATO#########################
  ATOWeight = (1 - mydata$pa) * mydata$pa
  ATOWeight_std = ATOWeight/sum(ATOWeight)
  ATO = sum(ATOWeight_std *(mydata$y1-mydata$y0))
  ATO
  

  ############## ATM matching weights ##################
  ATMWeight = (1-mydata$pa)*as.numeric( mydata$pa > 0.5 ) + mydata$pa * as.numeric( mydata$pa <= 0.5 )
  ATMWeight_std=ATMWeight/sum(ATMWeight)
  ATM=NULL
  ATM=sum(ATMWeight_std*(mydata$y1-mydata$y0))
  ATM
  
  
  ############################
  ATT=mean((mydata$y1-mydata$y0)[which(mydata$Z0==1)])
  ATT

  
  ############ATC#########################
  ATC=mean((mydata$y1-mydata$y0)[which(mydata$Z0==0)])
  ATC
  
  
  ###asymetric truncation, at quantile level
  truncateVal=seq(0.01, 0.1, 0.01)
  truncateQVal=seq(0, 0.03, 0.005)
  
  
  ############truncated weights################
  ############truncated weights################
  truncate=NULL
  for(qval in truncateVal){
    
    ######first time point 
    setInt1=which(mydata$pa >= qval & mydata$pa <= 1-qval )
    truncate=c(truncate, mean((mydata$y1-mydata$y0)[setInt1]) )
  }
  truncate
  
  
  
  ############truncated weights truncation at quantile################
  truncateQ=NULL
  for(qval in truncateQVal){
    
    #########truncate at quantile level#######
    qtreat=quantile(mydata$pa[mydata$Z0==1], probs = c(qval, 1-qval))
    qcontrol=quantile((1-mydata$pa)[mydata$Z0==0], probs = c(qval, 1-qval))
    
    ######first time point 
    setInt1=NULL
    set1a=which(mydata$pa >= qtreat[1] & mydata$pa <= qtreat[2])
    set1b=which((1-mydata$pa) >= qcontrol[1] & (1-mydata$pa) <= qcontrol[2])
    setInt1=set1a[set1a %in% set1b]
    
    truncateQ=c(truncateQ, mean((mydata$y1-mydata$y0)[setInt1]) )
    
  }
  truncateQ
  
  
  names(truncate)=paste0("truncate", truncateVal)
  names(truncateQ)=paste0("truncateQ", truncateQVal)
  
  
  out=list(ATE=ATE, ATM=ATM, ATT=ATT, ATC=ATC, ATO=ATO, truncate=truncate, truncateQ=truncateQ)
  
  #############################################################################################################################
  #############################################################################################################################
  
  } else if(modelSpec == "misWeight"){   ###misspecified propensity score model
    
    
    
    ######## propensity score model ########################################################### 
    ###fit the propensity, use them for all the estimates
    ##for propensity model
    propen.model=NULL
    propen.model=as.formula(paste("Z0", "~ ", paste(c("x1", "x2", "x3", "x4"), collapse = "+")))
    
    ######## propensity score model ##################################################################################
    ######## estimate propensity score model in the boostrap sample########################################################### 
    ps.fit=glm(propen.model, data=mydata, family="binomial")
    mydata$pa_mis=predict(ps.fit, newdata=mydata, type="response")
    
  
    ATE=mean(mydata$y1 - mydata$y0)
    ATE
    
    
    ############ATO#########################
    ATOWeight = (1 - mydata$pa_mis) * mydata$pa_mis
    ATOWeight_std = ATOWeight/sum(ATOWeight)
    ATO = sum(ATOWeight_std *(mydata$y1-mydata$y0))
    ATO
    
    
    ############ ATM weight ######################
    ATMWeight = (1-mydata$pa_mis)*as.numeric( mydata$pa_mis > 0.5 ) + mydata$pa_mis * as.numeric( mydata$pa_mis <= 0.5 )
    ATMWeight_std=ATMWeight/sum(ATMWeight)
    ATM=NULL
    ATM=sum(ATMWeight_std*(mydata$y1-mydata$y0))
    ATM
    
    
    ############################
    ATT=mean((mydata$y1-mydata$y0)[which(mydata$Z0==1)])
    ATT
    
    
    
    ############ATC#########################
    ATC=mean((mydata$y1-mydata$y0)[which(mydata$Z0==0)])
    ATC
    
    
    
    ###asymetric truncation, at quantile level
    truncateVal=seq(0.01, 0.1, 0.01)
    truncateQVal=seq(0, 0.03, 0.005)
    
    
    ############truncated weights################
    ############truncated weights################
    truncate=NULL
    for(qval in truncateVal){
      
      ######first time point 
      setInt1=which(mydata$pa_mis >= qval & mydata$pa_mis <= 1-qval )
      truncate=c(truncate, mean((mydata$y1-mydata$y0)[setInt1]) )
    }
    truncate
    
    
    ############truncated weights truncation at quantile################
    truncateQ=NULL
    for(qval in truncateQVal){
      
      #########truncate at quantile level#######
      qtreat=quantile(mydata$pa_mis[mydata$Z0==1], probs = c(qval, 1-qval))
      qcontrol=quantile((1-mydata$pa_mis)[mydata$Z0==0], probs = c(qval, 1-qval))
      
      ######first time point 
      setInt1=NULL
      set1a=which(mydata$pa_mis >= qtreat[1] & mydata$pa_mis <= qtreat[2])
      set1b=which((1-mydata$pa_mis) >= qcontrol[1] & (1-mydata$pa_mis) <= qcontrol[2])
      setInt1=set1a[set1a %in% set1b]
      
      truncateQ=c(truncateQ, mean((mydata$y1-mydata$y0)[setInt1]) )
      
    }
    truncateQ
    
    names(truncate)=paste0("truncate", truncateVal)
    names(truncateQ)=paste0("truncateQ", truncateQVal)
    
    
    out=list(ATE=ATE, ATM=ATM, ATT=ATT, ATC=ATC, ATO=ATO, truncate=truncate, truncateQ=truncateQ)
    
    
  }
  
  
  
  return(out)
  
  
}







