####this script simulates the dataset by varying the factors
###########################################################

#varying the value of gammaV for different degrees of overlap
##for set the proportion of treated roughyly equal to 40%, set the values gamma and intercept of propensity score as follows
##when gammaV=0.4, interceptV is 0.4; gammaV=3, interceptV=0.3; gammaV=2, interceptV=0.1, gammaV=1, interceptV=-0.1

#gammaV=0.4
#interceptV=0.4

simulateDate=function(sampleSize, seed.num, gammaV, interceptV) {
  
  library("MASS")
  set.seed(seed.num)
  
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
  
  return(mydata)
  
}



