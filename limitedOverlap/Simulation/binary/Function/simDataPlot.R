####this script simulates the dataset by varying the factors
###########################################################

#sample size in each simulation
sampleSize=1000
gammaV=4
interceptV=0.4

#gammaV=2
#interceptV=0.1

DIRECF="C:\\Users\\Tingting.Zhou\\Desktop\\paper2\\Function\\"  ###function location

source(paste(DIRECF, "\\simData.R", sep=""))
simdat=simulateDate(sampleSize, seed.num=d, gammaV, interceptV)


####################################################################################################################
###########measure the amount of overlaps between the treated and control propensity distributions##################
####################################################################################################################
overlapMeasure=function(data, alpha.level=0.05, treat.varname, sampleSize){
  #################calculate percent of treated inside 5% and 95% of control distributions##########################
  ##################################################################################################################
  propTreat=data$pa
  propControl=1-data$pa
  treatInd=data[, treat.varname]
  
  qtreat=quantile(propTreat[treatInd==1], probs = c(alpha.level, 1-alpha.level))
  qcontrol=quantile(propControl[treatInd==0], probs = c(alpha.level, 1-alpha.level))
  
  plot(density(propTreat[treatInd==1]))
  lines(density(propTreat[treatInd==0]))
  abline(v=qtreat[1])
  abline(v=qtreat[2])
  
  ######first time point 
  set1a=which(propTreat >= qtreat[1] & propTreat <= qtreat[2])
  set1b=which(propControl >= qcontrol[1] & propControl <= qcontrol[2])
  setInt1=set1a[set1a %in% set1b]
  
  return( c(length(setInt1)/sampleSize, sum(treatInd[setInt1]),  sum( (treatInd[setInt1])==0 )))
  
}


###############################################################################################
###############################################################################################
alpha.level=0.05
simdat=simulateDate(sampleSize=10000, overlapL="low", parallS="no", seed.num=1)  
overlapMeasure(data=simdat, alpha.level=alpha.level, treat.varname="A0", sampleSize = nrow(simdat))

simdat=simulateDate(sampleSize=10000, overlapL="high", parallS="yes", seed.num=1)  
overlapMeasure(data=simdat, alpha.level=alpha.level, treat.varname="A0", sampleSize = nrow(simdat))



