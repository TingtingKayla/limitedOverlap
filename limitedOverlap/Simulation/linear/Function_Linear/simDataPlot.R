####this script simulates the dataset by varying the factors
###########################################################

#sample size in each simulation
sampleSize=1000
gammaV=4
interceptV=0.4

#gammaV=2
#interceptV=0.1

DIRECF="C:\\Users\\Tingting.Zhou\\Desktop\\paper2\\Function_Linear\\"  ###function location

source(paste(DIRECF, "\\simData2.R", sep=""))
simdat=simulateDate(sampleSize=1000, seed.num=1, gammaV, interceptV)

data=simdat
treat.varname="Z0"
alpha.level=0


####################################################################################################################
###########measure the amount of overlaps between the treated and control propensity distributions##################
####################################################################################################################
overlapMeasure=function(data, alpha.level, treat.varname, sampleSize){
  #################calculate percent of treated inside 5% and 95% of control distributions##########################
  ##################################################################################################################
  propTreat=data$pa
  propControl=1-data$pa
  treatInd=data[, treat.varname]
  
  qtreat=quantile(propTreat[treatInd==1], probs = c(alpha.level, 1-alpha.level))
  qcontrol=quantile(propControl[treatInd==0], probs = c(alpha.level, 1-alpha.level))
  
  plot(density(propTreat[treatInd==1]))
  lines(density(propTreat[treatInd==0]))
  #abline(v=qtreat[1])
  #abline(v=qtreat[2])
  
  ######first time point 
  set1a=which(propTreat >= qtreat[1] & propTreat <= qtreat[2])
  set1b=which(propControl >= qcontrol[1] & propControl <= qcontrol[2])
  setInt1=set1a[set1a %in% set1b]
  
  return( c(length(setInt1)/sampleSize, sum(treatInd[setInt1]),  sum( (treatInd[setInt1])==0 )))
  
}


###############################################################################################
###############################################################################################
###asymetric truncation, at quantile level
truncateQVal=seq(0, 0.02, 0.005)

simdat=simulateDate(sampleSize=1000, seed.num=1, gammaV, interceptV)
outOverlap=NULL
for(alpha.level in truncateQVal){
  outOverlap=rbind(outOverlap, overlapMeasure(data=simdat, alpha.level=alpha.level, treat.varname, sampleSize = nrow(simdat)) )
}

outOverlap

simdat=simulateDate(sampleSize=500, seed.num=1, gammaV, interceptV)
outOverlap=NULL
for(alpha.level in truncateQVal){
  outOverlap=rbind(outOverlap, overlapMeasure(data=simdat, alpha.level=alpha.level, treat.varname, sampleSize = nrow(simdat)) )
}

outOverlap

