##############################################################################################
##############################################################################################
#### ONE TIME POINT Treatment for PENCOMP
###MISWEIGHT MODEL, USING VARIABLE SELECTION METHOD IN IMBENS AND RUBIN 2015
##rerun pencomp only 

rm(list=ls())
#library(splines)
library(nlme)
library("mgcv")
library("ggplot2")
library("MASS")

numSim=500  ###500 simulated datasets
#sample size in each simulation
sampleSize=500
modelSpec="both" ###both propensity score and outcome models correctly specified
#modelSpec="misPred" ###misspecified outcome model
#modelSpec="misPred2" ###misspecified outcome model
#modelSpec="misWeight" ###misspecified propensity score model

gammaV=4
interceptV=0.4

#gammaV=2
#interceptV=0.1

DIRECF="C:\\Users\\Tingting.Zhou\\Desktop\\paper2\\Function\\"  ###function location
#DIRECR="C:/Users/Tingting.Zhou/Desktop/paper2/simulation/"  ###simulation result


DIRECR="C:/Users/tkzhou/Desktop/binary/homoT/"
DIRECF="C:/Users/tkzhou/Desktop/binary/Function/"


source(paste(DIRECF, "\\simData.R", sep=""))
source(paste(DIRECF, "\\imputeF.R", sep=""))  
source(paste(DIRECF, "\\pencomp2.R", sep=""))  
source(paste(DIRECF, "\\process.R", sep=""))  
source(paste(DIRECF, "\\formulaContruct.R", sep="")) 
source(paste(DIRECF, "\\balanceWeight.R", sep="")) 
source(paste(DIRECF, "\\pairMatch.R", sep="")) 

#################################################################################################
#################################################################################################
#### repeat the simulations for a large number of times


##a is matrix, data.frame
combineR=function(a){
  outa=NULL
  for(j in 1:nrow(a)){
    outa=c(outa, a[j,])
  }     
  return(outa)
}

#################################################################################################
#################################################################################################
#### repeat the simulations for a large number of times

###asymetric truncation, at quantile level
truncateVal=seq(0.01, 0.1, 0.01)
truncateQVal=seq(0, 0.03, 0.005)


num.col=10
###pencomp
ATE_pencomp=matrix(NA, nrow=numSim, ncol=num.col)  
ATM_pencomp=matrix(NA, nrow=numSim, ncol=num.col)  
ATM_w_pencomp=matrix(NA, nrow=numSim, ncol=num.col)  
ATO_pencomp=matrix(NA, nrow=numSim, ncol=num.col) 
ATT_pencomp=matrix(NA, nrow=numSim, ncol=num.col)
ATT_w_pencomp=matrix(NA, nrow=numSim, ncol=num.col)
ATC_pencomp=matrix(NA, nrow=numSim, ncol=num.col)
ATC_w_pencomp=matrix(NA, nrow=numSim, ncol=num.col)

num.col=10*length(truncateVal)
truncate_pencomp=matrix(NA, nrow=numSim, ncol=num.col)

num.col=10*length(truncateQVal)
truncateQ_pencomp=matrix(NA, nrow=numSim, ncol=num.col)



for(d in 1:numSim) {
  
  print(d)   
  
  set.seed(d)
  
  simdat=simulateDate(sampleSize, seed.num=d, gammaV, interceptV)
  
  if(modelSpec=="both"){ ##both propensity and prediction models correct
    ###correct model
    propenVarList=c("x1", "x2", "x3", "x4", "x5", "x6")
    outcomeVarList0=c("x1", "x2", "x3", "x4", "x5", "x6")
    outcomeVarList1=c("x1", "x2", "x3", "x4", "x5", "x6")
  } else if(modelSpec=="misPred"){  ###misspecifed prediction model
    propenVarList=c("x1", "x2", "x3", "x4", "x5", "x6")
    outcomeVarList0=c("x1", "x2")
    outcomeVarList1=c("x1", "x2")
  } else if(modelSpec=="misWeight"){
    ###misspecified propensity model
    propenVarList=c("x1", "x2", "x3", "x4")
    outcomeVarList0=c("x1", "x2", "x3", "x4", "x5", "x6")
    outcomeVarList1=c("x1", "x2", "x3", "x4", "x5", "x6")
  } else if(modelSpec=="misPred2"){  ###misspecifed prediction model
    propenVarList=c("x1", "x2", "x3", "x4", "x5", "x6")
    outcomeVarList0=NULL
    outcomeVarList1=NULL
  }
  
  treat.varname = "Z0"
  outcome.varname = "y"
  
  dataOR=simdat
  
  treatID = which(dataOR[, treat.varname] == 1)
  controlID = which(dataOR[, treat.varname] == 0)
  
  
  
  ###################################################
  repNum=200  ### number of multiple imputation
  column.names=c("ATE",  "ATM", "ATM_w", "ATO",  "ATT", "ATT_w", "ATC", "ATC_w")
  column.names2=paste0("truncate", truncateVal) 
  column.names3=paste0("truncateQ", truncateQVal) 
  
  row.names1=c("estimate", "var1", "var2", "var3")
  matrix.names=paste0("boot", 1:repNum)
  
  mulResultP=array(NA, dim = c(length(row.names1), length(column.names), repNum), dimnames = list(row.names1, column.names, matrix.names)) ###using truncated linear basis
  mulResultP.truncate=array(NA, dim = c(length(row.names1), length(column.names2), repNum), dimnames = list(row.names1, column.names2, matrix.names)) ###using truncated linear basis
  mulResultP.truncateQ=array(NA, dim = c(length(row.names1), length(column.names3), repNum), dimnames = list(row.names1, column.names3, matrix.names)) ###using truncated linear basis
  
  
  for(k in 1:repNum){
    
    tryCatch ( 
      {
        ##take a bootstrap sample, stratified bootstrap sampling
        set.seed(k)
        data = dataOR[c( sample(treatID, replace = T), sample(controlID, replace = T) ),]  
        tempR=pencomp(dataOR, data, propenVarList, outcomeVarList0=outcomeVarList0[-c(6)], outcomeVarList1=outcomeVarList1[-c(6)], treat.varname, outcome.varname, truncateQVal, truncateVal)
        
        mulResultP[,,k]=tempR$other
        mulResultP.truncate[,,k]=tempR$truncate
        mulResultP.truncateQ[,,k]=tempR$truncateQ
        
      }, error=function(e){"error"} )
    
  }
  
  pencompEst=processPENCOMP(resultIn=mulResultP, estimand=c("ATE", "ATM","ATM_w", "ATO", "ATT", "ATT_w", "ATC", "ATC_w"))
  pencompEst.truncate=processPENCOMP(resultIn=mulResultP.truncate, estimand=column.names2)
  pencompEst.truncateQ=processPENCOMP(resultIn=mulResultP.truncateQ, estimand=column.names3)
  
  
  ##pencomp estimator   
  ATE_pencomp[d,]=pencompEst["ATE",]  
  ATM_pencomp[d,]=pencompEst["ATM",]  
  ATM_w_pencomp[d,]=pencompEst["ATM_w",] 
  ATO_pencomp[d,]=pencompEst["ATO",]
  ATT_pencomp[d,]=pencompEst["ATT",]
  ATT_w_pencomp[d,]=pencompEst["ATT_w",]
  ATC_pencomp[d,]=pencompEst["ATC",]
  ATC_w_pencomp[d,]=pencompEst["ATC_w",]
  
  truncate_pencomp[d,]=combineR(pencompEst.truncate)
  truncateQ_pencomp[d,]=combineR(pencompEst.truncateQ)
  
  
  
  ###pencomp output
  write.table(ATE_pencomp, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATE_pencomp", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=c("estimate", "std_1", "lowerCI_1", "upperCI_1", "std_2", "lowerCI_2", "upperCI_2", "std_3", "lowerCI_3", "upperCI_3"), quote = F)
  
  write.table(ATM_pencomp, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATM_pencomp", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=c("estimate", "std_1", "lowerCI_1", "upperCI_1", "std_2", "lowerCI_2", "upperCI_2", "std_3", "lowerCI_3", "upperCI_3"), quote = F)
  write.table(ATM_w_pencomp, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATM_w_pencomp", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=c("estimate", "std_1", "lowerCI_1", "upperCI_1", "std_2", "lowerCI_2", "upperCI_2", "std_3", "lowerCI_3", "upperCI_3"), quote = F)
  
  write.table(ATO_pencomp, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATO_pencomp", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=c("estimate", "std_1", "lowerCI_1", "upperCI_1", "std_2", "lowerCI_2", "upperCI_2", "std_3", "lowerCI_3", "upperCI_3"), quote = F)
  
  write.table(ATT_pencomp, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATT_pencomp", "_", modelSpec, ".txt", sep=""), 
              col.names=c("estimate", "std_1", "lowerCI_1", "upperCI_1", "std_2", "lowerCI_2", "upperCI_2", "std_3", "lowerCI_3", "upperCI_3"),  row.names = F, quote = F)
  write.table(ATT_w_pencomp, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATT_w_pencomp", "_", modelSpec, ".txt", sep=""), 
              col.names=c("estimate", "std_1", "lowerCI_1", "upperCI_1", "std_2", "lowerCI_2", "upperCI_2", "std_3", "lowerCI_3", "upperCI_3"), row.names = F, quote = F)
  
  write.table(ATC_pencomp, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATC_pencomp", "_", modelSpec, ".txt", sep=""), 
              col.names=c("estimate", "std_1", "lowerCI_1", "upperCI_1", "std_2", "lowerCI_2", "upperCI_2", "std_3", "lowerCI_3", "upperCI_3"),  row.names = F, quote = F)
  write.table(ATC_w_pencomp, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATC_w_pencomp", "_", modelSpec, ".txt", sep=""), 
              col.names=c("estimate", "std_1", "lowerCI_1", "upperCI_1", "std_2", "lowerCI_2", "upperCI_2", "std_3", "lowerCI_3", "upperCI_3"), row.names = F, quote = F)
  
  
  outColName=paste0(c("estimate", "std_1", "lowerCI_1", "upperCI_1", "std_2", "lowerCI_2", "upperCI_2", "std_3", "lowerCI_3", "upperCI_3"), rep(truncateVal, each=10))
  outColNameQ=paste0(c("estimate", "std_1", "lowerCI_1", "upperCI_1", "std_2", "lowerCI_2", "upperCI_2", "std_3", "lowerCI_3", "upperCI_3"), rep(truncateQVal, each=10))
  
  write.table(truncate_pencomp, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncate_pencomp", "_", modelSpec, ".txt", sep=""), 
              col.names=outColName,  row.names = F, quote = F)
  write.table(truncateQ_pencomp, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncateQ_pencomp", "_", modelSpec, ".txt", sep=""), 
              col.names=outColNameQ, row.names = F, quote = F)
  
  
  
}

