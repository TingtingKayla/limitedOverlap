##############################################################################################
##############################################################################################
#### ONE TIME POINT Treatment for PENCOMP
###MISWEIGHT MODEL, USING VARIABLE SELECTION METHOD IN IMBENS AND RUBIN 2015

rm(list=ls())
#library(splines)
library(nlme)
library("mgcv")
library("ggplot2")
library("MASS")

numSim=500  ###500 simulated datasets
#sample size in each simulation
sampleSize=500
#modelSpec="both" ###both propensity score and outcome models correctly specified
modelSpec="misPred" ###misspecified outcome model
#modelSpec="misPred2" ###misspecified outcome model
#modelSpec="misWeight" ###misspecified propensity score model

gammaV=4
interceptV=0.4

#gammaV=2
#interceptV=0.1d


#DIRECF="C:\\Users\\Tingting.Zhou\\Desktop\\paper2\\Function\\"  ###function location
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


num.col=4
###weighted estimators
ATE=matrix(NA, nrow=numSim, ncol=num.col)  
ATE.aug=matrix(NA, nrow=numSim, ncol=num.col) 
ATM=matrix(NA, nrow=numSim, ncol=num.col) 
ATM.aug=matrix(NA, nrow=numSim, ncol=num.col)
ATO=matrix(NA, nrow=numSim, ncol=num.col)
ATO.aug=matrix(NA, nrow=numSim, ncol=num.col)
ATT=matrix(NA, nrow=numSim, ncol=num.col)
ATT.aug=matrix(NA, nrow=numSim, ncol=num.col)
ATC=matrix(NA, nrow=numSim, ncol=num.col)
ATC.aug=matrix(NA, nrow=numSim, ncol=num.col)

num.col=4*length(truncateVal)
truncate=matrix(NA, nrow=numSim, ncol=num.col)
truncate.aug=matrix(NA, nrow=numSim, ncol=num.col)
truncate_rest=matrix(NA, nrow=numSim, ncol=num.col)
truncate.aug_rest=matrix(NA, nrow=numSim, ncol=num.col)

num.col=4*length(truncateQVal)
truncateQ=matrix(NA, nrow=numSim, ncol=num.col)
truncateQ.aug=matrix(NA, nrow=numSim, ncol=num.col)
truncateQ_rest=matrix(NA, nrow=numSim, ncol=num.col)
truncateQ.aug_rest=matrix(NA, nrow=numSim, ncol=num.col)



for(d in 1:numSim){
      
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
        tempR=pencomp(dataOR, data, propenVarList, outcomeVarList0, outcomeVarList1, treat.varname, outcome.varname, truncateQVal, truncateVal)
        
        mulResultP[,,k]=tempR$other
        mulResultP.truncate[,,k]=tempR$truncate
        mulResultP.truncateQ[,,k]=tempR$truncateQ
        
        
        }, error=function(e){"error"} )
        
      }
      
      pencompEst=processPENCOMP(resultIn=mulResultP, estimand=c("ATE", "ATM","ATM_w", "ATO", "ATT", "ATT_w", "ATC", "ATC_w"))
      pencompEst.truncate=processPENCOMP(resultIn=mulResultP.truncate, estimand=column.names2)
      pencompEst.truncateQ=processPENCOMP(resultIn=mulResultP.truncateQ, estimand=column.names3)
      
      
      
      
      ###weighted estimators
      est_weight=balanceWeight(data=dataOR, propenVarList, outcomeVarList0, outcomeVarList1, treat.varname, outcome.varname, truncateQVal, truncateVal)
      
      
      #####################################################
      repBoot=500 ###number of bootstrap samples for weighted estimators 
      column.namesW=c("ATE", "ATE.aug", "ATM", "ATM.aug", "ATT", "ATT.aug", "ATC" ,"ATC.aug", "ATO", "ATO.aug") 
      column.namesW2=paste0("truncate", truncateVal) 
      column.namesW3=paste0("truncateQ", truncateQVal)
      row.names=c("estimate")
      matrix.names=paste0("boot", 1:repBoot)
      
      
      ###truncate estimand
      est_weight.truncate=est_weight$truncate
      est_weight.truncate.aug=est_weight$truncate.aug
      est_weight.truncate_rest=est_weight$truncate_rest
      est_weight.truncate.aug_rest=est_weight$truncate.aug_rest
      
      ###truncated estimand
      est_weight.truncateQ=est_weight$truncateQ
      est_weight.truncateQ.aug=est_weight$truncateQ.aug
      est_weight.truncateQ_rest=est_weight$truncateQ_rest
      est_weight.truncateQ.aug_rest=est_weight$truncateQ.aug_rest
      
      
      wboot=array(NA, dim = c(1, length(column.namesW), repBoot), dimnames = list(row.names, column.namesW, matrix.names)) ###
      
      wboot.truncate=array(NA, dim = c(1, length(column.namesW2), repBoot), dimnames = list(row.names, column.namesW2, matrix.names)) ###
      wboot.truncate.aug=array(NA, dim = c(1, length(column.namesW2), repBoot), dimnames = list(row.names, column.namesW2, matrix.names)) ###
      wboot.truncate.rest=array(NA, dim = c(1, length(column.namesW2), repBoot), dimnames = list(row.names, column.namesW2, matrix.names)) ###
      wboot.truncate.aug.rest=array(NA, dim = c(1, length(column.namesW2), repBoot), dimnames = list(row.names, column.namesW2, matrix.names)) ###
      
      
      wboot.truncateQ=array(NA, dim = c(1, length(column.namesW3), repBoot), dimnames = list(row.names, column.namesW3, matrix.names)) ###
      wboot.truncateQ.aug=array(NA, dim = c(1, length(column.namesW3), repBoot), dimnames = list(row.names, column.namesW3, matrix.names)) ###
      wboot.truncateQ.rest=array(NA, dim = c(1, length(column.namesW3), repBoot), dimnames = list(row.names, column.namesW3, matrix.names)) ###
      wboot.truncateQ.aug.rest=array(NA, dim = c(1, length(column.namesW3), repBoot), dimnames = list(row.names, column.namesW3, matrix.names)) ###
      
      
      
      for(k in 1:repBoot){
        ##take a bootstrap sample, stratified bootstrap sampling
        set.seed(k)
        data = dataOR[c( sample(treatID, replace = T), sample(controlID, replace = T) ),] 
        tempR=balanceWeight(data=data, propenVarList, outcomeVarList0, outcomeVarList1, treat.varname, outcome.varname, truncateQVal, truncateVal)
        wboot[,,k]=tempR$other
        wboot.truncate[,,k]=tempR$truncate
        wboot.truncate.aug[,,k]=tempR$truncate.aug
        wboot.truncate.rest[,,k]=tempR$truncate_rest
        wboot.truncate.aug.rest[,,k]=tempR$truncate.aug_rest
        
        wboot.truncateQ[,,k]=tempR$truncateQ
        wboot.truncateQ.aug[,,k]=tempR$truncateQ.aug
        wboot.truncateQ.rest[,,k]=tempR$truncateQ_rest
        wboot.truncateQ.aug.rest[,,k]=tempR$truncateQ.aug_rest
        
        
      }
      
      
      weightEst=process.boot(estOrg=est_weight$other, estBoot=wboot, 
                             estimand=c("ATE", "ATE.aug", "ATM", "ATM.aug", "ATT", "ATT.aug", "ATC" ,"ATC.aug", "ATO", "ATO.aug") )
      
      weightEst.truncate=process.boot(estOrg=est_weight$truncate, estBoot=wboot.truncate, estimand=column.namesW2 )
      weightEst.truncate.aug=process.boot(estOrg=est_weight$truncate.aug, estBoot=wboot.truncate.aug, estimand=column.namesW2 )
      weightEst.truncate.rest=process.boot(estOrg=est_weight$truncate_rest, estBoot=wboot.truncate.rest, estimand=column.namesW2 )
      weightEst.truncate.aug.rest=process.boot(estOrg=est_weight$truncate.aug_rest, estBoot=wboot.truncate.aug.rest, estimand=column.namesW2 )
      
      
      weightEst.truncateQ=process.boot(estOrg=est_weight$truncateQ, estBoot=wboot.truncateQ, estimand=column.namesW3 )
      weightEst.truncateQ.aug=process.boot(estOrg=est_weight$truncateQ.aug, estBoot=wboot.truncateQ.aug, estimand=column.namesW3 )
      weightEst.truncateQ.rest=process.boot(estOrg=est_weight$truncateQ_rest, estBoot=wboot.truncateQ.rest, estimand=column.namesW3 )
      weightEst.truncateQ.aug.rest=process.boot(estOrg=est_weight$truncateQ.aug_rest, estBoot=wboot.truncateQ.aug.rest, estimand=column.namesW3 )
      
      
      
      
      
      
      
      
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
      
      
      
      ###weighted estimators
      ATE[d,]=weightEst["ATE",] 
      ATE.aug[d,]=weightEst["ATE.aug",] 
      ATM[d,]=weightEst["ATM",]
      ATM.aug[d,]=weightEst["ATM.aug",]
      ATO[d,]=weightEst["ATO",]
      ATO.aug[d,]=weightEst["ATO.aug",]
      
      
      truncate[d,]=combineR(weightEst.truncate)
      truncate.aug[d,]=combineR(weightEst.truncate.aug)
      truncate_rest[d,]=combineR(weightEst.truncate.rest)
      truncate.aug_rest[d,]=combineR(weightEst.truncate.aug.rest)
      
      truncateQ[d,]=combineR(weightEst.truncateQ)
      truncateQ.aug[d,]=combineR(weightEst.truncateQ.aug)
      truncateQ_rest[d,]=combineR(weightEst.truncateQ.rest)
      truncateQ.aug_rest[d,]=combineR(weightEst.truncateQ.aug.rest)
      
      
      ATT[d,]=weightEst["ATT",]
      ATT.aug[d,]=weightEst["ATT.aug",]
      ATC[d,]=weightEst["ATC",]
      ATC.aug[d,]=weightEst["ATC.aug",]
      
      
      
      
      ###weighted estimator output
      write.table(ATE, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATE", "_", modelSpec, ".txt", sep=""),
                  row.names = F, col.names=c("estimate", "std", "lowerCI", "upperCI"), quote = F)
      write.table(ATE.aug, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATE.aug", "_", modelSpec, ".txt", sep=""), row.names = F, 
                  col.names=c("estimate", "std", "lowerCI", "upperCI"), quote = F)
      
      write.table(ATM, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATM", "_", modelSpec, ".txt", sep=""), row.names = F,
                  col.names=c("estimate", "std", "lowerCI", "upperCI"), quote = F)
      write.table(ATM.aug, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATM.aug", "_", modelSpec, ".txt", sep=""), row.names = F,
                  col.names=c("estimate", "std", "lowerCI", "upperCI"), quote = F)
      
      write.table(ATO, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATO", "_", modelSpec, ".txt", sep=""), row.names = F, 
                  col.names=c("estimate", "std", "lowerCI", "upperCI"), quote = F)
      write.table(ATO.aug, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATO.aug", "_", modelSpec, ".txt", sep=""), row.names = F,
                  col.names=c("estimate", "std", "lowerCI", "upperCI"), quote = F)
      
      
      outColName=paste0(c("estimate", "std", "lowerCI", "upperCI"), rep(truncateVal, each=4))
      outColNameQ=paste0(c("estimate", "std", "lowerCI", "upperCI"), rep(truncateQVal, each=4))
      
      write.table(truncate, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncate", "_", modelSpec, ".txt", sep=""), row.names = F,
                  col.names=outColName, quote = F)
      write.table(truncate.aug, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncate.aug", "_", modelSpec, ".txt", sep=""), row.names = F, 
                  col.names=outColName, quote = F)
      
      write.table(truncateQ, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncateQ", "_", modelSpec, ".txt", sep=""), row.names = F,
                  col.names=outColNameQ, quote = F)
      write.table(truncateQ.aug, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncateQ.aug", "_", modelSpec, ".txt", sep=""), row.names = F,
                  col.names=outColNameQ, quote = F)
      
      
      #####re-estimate the propensity score
      write.table(truncate_rest, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncate_rest", "_", modelSpec, ".txt", sep=""), row.names = F,
                  col.names=outColName, quote = F)
      write.table(truncate.aug_rest, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncate.aug_rest", "_", modelSpec, ".txt", sep=""), row.names = F, 
                  col.names=outColName, quote = F)
      
      write.table(truncateQ_rest, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncateQ_rest", "_", modelSpec, ".txt", sep=""), row.names = F,
                  col.names=outColNameQ, quote = F)
      write.table(truncateQ.aug_rest, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncateQ.aug_rest", "_", modelSpec, ".txt", sep=""), row.names = F,
                  col.names=outColNameQ, quote = F)
      
      
      
      
      
      write.table(ATT, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATT", "_", modelSpec, ".txt", sep=""), row.names = F,
                  col.names=c("estimate", "std", "lowerCI", "upperCI"), quote = F)
      write.table(ATT.aug, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATT.aug", "_", modelSpec, ".txt", sep=""), row.names = F,
                  col.names=c("estimate", "std", "lowerCI", "upperCI"), quote = F)
      
      write.table(ATC, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATC", "_", modelSpec, ".txt", sep=""), row.names = F,
                  col.names=c("estimate", "std", "lowerCI", "upperCI"), quote = F)
      write.table(ATC.aug, paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATC.aug", "_", modelSpec, ".txt", sep=""), row.names = F,
                  col.names=c("estimate", "std", "lowerCI", "upperCI"), quote = F)
      
      
      
      
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

