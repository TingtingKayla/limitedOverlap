
rm(list=ls())

############################################################################################################
###process simulation results ############


processResult=function(result, truth) {
  
  result=result[!is.na(result[,1]),]
  
  ###coverage rate
  total_ps11=numeric(dim(result)[1])
  for (g in 1:dim(result)[1]) {
    total_ps11[g]=as.numeric(result[g,3] <= truth & result[g,4] >= truth)
  }
  coverage_ps11=sum(total_ps11)/length(total_ps11)
  coverage_ps11
  
  ###bias and RMSE
  bias11=mean(result[,1]-(truth))
  estimate11=mean(result[,1])
  temp11=(result[,1]-(truth))^2
  RMSE11=sqrt(mean(temp11))
  
  sd11=sd(result[,1])
  width11=mean(abs(result[,4]-result[,3]))
  
  sd11Boot=mean(result[,2]) 
  
  finalOut=c(truth, bias11, bias11/truth, sd11, RMSE11, coverage_ps11, width11, dim(result)[1], sd11Boot)  
  names(finalOut)=c("truth", "bias", "biasPercent", "sd", "RMSE", "coverage", "widthCI", "num.sim", "sdBoot")  
  return(finalOut)
  
}


DIREC_ROOT="C:/Users/Tingting.Zhou/Desktop/paper2/resubmission/linear/"


#varying the value of gammaV for different degrees of overlap
sampleSize=500
gammaV=4

truthVal=rep(0.75, 7)
names(truthVal)=c("ATE", "ATM", "ATT", "ATC", "ATO", "truncate", "truncateQ")


DIRECOUT=paste0(DIREC_ROOT, "Results/")


###asymetric truncation, at quantile level
truncateVal=seq(0.01, 0.1, 0.01)
truncateQVal=seq(0, 0.03, 0.005)

#modelSpec="misPred"
#modelSpec="both"
#modelSpec="misPred2"

for(modelSpec in c("misPred", "both", "misWeight")){
  
  
  ###pencomp
  ATE_pencomp=NULL
  ATM_pencomp=NULL
  ATM_w_pencomp=NULL 
  ATO_pencomp=NULL
  ATT_pencomp=NULL
  ATT_w_pencomp=NULL
  ATC_pencomp=NULL
  ATC_w_pencomp=NULL
  
  truncate_pencomp=NULL
  truncateQ_pencomp=NULL
  
  
  ###weighted estimators
  ATE=NULL 
  ATE.aug=NULL
  ATM=NULL
  ATM.aug=NULL
  ATO=NULL
  ATO.aug=NULL
  ATT=NULL
  ATT.aug=NULL
  ATC=NULL
  ATC.aug=NULL
  
  truncate=NULL
  truncate.aug=NULL
  truncate_rest=NULL
  truncate.aug_rest=NULL
  
  truncateQ=NULL
  truncateQ.aug=NULL
  truncateQ_rest=NULL
  truncateQ.aug_rest=NULL
  
  
  
  DIRECR=NULL
  
  DIRECR=paste0(DIREC_ROOT, "homoT/")
  
  
  ###weighted estimator output
  ATE=rbind(ATE, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATE", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATE.aug=rbind(ATE.aug, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATE.aug", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATM=rbind(ATM, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATM", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATM.aug=rbind(ATM.aug, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATM.aug", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATO=rbind(ATO, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATO", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATO.aug=rbind(ATO.aug, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATO.aug", "_", modelSpec, ".txt", sep=""), header = T))
  
  truncate=rbind(truncate, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncate", "_", modelSpec, ".txt", sep=""), header = T))
  
  truncate.aug=rbind(truncate.aug, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncate.aug", "_", modelSpec, ".txt", sep=""), header = T))
  
  truncateQ=rbind(truncateQ, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncateQ", "_", modelSpec, ".txt", sep=""), header = T))
  
  truncateQ.aug=rbind(truncateQ.aug, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncateQ.aug", "_", modelSpec, ".txt", sep=""), header = T))
  
  truncate_rest=rbind(truncate_rest, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncate_rest", "_", modelSpec, ".txt", sep=""), header = T))
  
  truncate.aug_rest=rbind(truncate.aug_rest, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncate.aug_rest", "_", modelSpec, ".txt", sep=""), header = T))
  
  truncateQ_rest=rbind(truncateQ_rest, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncateQ_rest", "_", modelSpec, ".txt", sep=""), header = T))
  
  truncateQ.aug_rest=rbind(truncateQ.aug_rest, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncateQ.aug_rest", "_", modelSpec, ".txt", sep=""), header = T))
  
  
  ATT=rbind(ATT, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATT", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATT.aug=rbind(ATT.aug, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATT.aug", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATC=rbind(ATC, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATC", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATC.aug=rbind(ATC.aug, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATC.aug", "_", modelSpec, ".txt", sep=""), header = T))
  
  
  ###pencomp output
  ATE_pencomp=rbind(ATE_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATE_pencomp", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATM_pencomp=rbind(ATM_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATM_pencomp", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATM_w_pencomp=rbind(ATM_w_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATM_w_pencomp", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATO_pencomp=rbind(ATO_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATO_pencomp", "_", modelSpec, ".txt", sep=""), header = T))
  
  truncate_pencomp=rbind(truncate_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncate_pencomp", "_", modelSpec, ".txt", sep=""), header = T))
  
  truncateQ_pencomp=rbind(truncateQ_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncateQ_pencomp", "_", modelSpec, ".txt", sep=""),header = T))
  
  ATT_pencomp=rbind(ATT_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATT_pencomp", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATT_w_pencomp=rbind(ATT_w_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATT_w_pencomp", "_", modelSpec, ".txt", sep=""),header = T))
  
  ATC_pencomp=rbind(ATC_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATC_pencomp", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATC_w_pencomp=rbind(ATC_w_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATC_w_pencomp", "_", modelSpec, ".txt", sep=""),header = T))
  
  
  
  ###pencomp
  ATE_pencomp=ATE_pencomp[which(!is.na(ATE_pencomp[,1])),]
  ATM_pencomp=ATM_pencomp[which(!is.na(ATM_pencomp[,1])),]
  ATM_w_pencomp=ATM_w_pencomp[which(!is.na(ATM_w_pencomp[,1])),] 
  ATO_pencomp=ATO_pencomp[which(!is.na(ATO_pencomp[,1])),]
  ATT_pencomp=ATT_pencomp[which(!is.na(ATT_pencomp[,1])),]
  ATT_w_pencomp=ATT_w_pencomp[which(!is.na(ATT_w_pencomp[,1])),]
  ATC_pencomp=ATC_pencomp[which(!is.na(ATC_pencomp[,1])),]
  ATC_w_pencomp=ATC_w_pencomp[which(!is.na(ATC_w_pencomp[,1])),]
  
  ###pencomp
  dim(ATE_pencomp)
  dim(ATM_pencomp)
  dim(ATM_w_pencomp)
  dim(ATO_pencomp)
  dim(ATT_pencomp)
  dim(ATT_w_pencomp)
  dim(ATC_pencomp)
  dim(ATC_w_pencomp)
  
  
  truncate_pencomp=truncate_pencomp[which(!is.na(truncate_pencomp[,1])),]
  truncateQ_pencomp=truncateQ_pencomp[which(!is.na(truncateQ_pencomp[,1])),]
  
  dim(truncate_pencomp)
  dim(truncateQ_pencomp)
  
  
  ###weighted estimators
  ATE=ATE[which(!is.na(ATE[,1])),]
  ATE.aug=ATE.aug[which(!is.na(ATE.aug[,1])),]
  ATM=ATM[which(!is.na(ATM[,1])),]
  ATM.aug=ATM.aug[which(!is.na(ATM.aug[,1])),]
  ATO=ATO[which(!is.na(ATO[,1])),]
  ATO.aug=ATO.aug[which(!is.na(ATO.aug[,1])),]
  ATT=ATT[which(!is.na(ATT[,1])),]
  ATT.aug=ATT.aug[which(!is.na(ATT.aug[,1])),]
  ATC=ATC[which(!is.na(ATC[,1])),]
  ATC.aug=ATC.aug[which(!is.na(ATC.aug[,1])),]
  
  truncate=truncate[which(!is.na(truncate[,1])),]
  truncate.aug=truncate.aug[which(!is.na(truncate.aug[,1])),]
  truncate_rest=truncate_rest[which(!is.na(truncate_rest[,1])),]
  truncate.aug_rest=truncate.aug_rest[which(!is.na(truncate.aug_rest[,1])),]
  
  truncateQ=truncateQ[which(!is.na(truncateQ[,1])),]
  truncateQ.aug=truncateQ.aug[which(!is.na(truncateQ.aug[,1])),]
  truncateQ_rest=truncateQ_rest[which(!is.na(truncateQ_rest[,1])),]
  truncateQ.aug_rest=truncateQ.aug_rest[which(!is.na(truncateQ.aug_rest[,1])),]
  
  
  ###weighted estimators
  dim(ATE)
  dim(ATE.aug)
  dim(ATM)
  dim(ATM.aug)
  dim(ATO)
  dim(ATO.aug)
  dim(ATT)
  dim(ATT.aug)
  dim(ATC)
  dim(ATC.aug)
  
  dim(truncate)
  dim(truncate.aug)
  dim(truncate_rest)
  dim(truncate.aug_rest)
  
  dim(truncateQ)
  dim(truncateQ.aug)
  dim(truncateQ_rest)
  dim(truncateQ.aug_rest)
  
  
  
  truth=truthVal["ATE"]
  ATE_all=rbind(processResult(result=ATE, truth = truth),
                processResult(result=ATE.aug, truth),
                processResult(result=ATE_pencomp, truth = truth))
  
  ATE_all
  row.names(ATE_all)=c("ATE", "ATE aug",  "pencomp")
  
  
  ###########################
  
  truth=truthVal["ATM"]
  ATM_all=rbind(processResult(result=ATM, truth = truth),
                processResult(result=ATM.aug, truth),
                processResult(result=ATM_pencomp, truth = truth),
                processResult(result=ATM_w_pencomp, truth = truth) )
  
  ATM_all
  row.names(ATM_all)=c("ATM", "ATM aug", "pencomp ATM", "pencomp w ATM")
  
  
  
  ###########################
  
  truth=truthVal["ATO"]
  ATO_all=rbind(processResult(result=ATO, truth = truth),
                processResult(result=ATO.aug, truth),
                processResult(result=ATO_pencomp, truth = truth) )
  
  ATO_all
  row.names(ATO_all)=c("ATO", "ATO aug", "pencomp ATO")
  
  
  ###########################
  
  truth=truthVal["ATT"]
  ATT_all=rbind(processResult(result=ATT, truth = truth),
                processResult(result=ATT.aug, truth),
                processResult(result=ATT_pencomp, truth = truth),
                processResult(result=ATT_w_pencomp, truth = truth) )
  
  
  ATT_all
  row.names(ATT_all)=c("ATT", "ATT aug", "pencomp ATT", "pencomp w ATT")
  
  
  ###########################
  
  truth=truthVal["ATC"]
  ATC_all=rbind(processResult(result=ATC, truth = truth),
                processResult(result=ATC.aug, truth),
                processResult(result=ATC_pencomp, truth = truth),
                processResult(result=ATC_w_pencomp, truth = truth) )
  
  
  ATC_all
  row.names(ATC_all)=c("ATC", "ATC aug", "pencomp ATC", "pencomp w ATC")
  
  
  
  ###asymetric truncation, at quantile level
  truncateVal=seq(0.01, 0.1, 0.01)
  truncateQVal=seq(0, 0.03, 0.005)
  
  truncate_all=NULL
  for(k in 1:length(truncateVal)){
    
    selCol=paste0(c("estimate", "std", "lowerCI","upperCI"), format(truncateVal[k], digits = 2))
    temp=rbind(processResult(result=truncate[, selCol], truth = truth), 
               processResult(result=truncate_rest[, selCol], truth = truth), 
               processResult(result=truncate.aug[, selCol], truth = truth), 
               processResult(result=truncate.aug_rest[, selCol], truth = truth),
               processResult(result=truncate_pencomp[, selCol], truth = truth) )
    
    row.names(temp)=paste0(c("truncate", "truncate rest", "truncate.aug", "truncate.aug rest", "truncate pencomp"), 
                           format(truncateVal[k], digits = 2))
    
    truncate_all=rbind(truncate_all, temp)
    
  }
  
  ##################################
  truncateQ_all=NULL
  for(k in 1:length(truncateQVal)){
    selCol=paste0(c("estimate", "std", "lowerCI","upperCI"), format(truncateQVal[k], digits = 2))
    temp=rbind(processResult(result=truncateQ[, selCol], truth = truth), 
               processResult(result=truncateQ_rest[, selCol], truth = truth), 
               processResult(result=truncateQ.aug[, selCol], truth = truth), 
               processResult(result=truncateQ.aug_rest[, selCol], truth = truth),
               processResult(result=truncateQ_pencomp[, selCol], truth = truth) )
    
    row.names(temp)=paste0(c("truncateQ", "truncateQ rest", "truncateQ.aug", "truncateQ.aug rest", "truncateQ pencomp"), 
                           format(truncateQVal[k], digits = 2))
    
    truncateQ_all=rbind(truncateQ_all, temp)
  }
  
  
  
  output=rbind(ATE_all, ATM_all, ATO_all, ATT_all, ATC_all, truncate_all, truncateQ_all)
  
  write.table(output, paste0(DIRECOUT, modelSpec, "_gammaV", gammaV, "_sampleSize", sampleSize, ".txt"), sep="\t")
  
  
}




########### pencomp with spline only ##########
for(modelSpec in c("misPred2")){
  
  ###pencomp
  ATE_pencomp=NULL
  ATM_pencomp=NULL
  ATM_w_pencomp=NULL 
  ATO_pencomp=NULL
  ATT_pencomp=NULL
  ATT_w_pencomp=NULL
  ATC_pencomp=NULL
  ATC_w_pencomp=NULL
  
  truncate_pencomp=NULL
  truncateQ_pencomp=NULL
  
  DIRECR=NULL
  
  DIRECR=paste0(DIREC_ROOT, "homoT/")
  
  ###pencomp output
  ATE_pencomp=rbind(ATE_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATE_pencomp", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATM_pencomp=rbind(ATM_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATM_pencomp", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATM_w_pencomp=rbind(ATM_w_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATM_w_pencomp", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATO_pencomp=rbind(ATO_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATO_pencomp", "_", modelSpec, ".txt", sep=""), header = T))
  
  truncate_pencomp=rbind(truncate_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncate_pencomp", "_", modelSpec, ".txt", sep=""), header = T))
  
  truncateQ_pencomp=rbind(truncateQ_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/truncateQ_pencomp", "_", modelSpec, ".txt", sep=""),header = T))
  
  ATT_pencomp=rbind(ATT_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATT_pencomp", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATT_w_pencomp=rbind(ATT_w_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATT_w_pencomp", "_", modelSpec, ".txt", sep=""),header = T))
  
  ATC_pencomp=rbind(ATC_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATC_pencomp", "_", modelSpec, ".txt", sep=""), header = T))
  
  ATC_w_pencomp=rbind(ATC_w_pencomp, read.table(paste(DIRECR, "sampleSize", sampleSize, "/gamma", gammaV, "/ATC_w_pencomp", "_", modelSpec, ".txt", sep=""),header = T))
  
  ###pencomp
  ATE_pencomp=ATE_pencomp[which(!is.na(ATE_pencomp[,1])),]
  ATM_pencomp=ATM_pencomp[which(!is.na(ATM_pencomp[,1])),]
  ATM_w_pencomp=ATM_w_pencomp[which(!is.na(ATM_w_pencomp[,1])),] 
  ATO_pencomp=ATO_pencomp[which(!is.na(ATO_pencomp[,1])),]
  ATT_pencomp=ATT_pencomp[which(!is.na(ATT_pencomp[,1])),]
  ATT_w_pencomp=ATT_w_pencomp[which(!is.na(ATT_w_pencomp[,1])),]
  ATC_pencomp=ATC_pencomp[which(!is.na(ATC_pencomp[,1])),]
  ATC_w_pencomp=ATC_w_pencomp[which(!is.na(ATC_w_pencomp[,1])),]
  
  ###pencomp
  dim(ATE_pencomp)
  dim(ATM_pencomp)
  dim(ATM_w_pencomp)
  dim(ATO_pencomp)
  dim(ATT_pencomp)
  dim(ATT_w_pencomp)
  dim(ATC_pencomp)
  dim(ATC_w_pencomp)
  
  
  truncate_pencomp=truncate_pencomp[which(!is.na(truncate_pencomp[,1])),]
  truncateQ_pencomp=truncateQ_pencomp[which(!is.na(truncateQ_pencomp[,1])),]
  
  dim(truncate_pencomp)
  dim(truncateQ_pencomp)
  
  
  truth=truthVal["ATE"]
  ATE_all=rbind(processResult(result=ATE_pencomp, truth = truth))
  
  ATE_all
  row.names(ATE_all)=c("pencomp")
  
  
  ###########################
  
  truth=truthVal["ATM"]
  ATM_all=rbind(processResult(result=ATM_pencomp, truth = truth),
                processResult(result=ATM_w_pencomp, truth = truth) )
  
  ATM_all
  row.names(ATM_all)=c( "pencomp ATM", "pencomp w ATM")
  
  ###########################
  
  truth=truthVal["ATO"]
  ATO_all=rbind(processResult(result=ATO_pencomp, truth = truth))
  
  ATO_all
  row.names(ATO_all)=c("pencomp ATO")
  
  
  ###########################
  
  truth=truthVal["ATT"]
  ATT_all=rbind(processResult(result=ATT_pencomp, truth = truth),
                processResult(result=ATT_w_pencomp, truth = truth) )
  
  
  ATT_all
  row.names(ATT_all)=c( "pencomp ATT", "pencomp w ATT")
  
  
  ###########################
  
  truth=truthVal["ATC"]
  ATC_all=rbind(processResult(result=ATC_pencomp, truth = truth),
                processResult(result=ATC_w_pencomp, truth = truth) )
  
  
  ATC_all
  row.names(ATC_all)=c( "pencomp ATC", "pencomp w ATC")
  
  
  ###asymetric truncation, at quantile level
  truncateVal=seq(0.01, 0.1, 0.01)
  truncateQVal=seq(0, 0.03, 0.005)
  
  truncate_all=NULL
  for(k in 1:length(truncateVal)){
    
    selCol=paste0(c("estimate", "std", "lowerCI","upperCI"), format(truncateVal[k], digits = 2))
    temp=rbind(processResult(result=truncate_pencomp[, selCol], truth = truth) )
    
    row.names(temp)=paste0(c("truncate pencomp"), format(truncateVal[k], digits = 2))
    
    truncate_all=rbind(truncate_all, temp)
    
  }
  
  ##################################
  truncateQ_all=NULL
  for(k in 1:length(truncateQVal)){
    selCol=paste0(c("estimate", "std", "lowerCI","upperCI"), format(truncateQVal[k], digits = 2))
    temp=rbind(processResult(result=truncateQ_pencomp[, selCol], truth = truth) )
    
    row.names(temp)=paste0(c( "truncateQ pencomp"), 
                           format(truncateQVal[k], digits = 2))
    
    truncateQ_all=rbind(truncateQ_all, temp)
  }
  
  
  
  output=rbind(ATE_all, ATM_all, ATO_all, ATT_all, ATC_all, truncate_all, truncateQ_all)
  
  write.table(output, paste0(DIRECOUT, modelSpec, "_gammaV", gammaV, "_sampleSize", sampleSize, ".txt"), sep="\t")
  
  
}


#################################################################################
###############output results in tables #########################################

#varying the value of gammaV for different degrees of overlap
sampleSize=500
gammaV=4


DIRECOUT=paste0(DIREC_ROOT, "Results/")

misPred2=read.table(paste0(DIRECOUT, "misPred2", "_gammaV", gammaV, "_sampleSize", sampleSize,  ".txt"), sep="\t")
misPred=read.table(paste0(DIRECOUT, "misPred", "_gammaV", gammaV, "_sampleSize", sampleSize,  ".txt"), sep="\t")
both=read.table(paste0(DIRECOUT, "both", "_gammaV", gammaV, "_sampleSize", sampleSize,  ".txt"), sep="\t")
misWeight=read.table(paste0(DIRECOUT, "misWeight", "_gammaV", gammaV, "_sampleSize", sampleSize,  ".txt"), sep="\t")

########change non-coverage rate
noncoverage=function(data, var.name="coverage"){
  data[,"noncoverage"] =format(100*(1-data[, var.name]), digits = 2)
  return(data)
}


########empirical RMSE relative to correct IPTW (including everyone)
relRMSE=function(data, var.name="RMSE", bench=both["ATE", "RMSE"]){
  data[,"relRMSE"] = format(abs(data[, var.name] / bench), digits = 2)
  return(data)
}


########multiple bias by 1000
biasT=function(data, var.name="bias"){
  data[,"biasT"] =format(abs(data[, var.name])*1000, digits = 0)
  return(data)
}

########multiple bias percentage by 100
biasPer=function(data, var.name="biasPercent"){
  data[,"biasPer"] =format(abs(data[, var.name]) * 100, digits = 0)
  return(data)
}


misPred=relRMSE(misPred, var.name = "RMSE", bench = both["ATE", "RMSE"])
misPred2=relRMSE(misPred2, var.name = "RMSE", bench = both["ATE", "RMSE"])
both=relRMSE(both, var.name = "RMSE", bench = both["ATE", "RMSE"])
misWeight=relRMSE(misWeight, var.name = "RMSE", bench = both["ATE", "RMSE"])

misPred=noncoverage(misPred, var.name = "coverage")
misPred2=noncoverage(misPred2, var.name = "coverage")
both=noncoverage(both, var.name = "coverage")
misWeight=noncoverage(misWeight, var.name = "coverage")


misPred=biasT(misPred, var.name = "bias")
misPred2=biasT(misPred2, var.name = "bias")
both=biasT(both, var.name = "bias")
misWeight=biasT(misWeight, var.name = "bias")


misPred=biasPer(misPred, var.name="biasPercent")
misPred2=biasPer(misPred2, var.name="biasPercent")
both=biasPer(both, var.name="biasPercent")
misWeight=biasPer(misWeight, var.name="biasPercent")


#################
methods=c("ATE", "ATE aug" , "pencomp" ,"ATM"  , "ATM aug" ,               
          "pencomp ATM", "pencomp w ATM", "ATO" ,  "ATO aug" , "pencomp ATO", "ATT",   "ATT aug", 
          "pencomp ATT", "pencomp w ATT" , "ATC"  ,                  
          "ATC aug" , "pencomp ATC" , "pencomp w ATC",
          
          "truncate0.01", "truncate rest0.01","truncate0.05", "truncate rest0.05",
          "truncate.aug0.01", "truncate.aug rest0.01","truncate.aug0.05", "truncate.aug rest0.05",
          "truncate pencomp0.01", "truncate pencomp0.05",
          
          "truncateQ0", "truncateQ rest0","truncateQ0.005", "truncateQ rest0.005",
          "truncateQ.aug0", "truncateQ.aug rest0","truncateQ.aug0.005", "truncateQ.aug rest0.005",
          "truncateQ pencomp0", "truncateQ pencomp0.005")

#View(misPred[which(row.names(misPred) %in% methods),])

methods2 = c("pencomp", "pencomp ATM", "pencomp w ATM", "pencomp ATO" , "pencomp ATT", "pencomp w ATT" ,        
             "pencomp ATC" , "pencomp w ATC" , "truncate pencomp0.01", "truncate pencomp0.05", "truncateQ pencomp0" ,
             "truncateQ pencomp0.005") 


########select only the methods listed above###########
both=both[which(row.names(both) %in% methods), ]
misPred=misPred[which(row.names(misPred) %in% methods), ]
misPred2=misPred2[which(row.names(misPred2) %in% methods2), ]
misWeight=misWeight[which(row.names(misWeight) %in% methods), ]

row.names(both)==row.names(misPred)
row.names(misPred)==row.names(misWeight)


########## both models are correctly specified###########
n=nrow(both)
bothResult=cbind(rep("&", n), format(both[, "truth"]*1000, digits = 0), rep("&", n), both[, "biasT"], rep("&", n), both[, "biasPer"],  rep("&", n), both[, "relRMSE"],
                 rep("&", n), both[, "noncoverage"], rep("\\\\", n))

bothResult=cbind(row.names(both), bothResult)


write.table(bothResult, paste0(DIRECOUT, "both.txt"), row.names = FALSE, col.names = FALSE, quote = FALSE)



########## misspecified propensity score model###########
n=nrow(misWeight)
misWeightResult=cbind(rep("&", n), format(misWeight[, "truth"]*1000, digits = 0), rep("&", n), misWeight[, "biasT"], rep("&", n), misWeight[, "biasPer"], 
                      rep("&", n), misWeight[, "relRMSE"],
                      rep("&", n), misWeight[, "noncoverage"], rep("\\\\", n))

misWeightResult=cbind(row.names(misWeight), misWeightResult)


write.table(misWeightResult, paste0(DIRECOUT, "misWeight.txt"), row.names = FALSE, col.names = FALSE, quote = FALSE)





#################################################################################
###############output results in tables #########################################


########## misspecified prediction model ###########
misPredResult=rbind(misPred["ATE",], misPred["ATE aug",], misPred["pencomp",], misPred2["pencomp",],
                    
                    misPred["ATM",], misPred["ATM aug",], misPred["pencomp ATM",], misPred["pencomp w ATM",],
                    misPred2["pencomp ATM",], misPred2["pencomp w ATM",],
                    
                    misPred["ATO",], misPred["ATO aug",], misPred["pencomp ATO",],
                    misPred2["pencomp ATO",],
                    
                    misPred["ATT",], misPred["ATT aug",], misPred["pencomp ATT",], misPred["pencomp w ATT",],
                    misPred2["pencomp ATT",], misPred2["pencomp w ATT",],
                    
                    misPred["ATC",], misPred["ATC aug",], misPred["pencomp ATC",], misPred["pencomp w ATC",],
                    misPred2["pencomp ATC",], misPred2["pencomp w ATC",],
                    
                    misPred["truncate0.01",], misPred["truncate rest0.01",], misPred["truncate.aug0.01",], misPred["truncate.aug rest0.01",],
                    misPred["truncate pencomp0.01",],
                    misPred2["truncate pencomp0.01",],
                    
                    misPred["truncate0.05",], misPred["truncate rest0.05",], misPred["truncate.aug0.05",], misPred["truncate.aug rest0.05",],
                    misPred["truncate pencomp0.05",],
                    misPred2["truncate pencomp0.05",],
                    
                    misPred["truncateQ0",], misPred["truncateQ rest0",], misPred["truncateQ.aug0",], misPred["truncateQ.aug rest0",],
                    misPred["truncateQ pencomp0",],
                    misPred2["truncateQ pencomp0",],
                    
                    misPred["truncateQ0.005",], misPred["truncateQ rest0.005",], misPred["truncateQ.aug0.005",], misPred["truncateQ.aug rest0.005",],
                    misPred["truncateQ pencomp0.005",],
                    misPred2["truncateQ pencomp0.005",] )


n=nrow(misPredResult)
misPredResult2=cbind(rep("&", n), format(misPredResult[, "truth"]*1000, digits = 0), rep("&", n), misPredResult[, "biasT"], rep("&", n), misPredResult[, "biasPer"], 
                     rep("&", n), misPredResult[, "relRMSE"],
                     rep("&", n), misPredResult[, "noncoverage"], rep("\\\\", n))


misPredResult2=cbind(row.names(misPredResult), misPredResult2)


write.table(misPredResult2, paste0(DIRECOUT, "misPred.txt"), quote = F, row.names = F, col.names = F)





############################################### plots ##################################################
############3 for truncated estimands ##################################################################
#rm(list=ls())



pdf(paste0(DIREC_ROOT, "Results/linear_homo.pdf"))

###asymetric truncation, at quantile level
truncateVal=seq(0.01, 0.1, 0.01)
truncateQVal=seq(0, 0.03, 0.005)

gammaV=4
sampleSize=500

DIRECOUT=paste0(DIREC_ROOT, "Results/") ##ouput directory


varName="RMSE"
#varName="absBias"

yrange=c(0.05, 0.7)

################ plots ##################
#par(mfrow=c(2, 3))

### for mispred2 model specification, pencomp estimate
modelSpec="misPred2"
output500=read.table(paste0(DIRECOUT, modelSpec, "_gammaV", gammaV, "_sampleSize", sampleSize, ".txt"), sep="\t")
output500$absBias=abs(output500$bias)

###for weighted estimators
modelSpec="misPred"
output500_w=read.table(paste0(DIRECOUT, modelSpec, "_gammaV", gammaV, "_sampleSize", sampleSize, ".txt"), sep="\t")
output500_w$absBias=abs(output500_w$bias)


##########################
###truncate################
rowSel=paste0(c("truncate"), truncateVal)
b=1:length(truncateVal)
plot(b, output500_w[rowSel,varName], type="o", xlab = "truncation level", ylim = yrange, xaxt="n", ylab = varName, 
     main="RMSE: Truncate estimand", lty=1, col="cyan")

#########
rowSel=paste0(c("truncate rest"), truncateVal)
lines(b, output500_w[rowSel,varName], type="o", col="cyan4", lty=2)

########
rowSel=paste0(c("truncate pencomp"), truncateVal)
lines(b, output500[rowSel,varName], type="o", col="red", lty=5)


axis(1, at=b, labels=paste0(truncateVal),  las=1)
legend("topright", legend=c("truncate", "truncate rest",  "truncate pencomp"),
       col=c("cyan","cyan4",  "red"), lty=c(1, 2, 5), cex=0.8)




###################################
############# bias ################
varName="absBias"
yrange=c(0, 0.1)



##########################
###truncate################
rowSel=paste0(c("truncate"), truncateVal)
b=1:length(truncateVal)
plot(b, output500_w[rowSel,varName], type="o", xlab = "truncation level", ylim = yrange, xaxt="n", ylab = varName, 
     main="Absolute Bias: Truncate estimand", lty=1, col="cyan")

#########
rowSel=paste0(c("truncate rest"), truncateVal)
lines(b, output500_w[rowSel,varName], type="o", col="cyan4", lty=2)

########
rowSel=paste0(c("truncate pencomp"), truncateVal)
lines(b, output500[rowSel,varName], type="o", col="red", lty=5)


axis(1, at=b, labels=paste0(truncateVal),  las=1)
legend("topright", legend=c("truncate", "truncate rest",  "truncate pencomp"),
       col=c("cyan","cyan4",  "red"), lty=c(1, 2, 5), cex=0.8)





############################################### plots ##################################################
############3 for truncated estimands at quantile #################################################################

varName="RMSE"
yrange=c(0.05, 1)

##########################
###truncate################
rowSel=paste0(c("truncateQ"), truncateQVal)
b=1:length(truncateQVal)
plot(b, output500_w[rowSel,varName], type="o", xlab = "truncation level", ylim = yrange, xaxt="n", ylab = varName, 
     main="RMSE TruncateQ estimand", lty=1, col="cyan")

#########
rowSel=paste0(c("truncateQ rest"), truncateQVal)
lines(b, output500_w[rowSel,varName], type="o", col="cyan4", lty=2)

########
rowSel=paste0(c("truncateQ pencomp"), truncateQVal)
lines(b, output500[rowSel,varName], type="o", col="red", lty=5)


axis(1, at=b, labels=paste0(truncateQVal),  las=1)
legend("topright", legend=c("truncateQ", "truncateQ rest",  "truncateQ pencomp"),
       col=c("cyan","cyan4",  "red"), lty=c(1, 2, 5), cex=0.8)




#################absolute empirical bias#############################

varName="absBias"
yrange=c(0, 0.5)


##########################
###truncate################
rowSel=paste0(c("truncateQ"), truncateQVal)
b=1:length(truncateQVal)
plot(b, output500_w[rowSel,varName], type="o", xlab = "truncation level", ylim = yrange, xaxt="n", ylab = varName, 
     main="Absolute Bias TruncateQ estimand", lty=1, col="cyan")

#########
rowSel=paste0(c("truncateQ rest"), truncateQVal)
lines(b, output500_w[rowSel,varName], type="o", col="cyan4", lty=2)

########
rowSel=paste0(c("truncateQ pencomp"), truncateQVal)
lines(b, output500[rowSel,varName], type="o", col="red", lty=5)


axis(1, at=b, labels=paste0(truncateQVal),  las=1)
legend("topright", legend=c("truncateQ", "truncateQ rest",  "truncateQ pencomp"),
       col=c("cyan","cyan4",  "red"), lty=c(1, 2, 5), cex=0.8)


dev.off()
