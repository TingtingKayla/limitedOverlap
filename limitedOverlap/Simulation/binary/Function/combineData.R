
rm(list=ls())


#varying the value of gammaV for different degrees of overlap
sampleSize=500
gammaV=4

DIRECOUT="C:/Users/Tingting.Zhou/Desktop/paper2/resubmission/binary/homoT/"

#modelSpec="misPred"
#modelSpec="both"
#modelSpec="misPred2"


for(modelSpec in c("misPred", "misPred2", "both")){

  
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
  
  
  for(k in 1:2){
    
    DIRECR=NULL
    
    if(k==1){
      DIRECR="C:/Users/Tingting.Zhou/Desktop/paper2/resubmission/binary/homoT2/"  ###simulation result
    } else if(k==2){
      DIRECR="C:/Users/Tingting.Zhou/Desktop/paper2/resubmission/binary/homoT_copy/"  ###simulation result
    }
    
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
    
  }
  
  
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
  
  
  ###weighted estimator output
  write.table(ATE, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/ATE", "_", modelSpec, ".txt", sep=""),
              row.names = F, col.names=T, quote = F)
  write.table(ATE.aug, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/ATE.aug", "_", modelSpec, ".txt", sep=""), row.names = F, 
              col.names=T, quote = F)
  
  write.table(ATM, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/ATM", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=T, quote = F)
  write.table(ATM.aug, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/ATM.aug", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=T, quote = F)
  
  write.table(ATO, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/ATO", "_", modelSpec, ".txt", sep=""), row.names = F, 
              col.names=T, quote = F)
  write.table(ATO.aug, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/ATO.aug", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=T, quote = F)
  

  write.table(truncate, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/truncate", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=T, quote = F)
  write.table(truncate.aug, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/truncate.aug", "_", modelSpec, ".txt", sep=""), row.names = F, 
              col.names=T, quote = F)
  
  write.table(truncateQ, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/truncateQ", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=T, quote = F)
  write.table(truncateQ.aug, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/truncateQ.aug", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=T, quote = F)
  
  
  #####re-estimate the propensity score
  write.table(truncate_rest, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/truncate_rest", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=T, quote = F)
  write.table(truncate.aug_rest, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/truncate.aug_rest", "_", modelSpec, ".txt", sep=""), row.names = F, 
              col.names=T, quote = F)
  
  write.table(truncateQ_rest, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/truncateQ_rest", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=T, quote = F)
  write.table(truncateQ.aug_rest, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/truncateQ.aug_rest", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=T, quote = F)
  
  
  write.table(ATT, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/ATT", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=T, quote = F)
  write.table(ATT.aug, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/ATT.aug", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=T, quote = F)
  
  write.table(ATC, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/ATC", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=T, quote = F)
  write.table(ATC.aug, paste(DIRECOUT, "sampleSize", sampleSize, "/gamma", gammaV, "/ATC.aug", "_", modelSpec, ".txt", sep=""), row.names = F,
              col.names=T, quote = F)
  

  
}




