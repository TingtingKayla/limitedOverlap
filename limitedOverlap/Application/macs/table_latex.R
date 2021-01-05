
rm(list=ls())

##################################################################################################################
##################################################################################################################
#######store the raw results###############
DIREC="C:/Users/Tingting.Zhou/Desktop/paper2/Application/macs/"

###load function
source(paste(DIREC, "combineFunc.R", sep="")) 

modelSpec="all"
num.knot=20

truncateVal=seq(0.01, 0.1, 0.01)
truncateQVal=seq(0, 0.03, 0.005)

column.names2=paste0("truncate", truncateVal) 
column.names3=paste0("truncateQ", truncateQVal) 

DIREC_R="Results/"

modelSpec="all"
pencompEst=read.table(paste(DIREC, DIREC_R,"pencompEst", "_", modelSpec, "_numknot", num.knot,".txt", sep=""), sep="\t")
pencompEst.truncate=read.table(paste(DIREC, DIREC_R, "pencompEst.truncate", "_", modelSpec, "_numknot", num.knot, ".txt", sep=""), sep="\t")
pencompEst.truncateQ=read.table(paste(DIREC, DIREC_R, "pencompEst.truncateQ", "_", modelSpec, "_numknot", num.knot, ".txt", sep=""), sep="\t")


weightEst=read.table(paste(DIREC, DIREC_R, "weightEst", "_", modelSpec, ".txt", sep=""), sep="\t")
weightEst.truncate.rest=read.table(paste(DIREC, DIREC_R, "weightEst.truncate.rest", "_", modelSpec, ".txt", sep=""), sep="\t")
weightEst.truncate.aug.rest=read.table(paste(DIREC, DIREC_R, "weightEst.truncate.aug.rest", "_", modelSpec, ".txt", sep=""), sep="\t")
weightEst.truncateQ.rest=read.table(paste(DIREC, DIREC_R, "weightEst.truncateQ.rest", "_", modelSpec, ".txt", sep=""),  sep="\t")
weightEst.truncateQ.aug.rest=read.table(paste(DIREC, DIREC_R, "weightEst.truncateQ.aug.rest", "_", modelSpec, ".txt", sep=""), sep="\t")


weightEst.truncate=read.table(paste(DIREC, DIREC_R, "weightEst.truncate", "_", modelSpec, ".txt", sep=""), sep="\t")
weightEst.truncate.aug=read.table(paste(DIREC, DIREC_R, "weightEst.truncate.aug", "_", modelSpec, ".txt", sep=""), sep="\t")
weightEst.truncateQ=read.table(paste(DIREC, DIREC_R, "weightEst.truncateQ", "_", modelSpec, ".txt", sep=""),  sep="\t")
weightEst.truncateQ.aug=read.table(paste(DIREC, DIREC_R, "weightEst.truncateQ.aug", "_", modelSpec, ".txt", sep=""), sep="\t")


pencompEst=rbind(pencompEst, pencompEst.truncate, pencompEst.truncateQ)
row.names(pencompEst)=paste0("pencomp ", row.names(pencompEst))
pencompEst$lengthCI=pencompEst[,4]-pencompEst[,3]


row.names(weightEst)=c("ATE", "ATE aug", "ATM" , "ATM aug", "ATT",  "ATT aug" ,"ATC", "ATC aug" ,"ATO", "ATO aug")


row.names(weightEst.truncate.aug)=paste0(row.names(weightEst.truncate.aug.rest), " aug")
row.names(weightEst.truncateQ.aug)=paste0(row.names(weightEst.truncateQ.aug.rest), " aug")

row.names(weightEst.truncate.aug.rest)=paste0(row.names(weightEst.truncate.aug.rest), " aug rest")
row.names(weightEst.truncateQ.aug.rest)=paste0(row.names(weightEst.truncateQ.aug.rest), " aug rest")

row.names(weightEst.truncate.rest)=paste0(row.names(weightEst.truncate.rest), " rest")
row.names(weightEst.truncateQ.rest)=paste0(row.names(weightEst.truncateQ.rest), " rest")


weightEst=rbind(weightEst, weightEst.truncate.rest, weightEst.truncate.aug.rest, weightEst.truncateQ.rest, weightEst.truncateQ.aug.rest,
                weightEst.truncate, weightEst.truncate.aug, weightEst.truncateQ, weightEst.truncateQ.aug )

weightEst$lengthCI=weightEst[,4]-weightEst[,3]


#################
methods=c("ATE", "ATE aug" , "pencomp ATE" , 
          "ATM"  , "ATM aug", "pencomp ATM",
          
          "ATO" ,  "ATO aug" , "pencomp ATO", 
          "ATT",   "ATT aug", "pencomp ATT", 
          "ATC", "ATC aug" , "pencomp ATC" , 
          
          "truncate0.01", "truncate0.01 rest","truncate0.01 aug", "truncate0.01 aug rest", "pencomp truncate0.01", 
          "truncate0.05", "truncate0.05 rest","truncate0.05 aug", "truncate0.05 aug rest", "pencomp truncate0.05",
          
          "truncateQ0", "truncateQ0 rest","truncateQ0 aug", "truncateQ0 aug rest", "pencomp truncateQ0", 
          "truncateQ0.005", "truncateQ0.005 rest","truncateQ0.005 aug", "truncateQ0.005 aug rest", "pencomp truncateQ0.005"
          )

pencompEst=pencompEst[which(row.names(pencompEst) %in% methods),]
weightEst=weightEst[which(row.names(weightEst) %in% methods),]

allResult=rbind(pencompEst, weightEst)

allTable=c(-7.21, 0.46, 1.82)
for(k in 1:length(methods)){
  allTable=rbind(allTable, allResult[which(row.names(allResult)==methods[k]),c(1, 2, 5)])
}



########## both models are correctly specified###########
n=nrow(allTable)
bothResult=cbind(rep("&", n), round(allTable[, 1], digits = 0), rep("&", n), round(allTable[, 2], digits = 2), rep("&", n), 
                 round(allTable[, 3], digits = 2),
                 rep("\\\\", n))


bothResult=cbind(c("unadjusted", methods), bothResult)


write.table(bothResult, "C:/Users/Tingting.Zhou/Desktop/paper2/Application/macs/tableLatex_all.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)






