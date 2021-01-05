
rm(list=ls())

##################################################################################################################
##################################################################################################################
#######store the raw results###############
DIREC="C:/Users/Tingting.Zhou/Desktop/paper2/Application/rightHeart/"

###load function
source(paste(DIREC, "combineFunc.R", sep="")) 

modelSpec="all"
num.knot=30

truncateVal=seq(0.01, 0.1, 0.01)
truncateQVal=seq(0, 0.03, 0.005)

column.names2=paste0("truncate", truncateVal) 
column.names3=paste0("truncateQ", truncateQVal) 

#########################
DIREC_R="Results_rev2/"  ###folder where results for sample 1-200 are stored
ATE_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATE.txt", sep=""), header=T, sep="\t")
ATM_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATM_raw.txt", sep=""),header = T, sep="\t")
ATM_w_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATM_w.txt", sep=""),header = T, sep="\t")
ATO_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATO.txt", sep=""),header = T, sep="\t")
ATT_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATT.txt", sep=""),header = T, sep="\t")
ATT_w_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATT_w.txt", sep=""),header = T, sep="\t")
ATC_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATC.txt", sep=""),header = T, sep="\t")
ATC_w_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATC_w.txt", sep=""),header = T, sep="\t")


#######################
DIREC_R="Results_rev3/"  ###folder where results where sample 201-500 are stored
ATE_raw=rbind(ATE_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATE.txt", sep=""), header=T, sep="\t"))
ATM_raw=rbind(ATM_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATM_raw.txt", sep=""),header = T, sep="\t"))
ATM_w_raw=rbind(ATM_w_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATM_w.txt", sep=""),header = T, sep="\t"))
ATO_raw=rbind(ATO_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATO.txt", sep=""),header = T, sep="\t"))
ATT_raw=rbind(ATT_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATT.txt", sep=""),header = T, sep="\t"))
ATT_w_raw=rbind(ATT_w_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATT_w.txt", sep=""),header = T, sep="\t"))
ATC_raw=rbind(ATC_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATC.txt", sep=""),header = T, sep="\t"))
ATC_w_raw=rbind(ATC_w_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATC_w.txt", sep=""),header = T, sep="\t"))



#######################
DIREC_R="Results_rev4/"  ###folder where results where sample 501-1000 are stored
ATE_raw=rbind(ATE_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATE.txt", sep=""), header=T, sep="\t"))
ATM_raw=rbind(ATM_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATM_raw.txt", sep=""),header = T, sep="\t"))
ATM_w_raw=rbind(ATM_w_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATM_w.txt", sep=""),header = T, sep="\t"))
ATO_raw=rbind(ATO_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATO.txt", sep=""),header = T, sep="\t"))
ATT_raw=rbind(ATT_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATT.txt", sep=""),header = T, sep="\t"))
ATT_w_raw=rbind(ATT_w_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATT_w.txt", sep=""),header = T, sep="\t"))
ATC_raw=rbind(ATC_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATC.txt", sep=""),header = T, sep="\t"))
ATC_w_raw=rbind(ATC_w_raw, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATC_w.txt", sep=""),header = T, sep="\t"))



ATE_raw=ATE_raw[which(!is.na(ATE_raw[,1])),]
ATM_raw=ATM_raw[which(!is.na(ATM_raw[,1])),]
ATM_w_raw=ATM_w_raw[which(!is.na(ATM_w_raw[,1])),]
ATO_raw=ATO_raw[which(!is.na(ATO_raw[,1])),]
ATT_raw=ATT_raw[which(!is.na(ATT_raw[,1])),]
ATT_w_raw=ATT_w_raw[which(!is.na(ATT_w_raw[,1])),]
ATC_raw=ATC_raw[which(!is.na(ATC_raw[,1])),]
ATC_w_raw=ATC_w_raw[which(!is.na(ATC_w_raw[,1])),]


dim(ATE_raw)
dim(ATM_raw)
dim(ATM_w_raw)
dim(ATO_raw)
dim(ATT_raw)
dim(ATT_w_raw)
dim(ATC_raw)
dim(ATC_w_raw)



############ ATE raw ##################
ATE=processPENCOMP(resultIn=ATE_raw)[1,]


############ ATM raw ##################
ATM=processPENCOMP(resultIn=ATM_raw)[1,]


############ ATM weights raw ##################
ATM_w=processPENCOMP(resultIn=ATM_w_raw)[1,]


############ ATO raw ##################
ATO=processPENCOMP(resultIn=ATO_raw)[1,]


############ ATT raw ##################
ATT=processPENCOMP(resultIn=ATT_raw)[1,]


############ ATT_w raw ##################
ATT_w=processPENCOMP(resultIn=ATT_w_raw)[1,]


############ ATC raw ##################
ATC=processPENCOMP(resultIn=ATC_raw)[1,]

############ ATT_w raw ##################
ATC_w=processPENCOMP(resultIn=ATC_w_raw)[1,]


#####################################################
truncate=rep(NA, 10)
for(g in 1:length(column.names2)){
  
  DIREC_R="Results_rev2/"  ###folder where results are stored
  temp=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst.truncate", "_", modelSpec, "_numknot", num.knot, "_",column.names2[g], ".txt", sep=""),header = T, sep="\t")
  
  DIREC_R="Results_rev3/"  ###folder where results are stored
  temp=rbind(temp, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst.truncate", "_", modelSpec, "_numknot", num.knot, "_",column.names2[g], ".txt", sep=""),
                              header = T, sep="\t"))
  
  temp=temp[which(!is.na(temp[,1])),]
  truncate=rbind(truncate, processPENCOMP(resultIn=temp)[1,])

}

truncate=truncate[-c(1),]
row.names(truncate)=paste0("pencomp ", column.names2)



#######################################################
truncateQ=rep(NA, 10)
for(g in 1:length(column.names3)){
  
  DIREC_R="Results_rev2/"  ###folder where results are stored
  temp=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst.truncateQ", "_", modelSpec, "_numknot", num.knot, "_",column.names3[g], ".txt", sep=""),header = T, sep="\t")
  
  DIREC_R="Results_rev3/"  ###folder where results are stored
  temp=rbind(temp, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst.truncateQ", "_", modelSpec, "_numknot", num.knot, "_",column.names3[g], ".txt", sep=""),
                              header = T, sep="\t"))
  
  temp=temp[which(!is.na(temp[,1])),]
  truncateQ=rbind(truncateQ, processPENCOMP(resultIn=temp)[1,])
  
}

truncateQ=truncateQ[-c(1),]
row.names(truncateQ)=paste0("pencomp ", column.names3)


##### combine the results ##############
pencompEst=rbind(ATE, ATM, ATM_w, ATO, ATT, ATT_w, ATC, ATC_w)
row.names(pencompEst)=paste0("pencomp ", c("ATE",  "ATM", "ATM w" ,"ATO" ,  "ATT"  , "ATT w", "ATC"  , "ATC w"))

pencompEst=rbind(pencompEst, truncate, truncateQ)



##############################################################################
##############################################################################
############ pencomp without any covariates in the outcome model #############

modelSpec="null"

#########################
DIREC_R="Results_rev4/"  ###folder where results for sample 1-200 are stored
ATE_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATE.txt", sep=""), header=T, sep="\t")
ATM_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATM_raw.txt", sep=""),header = T, sep="\t")
ATM_w_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATM_w.txt", sep=""),header = T, sep="\t")
ATO_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATO.txt", sep=""),header = T, sep="\t")
ATT_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATT.txt", sep=""),header = T, sep="\t")
ATT_w_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATT_w.txt", sep=""),header = T, sep="\t")
ATC_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATC.txt", sep=""),header = T, sep="\t")
ATC_w_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATC_w.txt", sep=""),header = T, sep="\t")


dim(ATE_raw)
dim(ATM_raw)
dim(ATM_w_raw)
dim(ATO_raw)
dim(ATT_raw)
dim(ATT_w_raw)
dim(ATC_raw)
dim(ATC_w_raw)



############ ATE raw ##################
ATE=processPENCOMP(resultIn=ATE_raw)[1,]


############ ATM raw ##################
ATM=processPENCOMP(resultIn=ATM_raw)[1,]


############ ATM weights raw ##################
ATM_w=processPENCOMP(resultIn=ATM_w_raw)[1,]


############ ATO raw ##################
ATO=processPENCOMP(resultIn=ATO_raw)[1,]


############ ATT raw ##################
ATT=processPENCOMP(resultIn=ATT_raw)[1,]


############ ATT_w raw ##################
ATT_w=processPENCOMP(resultIn=ATT_w_raw)[1,]


############ ATC raw ##################
ATC=processPENCOMP(resultIn=ATC_raw)[1,]

############ ATT_w raw ##################
ATC_w=processPENCOMP(resultIn=ATC_w_raw)[1,]


#####################################################
truncate=rep(NA, 10)
for(g in 1:length(column.names2)){
  
  DIREC_R="Results_rev4/"  ###folder where results are stored
  temp=rbind(temp, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst.truncate", "_", modelSpec, "_numknot", num.knot, "_",column.names2[g], ".txt", sep=""),
                              header = T, sep="\t"))
  
  temp=temp[which(!is.na(temp[,1])),]
  truncate=rbind(truncate, processPENCOMP(resultIn=temp)[1,])
  
}

truncate=truncate[-c(1),]
row.names(truncate)=paste0("pencomp null ", column.names2)



#######################################################
truncateQ=rep(NA, 10)
for(g in 1:length(column.names3)){
  
  DIREC_R="Results_rev4/"  ###folder where results are stored
  temp=rbind(temp, read.table(paste(DIREC, DIREC_R, "/raw/pencompEst.truncateQ", "_", modelSpec, "_numknot", num.knot, "_",column.names3[g], ".txt", sep=""),
                              header = T, sep="\t"))
  
  temp=temp[which(!is.na(temp[,1])),]
  truncateQ=rbind(truncateQ, processPENCOMP(resultIn=temp)[1,])
  
}

truncateQ=truncateQ[-c(1),]
row.names(truncateQ)=paste0("pencomp null ", column.names3)


##### combine the results ##############
pencompEst.null=rbind(ATE, ATM, ATM_w, ATO, ATT, ATT_w, ATC, ATC_w)
row.names(pencompEst.null)=paste0("pencomp null ", c("ATE",  "ATM", "ATM w" ,"ATO" ,  "ATT"  , "ATT w", "ATC"  , "ATC w"))

pencompEst.null=rbind(pencompEst.null, truncate, truncateQ)










#################################################################################
#################################################################################
###### weighted estimators ####################################################
repNum=1000
modelSpec="all"

#################################
DIREC_R="Results_rev2/"  ###folder where results where sample 1-500 are stored
ATE_raw=read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATE.txt", sep=""), header = T, sep="\t")
ATE_aug_raw=read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATE_aug.txt", sep=""),header=T,  sep="\t")
ATM_raw=read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATM.txt", sep=""), header = T, sep="\t")
ATM_aug_raw=read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATM_aug.txt", sep=""), header = T, sep="\t")
ATT_raw=read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATT.txt", sep=""), header = T, sep="\t")
ATT_aug_raw=read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATT_aug.txt", sep=""), header = T, sep="\t")
ATC_raw=read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATC.txt", sep=""), header = T, sep="\t")
ATC_aug_raw=read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATC_aug.txt", sep=""), header = T, sep="\t")
ATO_raw=read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATO.txt", sep=""), header = T, sep="\t")
ATO_aug_raw=read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATO_aug.txt", sep=""), header = T, sep="\t")


#################################
DIREC_R="Results_rev3/"  ###folder where results where 501-1000 are stored
ATE_raw=rbind(ATE_raw, read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATE.txt", sep=""), header = T, sep="\t"))
ATE_aug_raw=rbind(ATE_aug_raw, read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATE_aug.txt", sep=""),header=T,  sep="\t"))
ATM_raw=rbind(ATM_raw, read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATM.txt", sep=""), header = T, sep="\t"))
ATM_aug_raw=rbind(ATM_aug_raw, read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATM_aug.txt", sep=""), header = T, sep="\t"))
ATT_raw=rbind(ATT_raw, read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATT.txt", sep=""), header = T, sep="\t"))
ATT_aug_raw=rbind(ATT_aug_raw, read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATT_aug.txt", sep=""), header = T, sep="\t"))
ATC_raw=rbind(ATC_raw, read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATC.txt", sep=""), header = T, sep="\t"))
ATC_aug_raw=rbind(ATC_aug_raw, read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATC_aug.txt", sep=""), header = T, sep="\t"))
ATO_raw=rbind(ATO_raw, read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATO.txt", sep=""), header = T, sep="\t"))
ATO_aug_raw=rbind(ATO_aug_raw, read.table(paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATO_aug.txt", sep=""), header = T, sep="\t"))


ATE_raw=ATE_raw[which(!is.na(ATE_raw[,2])),]
ATE_aug_raw=ATE_aug_raw[which(!is.na(ATE_aug_raw[,2])),]
ATM_raw=ATM_raw[which(!is.na(ATM_raw[,2])),]
ATM_aug_raw=ATM_aug_raw[which(!is.na(ATM_aug_raw[,2])),]
ATT_raw=ATT_raw[which(!is.na(ATT_raw[,2])),]
ATT_aug_raw=ATT_aug_raw[which(!is.na(ATT_aug_raw[,2])),]
ATC_raw=ATC_raw[which(!is.na(ATC_raw[,2])),]
ATC_aug_raw=ATC_aug_raw[which(!is.na(ATC_aug_raw[,2])),]
ATO_raw=ATO_raw[which(!is.na(ATO_raw[,2])),]
ATO_aug_raw=ATO_aug_raw[which(!is.na(ATO_aug_raw[,2])),]


#################### ATE ############
ATE=process.boot(estOrg=ATE_raw[1,1], estBoot=ATE_raw[,2])[1,]

#################### augmented ATE ########
ATE_aug=process.boot(estOrg=ATE_aug_raw[1,1], estBoot=ATE_aug_raw[,2])[1,]

#################### ATM ############
ATM=process.boot(estOrg=ATM_raw[1,1], estBoot=ATM_raw[,2])[1,]

#################### augmented ATM ########
ATM_aug=process.boot(estOrg=ATM_aug_raw[1,1], estBoot=ATM_aug_raw[,2])[1,]

#################### ATT ############
ATT=process.boot(estOrg=ATT_raw[1,1], estBoot=ATT_raw[,2])[1,]

#################### augmented ATT ########
ATT_aug=process.boot(estOrg=ATT_aug_raw[1,1], estBoot=ATT_aug_raw[,2])[1,]

#################### ATC ############
ATC=process.boot(estOrg=ATC_raw[1,1], estBoot=ATC_raw[,2])[1,]

#################### augmented ATC ########
ATC_aug=process.boot(estOrg=ATC_aug_raw[1,1], estBoot=ATC_aug_raw[,2])[1,]


#################### ATO ############
ATO=process.boot(estOrg=ATO_raw[1,1], estBoot=ATO_raw[,2])[1,]

#################### augmented ATO ########
ATO_aug=process.boot(estOrg=ATO_aug_raw[1,1], estBoot=ATO_aug_raw[,2])[1,]



################### truncated weighted estimators ##################################
truncate=rep(NA, 4)
for (g in 1:length(column.names2)){
  
  DIREC_R="Results_rev2/"
  temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncate", "_", modelSpec, "_",column.names2[g], ".txt", sep=""),header = T, sep="\t")
  
  DIREC_R="Results_rev3/"
  temp=rbind(temp, read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncate", "_", modelSpec, "_",column.names2[g], ".txt", sep=""),header = T, sep="\t"))
  
  temp=temp[which(!is.na(temp[,1])),]
  
  truncate=rbind(truncate, process.boot(estOrg=temp[1,1], estBoot=temp[,2]))

}

truncate=truncate[-c(1),]
row.names(truncate)=column.names2


#######################truncated weighted esttimators with re-estimation##############################
truncate.rest=rep(NA, 4)
for(g in 1:length(column.names2)){
  
  DIREC_R="Results_rev2/"
  temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncate.rest", "_", modelSpec, "_",column.names2[g], ".txt", sep=""),header = T, sep="\t")
  
  DIREC_R="Results_rev3/"
  temp=rbind(temp, read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncate.rest", "_", modelSpec, "_",column.names2[g], ".txt", sep=""),header = T, sep="\t"))
  
  temp=temp[which(!is.na(temp[,1])),]
  
  truncate.rest=rbind(truncate.rest, process.boot(estOrg=temp[1,1], estBoot=temp[,2]))
  
}

truncate.rest=truncate.rest[-c(1),]
row.names(truncate.rest)=paste0(column.names2, " rest")





############################ augmented truncated estimators #########################
truncate.aug=rep(NA, 4)
for(g in 1:length(column.names2)){
  DIREC_R="Results_rev2/"
  temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncate.aug", "_", modelSpec, "_",column.names2[g], ".txt", sep=""),header = T, sep="\t")
  
  DIREC_R="Results_rev3/"
  temp=rbind(temp, read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncate.aug", "_", modelSpec, "_",column.names2[g], ".txt", sep=""),header = T, sep="\t"))
  
  temp=temp[which(!is.na(temp[,1])),]
  
  truncate.aug=rbind(truncate.aug, process.boot(estOrg=temp[1,1], estBoot=temp[,2]))
}

truncate.aug=truncate.aug[-c(1),]
row.names(truncate.aug)=paste0(column.names2, " aug")


############################# augmented truncated estimators with re-estimation ########################
truncate.aug.rest=rep(NA, 4)
for(g in 1:length(column.names2)){
  
  DIREC_R="Results_rev2/"
  temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncate.aug.rest", "_", modelSpec, "_",column.names2[g], ".txt", sep=""),header = T, sep="\t")
  
  DIREC_R="Results_rev3/"
  temp=rbind(temp, read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncate.aug.rest", "_", modelSpec, "_",column.names2[g], ".txt", sep=""),header = T, sep="\t"))
  
  temp=temp[which(!is.na(temp[,1])),]
  
  truncate.aug.rest=rbind(truncate.aug.rest, process.boot(estOrg=temp[1,1], estBoot=temp[,2]))

}


truncate.aug.rest=truncate.aug.rest[-c(1),]
row.names(truncate.aug.rest)=paste0(column.names2, " aug rest")




#####################################################################################
####################################### quantile truncation #########################
################### truncated weighted estimators ###################################

################### truncated weighted estimators ##################################
truncateQ=rep(NA, 4)
for (g in 1:length(column.names3)){
  
  DIREC_R="Results_rev2/"
  temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ", "_", modelSpec, "_",column.names3[g], ".txt", sep=""),header = T, sep="\t")
  
  DIREC_R="Results_rev3/"
  temp=rbind(temp, read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ", "_", modelSpec, "_",column.names3[g], ".txt", sep=""),header = T, sep="\t"))
  
  temp=temp[which(!is.na(temp[,1])),]
  
  truncateQ=rbind(truncateQ, process.boot(estOrg=temp[1,1], estBoot=temp[,2]))
  
}

truncateQ=truncateQ[-c(1),]
row.names(truncateQ)=column.names3


#######################truncated weighted esttimators with re-estimation##############################
truncateQ.rest=rep(NA, 4)
for(g in 1:length(column.names3)){
  
  DIREC_R="Results_rev2/"
  temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ.rest", "_", modelSpec, "_",column.names3[g], ".txt", sep=""),header = T, sep="\t")
  
  DIREC_R="Results_rev3/"
  temp=rbind(temp, read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ.rest", "_", modelSpec, "_",column.names3[g], ".txt", sep=""),header = T, sep="\t"))
  
  temp=temp[which(!is.na(temp[,1])),]
  
  truncateQ.rest=rbind(truncateQ.rest, process.boot(estOrg=temp[1,1], estBoot=temp[,2]))
  
}

truncateQ.rest=truncateQ.rest[-c(1),]
row.names(truncateQ.rest)=paste0(column.names3, " rest")





############################ augmented truncated estimators #########################
truncateQ.aug=rep(NA, 4)
for(g in 1:length(column.names3)){
  DIREC_R="Results_rev2/"
  temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ.aug", "_", modelSpec, "_",column.names3[g], ".txt", sep=""),header = T, sep="\t")
  
  DIREC_R="Results_rev3/"
  temp=rbind(temp, read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ.aug", "_", modelSpec, "_",column.names3[g], ".txt", sep=""),header = T, sep="\t"))
  
  temp=temp[which(!is.na(temp[,1])),]
  
  truncateQ.aug=rbind(truncateQ.aug, process.boot(estOrg=temp[1,1], estBoot=temp[,2]))
}

truncateQ.aug=truncateQ.aug[-c(1),]
row.names(truncateQ.aug)=paste0(column.names3, " aug")


############################# augmented truncated estimators with re-estimation ########################
truncateQ.aug.rest=rep(NA, 4)
for(g in 1:length(column.names3)){
  
  DIREC_R="Results_rev2/"
  temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ.aug.rest", "_", modelSpec, "_",column.names3[g], ".txt", sep=""),header = T, sep="\t")
  
  DIREC_R="Results_rev3/"
  temp=rbind(temp, read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ.aug.rest", "_", modelSpec, "_",column.names3[g], ".txt", sep=""),header = T, sep="\t"))
  
  temp=temp[which(!is.na(temp[,1])),]
  
  truncateQ.aug.rest=rbind(truncateQ.aug.rest, process.boot(estOrg=temp[1,1], estBoot=temp[,2]))
  
}

truncateQ.aug.rest=truncateQ.aug.rest[-c(1),]
row.names(truncateQ.aug.rest)=paste0(column.names3, " aug rest")




weightEst=rbind(ATE, ATE_aug, ATM, ATM_aug, ATT, ATT_aug, ATC, ATC_aug, ATO, ATO_aug)
row.names(weightEst)=c("ATE", "ATE aug", "ATM", "ATM aug", "ATT", "ATT aug", "ATC", "ATC aug", "ATO", "ATO aug")

weightEst=rbind(weightEst, truncate, truncate.aug, truncate.rest, truncate.aug.rest, truncateQ, truncateQ.aug, truncateQ.rest, truncateQ.aug.rest)



pencompEst=pencompEst[, c(1:4)]

pencompEst=cbind(pencompEst, pencompEst[,4]-pencompEst[,3])
weightEst=cbind(weightEst, weightEst[,4]-weightEst[,3])




#################
methods=c("ATE", "ATE aug" , "pencomp ATE" , 
          "ATM"  , "ATM aug", "pencomp ATM", "pencomp ATM w",
          
          "ATO" ,  "ATO aug" , "pencomp ATO", 
          "ATT",   "ATT aug", "pencomp ATT", "pencomp ATT w", 
          "ATC", "ATC aug" , "pencomp ATC" , "pencomp ATC w", 
          
           "truncate0.05 rest","truncate0.05 aug rest", "pencomp truncate0.05",  
          
           "truncateQ0.005 rest","truncateQ0.005 aug rest", "pencomp truncateQ0.005")


combineR=rbind(pencompEst[, c(1,2,5)], weightEst[, c(1,2,5)])


allTable=NULL
for(i in 1:length(methods)){
  allTable=rbind(allTable, combineR[which(row.names(combineR) %in% methods[i]),])
}



########## both models are correctly specified###########
n=nrow(allTable)
bothResult=cbind(rep("&", n), format(allTable[, 1]*100, digits = 3), rep("&", n), format(allTable[, 2]*100, digits = 3),
                 rep("&", n), format(allTable[, 3]*100, digits = 3),
                 rep("\\\\", n))


bothResult=cbind(methods, bothResult)


write.table(bothResult, "C:/Users/Tingting.Zhou/Desktop/paper2/Application/rightHeart/tableLatex_all.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)






