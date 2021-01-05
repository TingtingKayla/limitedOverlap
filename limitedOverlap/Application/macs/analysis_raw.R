
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

#########################
DIREC_R="Results/"  ###folder where results for sample 1-200 are stored
ATE_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATE.txt", sep=""), header=T, sep="\t")
ATM_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATM_raw.txt", sep=""),header = T, sep="\t")
ATM_w_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATM_w.txt", sep=""),header = T, sep="\t")
ATO_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATO.txt", sep=""),header = T, sep="\t")
ATT_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATT.txt", sep=""),header = T, sep="\t")
ATT_w_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATT_w.txt", sep=""),header = T, sep="\t")
ATC_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATC.txt", sep=""),header = T, sep="\t")
ATC_w_raw=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATC_w.txt", sep=""),header = T, sep="\t")


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
repNum=2000
ATE=matrix(NA, nrow=repNum, ncol=4)

for(k in c(1:repNum)){
ATE[k,]=processPENCOMP(resultIn=ATE_raw[1:k,])[1,]
}

plot(1:repNum, ATE[,1])
plot(1:repNum, ATE[,2], col="black")

points(1:repNum, ATE[,3], col="red")
points(1:repNum, ATE[,4], col="blue")


############ ATM raw ##################
ATM=matrix(NA, nrow=repNum, ncol=4)

for(k in c(1:repNum)){
  ATM[k,]=processPENCOMP(resultIn=ATM_raw[1:k,])[1,]
}

plot(1:repNum, ATM[,1])
plot(1:repNum, ATM[,2], col="black")
points(1:repNum, ATM[,3], col="red")
points(1:repNum, ATM[,4], col="blue")


############ ATM weights raw ##################
ATM_w=matrix(NA, nrow=repNum, ncol=4)

for(k in c(1:repNum)){
  ATM_w[k,]=processPENCOMP(resultIn=ATM_w_raw[1:k,])[1,]
}

plot(1:repNum, ATM_w[,1])
plot(1:repNum, ATM_w[,2], col="black")
points(1:repNum, ATM_w[,3], col="red")
points(1:repNum, ATM_w[,4], col="blue")


############ ATO raw ##################
ATO=matrix(NA, nrow=repNum, ncol=4)

for(k in c(1:repNum)){
  ATO[k,]=processPENCOMP(resultIn=ATO_raw[1:k,])[1,]
}

plot(1:repNum, ATO[,1])
plot(1:repNum, ATO[,2], col="black")
points(1:repNum, ATO[,3], col="red")
points(1:repNum, ATO[,4], col="blue")


############ ATT raw ##################
ATT=matrix(NA, nrow=repNum, ncol=4)

for(k in c(1:repNum)){
  ATT[k,]=processPENCOMP(resultIn=ATT_raw[1:k,])[1,]
}

plot(1:repNum, ATT[,1])
plot(1:repNum, ATT[,2], col="black")
points(1:repNum, ATT[,3], col="red")
points(1:repNum, ATT[,4], col="blue")


############ ATT_w raw ##################
ATT_w=matrix(NA, nrow=repNum, ncol=4)

for(k in c(1:repNum)){
  ATT_w[k,]=processPENCOMP(resultIn=ATT_w_raw[1:k,])[1,]
}

plot(1:repNum, ATT_w[,1])
plot(1:repNum, ATT_w[,2], col="black")
points(1:repNum, ATT_w[,3], col="red")
points(1:repNum, ATT_w[,4], col="blue")



############ ATC raw ##################
ATC=matrix(NA, nrow=repNum, ncol=4)

for(k in c(1:repNum)){
  ATC[k,]=processPENCOMP(resultIn=ATC_raw[1:k,])[1,]
}

plot(1:repNum, ATC[,1])
plot(1:repNum, ATC[,2], col="black")
points(1:repNum, ATC[,3], col="red")
points(1:repNum, ATC[,4], col="blue")


############ ATT_w raw ##################
ATC_w=matrix(NA, nrow=repNum, ncol=4)

for(k in c(1:repNum)){
  ATC_w[k,]=processPENCOMP(resultIn=ATC_w_raw[1:k,])[1,]
}

plot(1:repNum, ATC_w[,1])
plot(1:repNum, ATC_w[,2], col="black")
points(1:repNum, ATC_w[,3], col="red")
points(1:repNum, ATC_w[,4], col="blue")


#####################################################
g=1
temp=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst.truncate", "_", modelSpec, "_numknot", num.knot, "_",column.names2[g], ".txt", sep=""),header = T, sep="\t")

temp=temp[which(!is.na(temp[,1])),]

truncate=matrix(NA, nrow=repNum, ncol = 4)
for(k in c(1:repNum)){
  truncate[k,]=processPENCOMP(resultIn=temp[1:k,])[1,]
}

plot(1:repNum, truncate[,1])
plot(1:repNum, truncate[,2], col="black")
points(1:repNum, truncate[,3], col="red")
points(1:repNum, truncate[,4], col="blue")


#######################################################
g=1
tempQ=read.table(paste(DIREC, DIREC_R, "/raw/pencompEst.truncateQ", "_", modelSpec, "_numknot", num.knot, "_",column.names3[g], ".txt", sep=""),header = T, sep="\t")

truncateQ=matrix(NA, nrow=repNum, ncol = 4)
for(k in c(1:repNum)){
  truncateQ[k,]=processPENCOMP(resultIn=temp[1:k,])[1,]
}

plot(1:repNum, truncateQ[,1])
plot(1:repNum, truncateQ[,2], col="black")
points(1:repNum, truncateQ[,3], col="red")
points(1:repNum, truncateQ[,4], col="blue")




#################################################################################
#################################################################################
repNum=2000

#################################
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


ATE_raw=ATE_raw[which(!is.na(ATE_raw[,1])),]
ATE_aug_raw=ATE_aug_raw[which(!is.na(ATE_aug_raw[,1])),]
ATM_raw=ATM_raw[which(!is.na(ATM_raw[,1])),]
ATM_aug_raw=ATM_aug_raw[which(!is.na(ATM_aug_raw[,1])),]
ATT_raw=ATT_raw[which(!is.na(ATT_raw[,1])),]
ATT_aug_raw=ATT_aug_raw[which(!is.na(ATT_aug_raw[,1])),]
ATC_raw=ATC_raw[which(!is.na(ATC_raw[,1])),]
ATC_aug_raw=ATC_aug_raw[which(!is.na(ATC_aug_raw[,1])),]
ATO_raw=ATO_raw[which(!is.na(ATO_raw[,1])),]
ATO_aug_raw=ATO_aug_raw[which(!is.na(ATO_aug_raw[,1])),]



#################### ATE ############
ATE=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  ATE[k,]=process.boot(estOrg=ATE_raw[1,1], estBoot=ATE_raw[1:k,2])[c(1,2)]
}

plot(1:repNum, ATE[,1])
plot(1:repNum, ATE[,2], col="black")


#################### augmented ATE ########
ATE_aug=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  ATE_aug[k,]=process.boot(estOrg=ATE_aug_raw[1,1], estBoot=ATE_aug_raw[1:k,2])[c(1,2)]
}

plot(1:repNum, ATE_aug[,1])
plot(1:repNum, ATE_aug[,2], col="black")


#################### ATM ############
ATM=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  ATM[k,]=process.boot(estOrg=ATM_raw[1,1], estBoot=ATM_raw[1:k,2])[c(1,2)]
}

plot(1:repNum, ATM[,1])
plot(1:repNum, ATM[,2], col="black")


#################### augmented ATM ########
ATM_aug=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  ATM_aug[k,]=process.boot(estOrg=ATM_aug_raw[1,1], estBoot=ATM_aug_raw[1:k,2])[c(1,2)]
}

plot(1:repNum, ATM_aug[,1])
plot(1:repNum, ATM_aug[,2], col="black")



#################### ATT ############
ATT=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  ATT[k,]=process.boot(estOrg=ATT_raw[1,1], estBoot=ATT_raw[1:k,2])[c(1,2)]
}

plot(1:repNum, ATT[,1])
plot(1:repNum, ATT[,2], col="black")



#################### augmented ATM ########
ATT_aug=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  ATT_aug[k,]=process.boot(estOrg=ATT_aug_raw[1,1], estBoot=ATT_aug_raw[1:k,2])[c(1,2)]
}

plot(1:repNum, ATT_aug[,1])
plot(1:repNum, ATT_aug[,2], col="black")




#################### ATC ############
ATC=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  ATC[k,]=process.boot(estOrg=ATC_raw[1,1], estBoot=ATC_raw[1:k,2])[c(1,2)]
}

plot(1:repNum, ATC[,1])
plot(1:repNum, ATC[,2], col="black")


#################### augmented ATM ########
ATC_aug=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  ATC_aug[k,]=process.boot(estOrg=ATC_aug_raw[1,1], estBoot=ATC_aug_raw[1:k,2])[c(1,2)]
}

plot(1:repNum, ATC_aug[,1])
plot(1:repNum, ATC_aug[,2], col="black")


################### truncated weighted estimators ##################################
g=1
temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncate", "_", modelSpec, "_",column.names2[g], ".txt", sep=""),header = T, sep="\t")

temp=temp[which(!is.na(temp[,1])),]

truncate=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  truncate[k,]=process.boot(estOrg=temp[1,1], estBoot=temp[1:k,2])[c(1,2)]
}

plot(1:repNum, truncate[,1])
plot(1:repNum, truncate[,2], col="black")


#######################truncated weighted esttimators with re-estimation##############################
g=1
temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncate.rest", "_", modelSpec, "_",column.names2[g], ".txt", sep=""),header = T, sep="\t")

temp=temp[which(!is.na(temp[,1])),]

truncate=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  truncate[k,]=process.boot(estOrg=temp[1,1], estBoot=temp[1:k,2])[c(1,2)]
}

plot(1:repNum, truncate[,1])
plot(1:repNum, truncate[,2], col="black")





############################ augmented truncated estimators #########################
g=1
temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncate.aug", "_", modelSpec, "_",column.names2[g], ".txt", sep=""),header = T, sep="\t")

temp=temp[which(!is.na(temp[,1])),]


truncate=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  truncate[k,]=process.boot(estOrg=temp[1,1], estBoot=temp[1:k,2])[c(1,2)]
}

plot(1:repNum, truncate[,1])
plot(1:repNum, truncate[,2], col="black")


############################# augmented truncated estimators with re-estimation ########################
g=1
temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncate.aug.rest", "_", modelSpec, "_",column.names2[g], ".txt", sep=""),header = T, sep="\t")

temp=temp[which(!is.na(temp[,1])),]

truncate=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  truncate[k,]=process.boot(estOrg=temp[1,1], estBoot=temp[1:k,2])[c(1,2)]
}

plot(1:repNum, truncate[,1])
plot(1:repNum, truncate[,2], col="black")



#####################################################################################
####################################### quantile truncation #########################
################### truncated weighted estimators ###################################
g=1
temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ", "_", modelSpec, "_",column.names3[g], ".txt", sep=""),header = T, sep="\t")

temp=temp[which(!is.na(temp[,1])),]

truncate=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  truncate[k,]=process.boot(estOrg=temp[1,1], estBoot=temp[1:k,2])[c(1,2)]
}

plot(1:repNum, truncate[,1])
plot(1:repNum, truncate[,2], col="black")


#######################truncated weighted esttimators with re-estimation##############################
g=1
temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ.rest", "_", modelSpec, "_",column.names3[g], ".txt", sep=""),header = T, sep="\t")

temp=temp[which(!is.na(temp[,1])),]

truncate=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  truncate[k,]=process.boot(estOrg=temp[1,1], estBoot=temp[1:k,2])[c(1,2)]
}

plot(1:repNum, truncate[,1])
plot(1:repNum, truncate[,2], col="black")





############################ augmented truncated estimators #########################
g=1
temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ.aug", "_", modelSpec, "_",column.names3[g], ".txt", sep=""),header = T, sep="\t")

temp=temp[which(!is.na(temp[,1])),]


truncate=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  truncate[k,]=process.boot(estOrg=temp[1,1], estBoot=temp[1:k,2])[c(1,2)]
}

plot(1:repNum, truncate[,1])
plot(1:repNum, truncate[,2], col="black")


############################# augmented truncated estimators with re-estimation ########################
g=1
temp=read.table(paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ.aug.rest", "_", modelSpec, "_",column.names3[g], ".txt", sep=""),header = T, sep="\t")

temp=temp[which(!is.na(temp[,1])),]



truncate=matrix(NA, nrow=repNum, ncol = 2)
for(k in c(1:repNum)){
  truncate[k,]=process.boot(estOrg=temp[1,1], estBoot=temp[1:k,2])[c(1,2)]
}

plot(1:repNum, truncate[,1])
plot(1:repNum, truncate[,2], col="black")




###############






