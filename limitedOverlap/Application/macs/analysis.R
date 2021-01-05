rm(list=ls())
library(nlme)
library("mgcv")
library("ggplot2")
library("MASS")

DIRECF="C:/Users/Tingting.Zhou/Desktop/paper2/Application/macs/Function/"  ###function location

source(paste(DIRECF, "imputeF.R", sep=""))  
source(paste(DIRECF, "pencomp2.R", sep=""))  
source(paste(DIRECF, "process.R", sep=""))  
source(paste(DIRECF, "formulaContruct.R", sep="")) 
source(paste(DIRECF, "balanceWeight.R", sep="")) 
source(paste(DIRECF, "pairMatch.R", sep="")) 



DIREC="C:/Users/Tingting.Zhou/Desktop/paper2/Application/macs/"
DIREC_R="Results/"  ###folder where results are stored

simdat=read.csv(paste0(DIREC, "simdat.csv"), header = T) ###write the dataset


#####keep only the subjects who were HIV+ at visit 130
keepHIV=simdat[which(simdat$hivStatus==1 & simdat$VISIT==130), c("CASEID","DAT2Y", "BIRTHDTY", "college", "white")]


###treatment indicator at first time point A1, at second time point A2
treat1=simdat[which(simdat$VISIT==130), c("CASEID", "treatRegm")] 
treat2=simdat[which(simdat$VISIT==140), c("CASEID", "treatRegm")]

treat=merge(treat1, treat2, by="CASEID", all.x = T)
names(treat)=c("CASEID", "treatV1", "treatV2")
treat$regm=paste(treat$treatV1, treat$treatV2, sep=":")

simdatFit=treat

############################LEU3N count history
count1=simdat[which(simdat$VISIT==120), c("CASEID", "LEU3N")]  
names(count1)[2]="LEU3N_v1"

count2=simdat[which(simdat$VISIT==130), c("CASEID", "LEU3N")]  
names(count2)[2]="LEU3N_v2"

count_fu=simdat[which(simdat$VISIT==140), c("CASEID", "LEU3N")] 
names(count_fu)[2]="LEU3N_fu"

simdatFit=merge(simdatFit, count1, by="CASEID", all.x = T)
simdatFit=merge(simdatFit, count2, by="CASEID", all.x = T)
simdatFit=merge(simdatFit, count_fu, by="CASEID", all.x = T)

#######################LEU2N count history
count1=simdat[which(simdat$VISIT==120), c("CASEID", "LEU2N")]  
names(count1)[2]="LEU2N_v1"

count2=simdat[which(simdat$VISIT==130), c("CASEID", "LEU2N")]  
names(count2)[2]="LEU2N_v2"

count_fu=simdat[which(simdat$VISIT==140), c("CASEID", "LEU2N")] 
names(count_fu)[2]="LEU2N_fu"

simdatFit=merge(simdatFit, count1, by="CASEID", all.x = T)
simdatFit=merge(simdatFit, count2, by="CASEID", all.x = T)
simdatFit=merge(simdatFit, count_fu, by="CASEID", all.x = T)


##########################WBC count history
count1=simdat[which(simdat$VISIT==120), c("CASEID", "WBC")]  
names(count1)[2]="WBC_v1"

count2=simdat[which(simdat$VISIT==130), c("CASEID", "WBC")]  
names(count2)[2]="WBC_v2"

count_fu=simdat[which(simdat$VISIT==140), c("CASEID", "WBC")] 
names(count_fu)[2]="WBC_fu"

simdatFit=merge(simdatFit, count1, by="CASEID", all.x = T)
simdatFit=merge(simdatFit, count2, by="CASEID", all.x = T)
simdatFit=merge(simdatFit, count_fu, by="CASEID", all.x = T)


#############################RBC count history
count1=simdat[which(simdat$VISIT==120), c("CASEID", "RBC")]  
names(count1)[2]="RBC_v1"

count2=simdat[which(simdat$VISIT==130), c("CASEID", "RBC")]  
names(count2)[2]="RBC_v2"

count_fu=simdat[which(simdat$VISIT==140), c("CASEID", "RBC")] 
names(count_fu)[2]="RBC_fu"

simdatFit=merge(simdatFit, count1, by="CASEID", all.x = T)
simdatFit=merge(simdatFit, count2, by="CASEID", all.x = T)
simdatFit=merge(simdatFit, count_fu, by="CASEID", all.x = T)


#######################PLATE history
count1=simdat[which(simdat$VISIT==120), c("CASEID", "PLATE")]  
names(count1)[2]="PLATE_v1"

count2=simdat[which(simdat$VISIT==130), c("CASEID", "PLATE")]  
names(count2)[2]="PLATE_v2"

count_fu=simdat[which(simdat$VISIT==140), c("CASEID", "PLATE")] 
names(count_fu)[2]="PLATE_fu"

simdatFit=merge(simdatFit, count1, by="CASEID", all.x = T)
simdatFit=merge(simdatFit, count2, by="CASEID", all.x = T)
simdatFit=merge(simdatFit, count_fu, by="CASEID", all.x = T)



#####################Viral load history
count1=simdat[which(simdat$VISIT==120), c("CASEID", "VLOAD")]  
names(count1)[2]="VLOAD_v1"

count2=simdat[which(simdat$VISIT==130), c("CASEID", "VLOAD")]  
names(count2)[2]="VLOAD_v2"

count_fu=simdat[which(simdat$VISIT==140), c("CASEID", "VLOAD")] 
names(count_fu)[2]="VLOAD_fu"

simdatFit=merge(simdatFit, count1, by="CASEID", all.x = T)
simdatFit=merge(simdatFit, count2, by="CASEID", all.x = T)
simdatFit=merge(simdatFit, count_fu, by="CASEID", all.x = T)


#########restrict to subjects who were HIV+ at visit 130#############
simdatFit=simdatFit[which(simdatFit$CASEID %in% keepHIV$CASEID),]
simdatFit=merge(simdatFit, keepHIV, by="CASEID", all.x = T)
simdatFit$age=as.numeric(simdatFit$DAT2Y-simdatFit$BIRTHDTY)


#########treatment indicator####################
simdatFit$treatInd=1
simdatFit$treatInd[which(simdatFit$treatV1=="none" | simdatFit$treatV2=="none")]=0

table(simdatFit$treatInd)



####################### histograms ###############################
hist(simdatFit$LEU3N_v1)
hist(simdatFit$LEU2N_v1)
hist(simdatFit$WBC_v1)
hist(simdatFit$RBC_v1)
hist(simdatFit$PLATE_v1)
hist(simdatFit$VLOAD_v1)

hist(simdatFit$LEU3N_v2)
hist(simdatFit$LEU2N_v2)
hist(simdatFit$WBC_v2)
hist(simdatFit$RBC_v2)
hist(simdatFit$PLATE_v2)
hist(simdatFit$VLOAD_v2)

hist(simdatFit$LEU3N_fu)


########### square root transformed histograms ###############################
hist(sqrt(simdatFit$LEU3N_v1))
hist(sqrt(simdatFit$LEU2N_v1))
hist(sqrt(simdatFit$WBC_v1))
hist(sqrt(simdatFit$RBC_v1))
hist(sqrt(simdatFit$PLATE_v1))
hist(sqrt(simdatFit$VLOAD_v1))

hist(sqrt(simdatFit$LEU3N_v2))
hist(sqrt(simdatFit$LEU2N_v2))
hist(sqrt(simdatFit$WBC_v2))
hist(sqrt(simdatFit$RBC_v2))
hist(sqrt(simdatFit$PLATE_v2))
hist(sqrt(simdatFit$VLOAD_v2))

hist(sqrt(simdatFit$LEU3N_fu))


########### log transformed histograms ###############################
hist(log(simdatFit$LEU3N_v1+1))
hist(log(simdatFit$LEU2N_v1+1))
hist(log(simdatFit$WBC_v1+1))
hist(log(simdatFit$RBC_v1+1))
hist(log(simdatFit$PLATE_v1+1))
hist(log(simdatFit$VLOAD_v1+1))

hist(log(simdatFit$LEU3N_v2+1))
hist(log(simdatFit$LEU2N_v2+1))
hist(log(simdatFit$WBC_v2+1))
hist(log(simdatFit$RBC_v2+1))
hist(log(simdatFit$PLATE_v2+1))
hist(log(simdatFit$VLOAD_v2+1))

hist(log(simdatFit$LEU3N_fu+1))


####transform the blood values by taking the square root, distributions of raw values are highly skewed
simdatFit$LEU3N_v1=sqrt(simdatFit$LEU3N_v1)
simdatFit$LEU2N_v1=sqrt(simdatFit$LEU2N_v1)
simdatFit$WBC_v1=sqrt(simdatFit$WBC_v1)
simdatFit$RBC_v1=sqrt(simdatFit$RBC_v1)
simdatFit$PLATE_v1=sqrt(simdatFit$PLATE_v1)
simdatFit$VLOAD_v1=sqrt(simdatFit$VLOAD_v1)


simdatFit$LEU3N_v2=sqrt(simdatFit$LEU3N_v2)
simdatFit$LEU2N_v2=sqrt(simdatFit$LEU2N_v2)
simdatFit$WBC_v2=sqrt(simdatFit$WBC_v2)
simdatFit$RBC_v2=sqrt(simdatFit$RBC_v2)
simdatFit$PLATE_v2=sqrt(simdatFit$PLATE_v2)
simdatFit$VLOAD_v2=sqrt(simdatFit$VLOAD_v2)


simdatFit$LEU3N_fu=sqrt(simdatFit$LEU3N_fu)



###########distribution between treated and not treated #######
boxplot(simdatFit$LEU3N_v1 ~ simdatFit$treatInd)
boxplot(simdatFit$LEU2N_v1 ~ simdatFit$treatInd)
boxplot(simdatFit$WBC_v1 ~ simdatFit$treatInd)
boxplot(simdatFit$RBC_v1 ~ simdatFit$treatInd)
boxplot(simdatFit$PLATE_v1 ~ simdatFit$treatInd)
boxplot(simdatFit$VLOAD_v1 ~ simdatFit$treatInd)

boxplot(simdatFit$LEU3N_v2 ~ simdatFit$treatInd)
boxplot(simdatFit$LEU2N_v2 ~ simdatFit$treatInd)
boxplot(simdatFit$WBC_v2 ~ simdatFit$treatInd)
boxplot(simdatFit$RBC_v2 ~ simdatFit$treatInd)
boxplot(simdatFit$PLATE_v2 ~ simdatFit$treatInd)
boxplot(simdatFit$VLOAD_v2 ~ simdatFit$treatInd)

boxplot(simdatFit$LEU3N_fu ~ simdatFit$treatInd)



######for our analysis, we looked only the complete data, avoid dealing with lost to followup/death topic of future research
simdatFit=simdatFit[which(!is.na(simdatFit$LEU3N_v1) & !is.na(simdatFit$LEU2N_v1) & !is.na(simdatFit$WBC_v1) & 
                            !is.na(simdatFit$RBC_v1) & !is.na(simdatFit$PLATE_v1) & 
                            
                            !is.na(simdatFit$LEU3N_v2) & !is.na(simdatFit$LEU2N_v2) & !is.na(simdatFit$WBC_v2) & 
                            !is.na(simdatFit$RBC_v2) & !is.na(simdatFit$PLATE_v2) & 
                            
                            
                            !is.na(simdatFit$LEU3N_fu) & !is.na(simdatFit$age) & !is.na(simdatFit$white)  ),]

dim(simdatFit)
sampleSize=nrow(simdatFit)
table(simdatFit$treatInd)



##########propensity score distribution ##################################
#########################################################################
treat.varname = "treatInd"
outcome.varname = "LEU3N_fu"
propenVarList=c("LEU3N_v1", "LEU3N_v2", "LEU2N_v1", "LEU2N_v2", "WBC_v1", "WBC_v2","RBC_v1" ,  "RBC_v2",  
                "PLATE_v1" ,"PLATE_v2", "age", "white")


######formula construct
formulaF=function(varList, y.name){
  return ( as.formula(paste(y.name, "~ ", paste(c(varList), collapse = "+"))) )
}

###### plot propensity score distributions#################################################
######## propensity score model ########################################################### 
propen.model=NULL
propen.model=formulaF(varList=propenVarList, y.name=treat.varname)


######## propensity score model ########################################################### 
model2a=glm(propen.model, data=simdatFit, family="binomial", control = list(maxit = 50))
simdatFit$pa=predict(model2a, newdata=simdatFit, type="response")

summary(simdatFit$pa)
summary(1/simdatFit$pa)


alpha.level=0.05
##########################################################
propTreat=simdatFit$pa
propControl=1-simdatFit$pa
treatInd=simdatFit[, treat.varname]

plot(density(propTreat[treatInd==1 & !is.na(propTreat)]))
lines(density(propTreat[treatInd==0 & !is.na(propTreat)]))





#######################################

simdatFit$id=1

####getting the t statistics#########
statResult=data.frame(var=propenVarList, tstats=NA)
for(k in 1:length(propenVarList)){
  
  print(k)
  testR=t.test(simdatFit[, propenVarList[k]] ~ simdatFit[, treat.varname])
  statResult$tstats[k]=as.numeric(testR$statistic)
  
}

statResult$tstatsAbs=abs(statResult$tstats)
sum(statResult$tstatsAbs>=2)
statResult$var[which(statResult$tstatsAbs>=2)]


#################################################################################################
#################################################################################################
###asymetric truncation, at quantile level
truncateVal=seq(0.01, 0.1, 0.01)
truncateQVal=seq(0, 0.03, 0.005)


#####################
treatID = which(simdatFit[, treat.varname] == 1)
controlID = which(simdatFit[, treat.varname] == 0)

####set initial outcome model
outcomeVarList0=c("LEU3N_v1", "LEU3N_v2", "LEU2N_v1", "LEU2N_v2", "WBC_v1", "WBC_v2","RBC_v1" ,  "RBC_v2",  
                  "PLATE_v1" ,"PLATE_v2", "age")
outcomeVarList1=c("LEU3N_v1", "LEU3N_v2", "LEU2N_v1", "LEU2N_v2", "WBC_v1", "WBC_v2","RBC_v1" ,  "RBC_v2",  
                  "PLATE_v1" ,"PLATE_v2", "age")


##################################################
modelSpec="all"
num.knot=20

###################################################
repNum=2000 ### number of multiple imputation
column.names=c("ATE",  "ATM", "ATM_w", "ATO",  "ATT", "ATT_w", "ATC", "ATC_w")
column.names2=paste0("truncate", truncateVal) 
column.names3=paste0("truncateQ", truncateQVal) 

row.names1=c("estimate", "var")
matrix.names=paste0("boot", 1:repNum)

mulResultP=array(NA, dim = c(2, length(column.names), repNum), dimnames = list(row.names1, column.names, matrix.names)) ###using truncated linear basis
mulResultP.truncate=array(NA, dim = c(2, length(column.names2), repNum), dimnames = list(row.names1, column.names2, matrix.names)) ###using truncated linear basis
mulResultP.truncateQ=array(NA, dim = c(2, length(column.names3), repNum), dimnames = list(row.names1, column.names3, matrix.names)) ###using truncated linear basis


for(k in 1:repNum){
  
  ##take a bootstrap sample, stratified bootstrap sampling
  set.seed(k)
  print(k)
  data = simdatFit[c( sample(treatID, replace = T), sample(controlID, replace = T) ),]  
  
  tempR=pencomp(simdatFit, data, propenVarList, outcomeVarList0=outcomeVarList0, outcomeVarList1=outcomeVarList1,
                treat.varname, outcome.varname, truncateQVal, truncateVal)
  
  
  mulResultP[,,k]=tempR$other
  mulResultP.truncate[,,k]=tempR$truncate
  mulResultP.truncateQ[,,k]=tempR$truncateQ
  
  
}

pencompEst=processPENCOMP(resultIn=mulResultP, estimand=c("ATE", "ATM","ATM_w", "ATO", "ATT", "ATT_w", "ATC", "ATC_w"))
pencompEst.truncate=processPENCOMP(resultIn=mulResultP.truncate, estimand=column.names2)
pencompEst.truncateQ=processPENCOMP(resultIn=mulResultP.truncateQ, estimand=column.names3)


sum(is.na(mulResultP))
sum(is.na(mulResultP.truncate))
sum(is.na(mulResultP.truncate))


####output results
write.table(pencompEst, paste(DIREC, DIREC_R, "pencompEst", "_", modelSpec, "_numknot", num.knot, ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(pencompEst.truncate, paste(DIREC, DIREC_R, "pencompEst.truncate", "_", modelSpec, "_numknot", num.knot, ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(pencompEst.truncateQ, paste(DIREC, DIREC_R, "pencompEst.truncateQ", "_", modelSpec, "_numknot", num.knot, ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")



#######store the raw results###############
ATE_raw=cbind(mulResultP[1,1,], mulResultP[2,1,])
ATM_raw=cbind(mulResultP[1,2,], mulResultP[2,2,])
ATM_w_raw=cbind(mulResultP[1,3,], mulResultP[2,3,])
ATO_raw=cbind(mulResultP[1,4,], mulResultP[2,4,])
ATT_raw=cbind(mulResultP[1,5,], mulResultP[2,5,])
ATT_w_raw=cbind(mulResultP[1,6,], mulResultP[2,6,])
ATC_raw=cbind(mulResultP[1,7,], mulResultP[2,7,])
ATC_w_raw=cbind(mulResultP[1,8,], mulResultP[2,8,])
write.table(ATE_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATE.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(ATM_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATM_raw.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(ATM_w_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATM_w.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(ATO_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATO.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(ATT_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATT.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(ATT_w_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATT_w.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(ATC_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATC.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(ATC_w_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATC_w.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")


for(k in 1:length(column.names2)){
  temp=cbind(mulResultP.truncate[1,k,], mulResultP.truncate[2,k,])
  write.table(temp, paste(DIREC, DIREC_R, "/raw/pencompEst.truncate", "_", modelSpec, "_numknot", num.knot, "_",column.names2[k], ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
}


for(k in 1:length(column.names3)){
  temp=cbind(mulResultP.truncateQ[1,k,], mulResultP.truncateQ[2,k,])
  write.table(temp, paste(DIREC, DIREC_R, "/raw/pencompEst.truncateQ", "_", modelSpec, "_numknot", num.knot, "_",column.names3[k], ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
}






##############################################################################################################
##############################################################################################################
###weighted estimators

####set initial outcome model
##might modify if some categorical variables do not have more than 1 level, remove those
#outcomeVarList0=propenVarList
#outcomeVarList1=propenVarList

est_weight=balanceWeight(data=simdatFit, propenVarList, outcomeVarList0, outcomeVarList1, treat.varname, outcome.varname, truncateQVal, truncateVal)


#####################################################
repBoot=2000 ###number of bootstrap samples for weighted estimators 
column.namesW=c("ATE", "ATE.aug", "ATM", "ATM.aug", "ATT", "ATT.aug", "ATC" ,"ATC.aug", "ATO", "ATO.aug") 
column.namesW2=paste0("truncate", truncateVal) 
column.namesW3=paste0("truncateQ", truncateQVal)
row.names=c("estimate")
matrix.names=paste0("boot", 1:repBoot)


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
  data = simdatFit[c( sample(treatID, replace = T), sample(controlID, replace = T) ),] 
  
  
  tempR=balanceWeight(data, propenVarList, outcomeVarList0, outcomeVarList1, treat.varname, outcome.varname, truncateQVal, truncateVal)
  print(tempR)
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


weightEst

est_weight$other
sum(is.na(wboot))


hist(wboot[1,1,])




####output results

write.table(weightEst, paste(DIREC, DIREC_R, "weightEst", "_", modelSpec, ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(weightEst.truncate, paste(DIREC, DIREC_R, "weightEst.truncate", "_", modelSpec, ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(weightEst.truncate.aug, paste(DIREC, DIREC_R, "weightEst.truncate.aug", "_", modelSpec, ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(weightEst.truncate.rest, paste(DIREC, DIREC_R, "weightEst.truncate.rest", "_", modelSpec, ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(weightEst.truncate.aug.rest, paste(DIREC, DIREC_R, "weightEst.truncate.aug.rest", "_", modelSpec, ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")

write.table(weightEst.truncateQ, paste(DIREC, DIREC_R, "weightEst.truncateQ", "_", modelSpec, ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(weightEst.truncateQ.aug, paste(DIREC, DIREC_R, "weightEst.truncateQ.aug", "_", modelSpec, ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(weightEst.truncateQ.rest, paste(DIREC, DIREC_R, "weightEst.truncateQ.rest", "_", modelSpec, ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(weightEst.truncateQ.aug.rest, paste(DIREC, DIREC_R, "weightEst.truncateQ.aug.rest", "_", modelSpec, ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")






#######store the raw results###############
ATE_raw=cbind(rep(est_weight$other["ATE"], repBoot), wboot[1,"ATE",])
ATE_aug_raw=cbind(rep(est_weight$other["ATE.aug"], repBoot), wboot[1,"ATE.aug",])
ATM_raw=cbind(rep(est_weight$other["ATM"], repBoot), wboot[1,"ATM",])
ATM_aug_raw=cbind(rep(est_weight$other["ATM.aug"], repBoot), wboot[1,"ATM.aug",])
ATT_raw=cbind(rep(est_weight$other["ATT"], repBoot), wboot[1,"ATT",])
ATT_aug_raw=cbind(rep(est_weight$other["ATT.aug"], repBoot), wboot[1,"ATT.aug",])
ATC_raw=cbind(rep(est_weight$other["ATC"], repBoot), wboot[1,"ATC",])
ATC_aug_raw=cbind(rep(est_weight$other["ATC.aug"], repBoot), wboot[1,"ATC.aug",])
ATO_raw=cbind(rep(est_weight$other["ATO"], repBoot), wboot[1,"ATO",])
ATO_aug_raw=cbind(rep(est_weight$other["ATO.aug"], repBoot), wboot[1,"ATO.aug",])
write.table(ATE_raw, paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATE.txt", sep=""), row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
write.table(ATE_aug_raw, paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATE_aug.txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
write.table(ATM_raw, paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATM.txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
write.table(ATM_aug_raw, paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATM_aug.txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
write.table(ATT_raw, paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATT.txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
write.table(ATT_aug_raw, paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATT_aug.txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
write.table(ATC_raw, paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATC.txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
write.table(ATC_aug_raw, paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATC_aug.txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
write.table(ATO_raw, paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATO.txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
write.table(ATO_aug_raw, paste(DIREC, DIREC_R, "/raw/weightEst", "_", modelSpec, "_ATO_aug.txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")


for(k in 1:length(column.names2)){
  temp=cbind(rep(est_weight$truncate[k], repBoot), wboot.truncate[1,k,])
  temp.rest=cbind(rep(est_weight$truncate_rest[k], repBoot), wboot.truncate.rest[1,k,])
  write.table(temp, paste(DIREC, DIREC_R, "/raw/weightEst.truncate", "_", modelSpec, "_",column.namesW2[k], ".txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
  write.table(temp.rest, paste(DIREC, DIREC_R, "/raw/weightEst.truncate.rest", "_", modelSpec, "_",column.namesW2[k], ".txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
  
}

#################
for(k in 1:length(column.names2)){
  temp=cbind(rep(est_weight$truncate.aug[k], repBoot), wboot.truncate.aug[1,k,])
  temp.rest=cbind(rep(est_weight$truncate.aug_rest[k], repBoot), wboot.truncate.aug.rest[1,k,])
  write.table(temp, paste(DIREC, DIREC_R, "/raw/weightEst.truncate.aug", "_", modelSpec, "_",column.namesW2[k], ".txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
  write.table(temp.rest, paste(DIREC, DIREC_R, "/raw/weightEst.truncate.aug.rest", "_", modelSpec, "_",column.namesW2[k], ".txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
  
}


#########################
for(k in 1:length(column.names3)){
  temp=cbind(rep(est_weight$truncateQ[k], repBoot), wboot.truncateQ[1,k,])
  temp.rest=cbind(rep(est_weight$truncateQ_rest[k], repBoot), wboot.truncateQ.rest[1,k,])
  write.table(temp, paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ", "_", modelSpec, "_",column.namesW3[k], ".txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
  write.table(temp.rest, paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ.rest", "_", modelSpec, "_",column.namesW3[k], ".txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
  
}

#################
for(k in 1:length(column.names3)){
  temp=cbind(rep(est_weight$truncateQ.aug[k], repBoot), wboot.truncateQ.aug[1,k,])
  temp.rest=cbind(rep(est_weight$truncateQ.aug_rest[k], repBoot), wboot.truncateQ.aug.rest[1,k,])
  write.table(temp, paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ.aug", "_", modelSpec, "_",column.namesW3[k], ".txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
  write.table(temp.rest, paste(DIREC, DIREC_R, "/raw/weightEst.truncateQ.aug.rest", "_", modelSpec, "_",column.namesW3[k], ".txt", sep=""),row.names = FALSE, col.names=TRUE, quote = F, sep="\t")
  
}






###################unadjusted analysis ######################
repBoot=2000
unadj=matrix(NA, nrow = repBoot, ncol = 2)
unadj[,1]=mean(simdatFit[which(simdatFit$treatInd==1), outcome.varname])-mean(simdatFit[which(simdatFit$treatInd==0), outcome.varname])
for(k in 1:repBoot){
  ##take a bootstrap sample, stratified bootstrap sampling
  set.seed(k)
  data = simdatFit[c( sample(treatID, replace = T), sample(controlID, replace = T) ),] 
  
  unadj[k,2]=mean(data[which(data[,treat.varname]==1), outcome.varname])-mean(data[which(data[, treat.varname]==0), outcome.varname])
  
  
}

unadj[1,1]
sd(unadj[,2])

unadj[1,1]+c(-1, 1)*1.96*sd(unadj[,2])

#> unadj[1,1]
#[1] -7.21012
#> sd(unadj[,2])
#[1] 0.464245
#> 
#  > unadj[1,1]+c(-1, 1)*1.96*sd(unadj[,2])
#[1] -8.120040 -6.300199

c(-7.21, 0.46, 1.82)

