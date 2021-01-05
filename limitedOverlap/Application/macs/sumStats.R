##############################################################################################
##############################################################################################
################PENCOMP #########################################################################
#########at every time point use the last two blood counts measures before initiation of treatment##########
########here I chose visit 7, 11 and 21

rm(list=ls())
library(splines)
library(nlme)
require(stats)
require(graphics)
#library("rootSolve")
library("mgcv")
require(glmnet)


DIREC="C:/Users/Tingting.Zhou/Desktop/paper2/Application/macs/"  ###function location

#source(paste0(DIREC, "stdMeanDiff.R") ) ###additional funcs for processing


###import the dataset
simdat=read.csv(paste0(DIREC, "simdat.csv"), header = T) ###write the dataset


table(simdat2$college)



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
simdatFit$pslogit=log(simdatFit$pa/(1-simdatFit$pa))

summary(simdatFit$pa)
summary(1/simdatFit$pa)


alpha.level=0.05
##########################################################
propTreat=simdatFit$pa
propControl=1-simdatFit$pa
treatInd=simdatFit[, treat.varname]

plot(density(propTreat[treatInd==1 & !is.na(propTreat)]), ylim=c(0, 5), main = "Propensity Score Distributions", xlab = "")
lines(density(propTreat[treatInd==0 & !is.na(propTreat)]))

hist(propTreat[simdatFit[, treat.varname]==0], xlim=c(0,1),col=rgb(0,0,1,1/4), main="Propensity Score Distributions", xlab="")
hist(propTreat[simdatFit[, treat.varname]==1], add=T, col=rgb(1,0,0,1/4) )




#######################################
simdatFit$id=1

####getting the t statistics#########
statResult=data.frame(mean1=rep(NA, length(propenVarList)), mean0=rep(NA, length(propenVarList)), 
                      sdiff=rep(NA, length(propenVarList)), tstats=rep(NA, length(propenVarList)), 
                      sdiffAfter=rep(NA, length(propenVarList)), tstatsAfter=rep(NA, length(propenVarList)))
for(k in 1:length(propenVarList)){
  
  print(k)
  testR=t.test(simdatFit[, propenVarList[k]] ~ simdatFit[, treat.varname])
  statResult$tstats[k]=as.numeric(testR$statistic)
  
  a=mean(simdatFit[which(simdatFit$treatInd==1), propenVarList[k]])
  a0=mean(simdatFit[which(simdatFit$treatInd==0), propenVarList[k]])
  
  a_var=var(simdatFit[which(simdatFit$treatInd==1), propenVarList[k]])
  a0_var=var(simdatFit[which(simdatFit$treatInd==0), propenVarList[k]])
  
  sdiff=(a-a0)/sqrt( ( a_var + a0_var ) / 2 )
  
  statResult$mean1[k]=a
  statResult$mean0[k]=a0
  statResult$sdiff[k]=sdiff
  
  #boxplot(simdatFit[,propenVarList[k]] ~ simdatFit[,treat.varname])
  
  #################################
  num.knot=10
  space0b=(max(simdatFit$pslogit)-min(simdatFit$pslogit))/(num.knot+1)
  knots0b=(min(simdatFit$pslogit)+space0b * (1:num.knot))
  
  ###assume a truncated linear basis
  linear0b=NULL
  linear0b =outer(simdatFit$pslogit, knots0b, "-")
  linear0b =linear0b * (linear0b > 0)
  
  response=simdatFit[,propenVarList[k]]
  covariateX=cbind(rep(1,nrow(simdatFit)), simdatFit$pslogit)
  
  all=rep(1, dim(simdatFit)[1])
  psppM=lme(response ~ covariateX-1, random=list(all=pdIdent(~0+linear0b)) ) 
  fixCoef=psppM$coefficients$fixed
  randCoef=psppM$coefficients$random$all
  
  predVal=covariateX %*% fixCoef + as.matrix(linear0b) %*% as.vector(unlist(randCoef))
  residVal=simdatFit[, propenVarList[k]]-predVal
  
  summary(residVal)
  
  testR_after=t.test(residVal ~ simdatFit[, treat.varname])
  statResult$tstatsAfter[k]=as.numeric(testR_after$statistic)
  
  
  a=mean(residVal[which(simdatFit$treatInd==1)])
  a0=mean(residVal[which(simdatFit$treatInd==0)])
  
  a_var=var(residVal[which(simdatFit$treatInd==1)])
  a0_var=var(residVal[which(simdatFit$treatInd==0)])
  
  sdiff_after=(a-a0)/sqrt( ( a_var + a0_var ) / 2 )
  
  statResult$sdiffAfter[k]=sdiff_after
  
  boxplot(residVal ~ simdatFit[,treat.varname])
  
  
}


statResult$tstatsAbs=abs(statResult$tstats)
sum(statResult$tstatsAbs>=2)
statResult$var[which(statResult$tstatsAbs>=2)]

  
  balanceCheck=cbind(rep("&", dim(statResult)[1]), format(statResult[,1], digits = 2), rep("&", dim(statResult)[1]), 
                format(statResult[,2], digits = 2),
                
                rep("&", dim(statResult)[1]), format(statResult[,3], digits = 2),
                rep("&", dim(statResult)[1]), format(statResult[,4], digits = 2),
                rep("&", dim(statResult)[1]), format(statResult[,5], digits = 2),
                rep("&", dim(statResult)[1]), format(statResult[,6], digits = 2),

                rep("\\\\", dim(statResult)[1]))
  
  
  balanceCheck=cbind(propenVarList, balanceCheck)
  

  write.table(balanceCheck, paste0(DIREC, "balanceCheck.txt"), sep="\t", quote=F, col.names=F, row.names = F)
  
  
