rm(list=ls())
library(nlme)
library("mgcv")
library("ggplot2")
library("MASS")

DIRECF="C:/Users/Tingting.Zhou/Desktop/paper2/Application/rightHeart/Function/"  ###function location

source(paste(DIRECF, "imputeF.R", sep=""))  
source(paste(DIRECF, "pencomp.R", sep=""))  
source(paste(DIRECF, "process.R", sep=""))  
source(paste(DIRECF, "formulaContruct.R", sep="")) 
source(paste(DIRECF, "balanceWeight.R", sep="")) 
source(paste(DIRECF, "pairMatch.R", sep="")) 


#################################################################
## Right heart cath dataset
#rhc <- read.csv("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.csv")
## first 6 rows
#head(rhc)

#dim(rhc)


DIREC="C:/Users/Tingting.Zhou/Desktop/paper2/Application/rightHeart/"
#DIREC_R="Results_rev2/"  ###folder where results are stored samples 1-200 for pencomp and 1-500 for weighted estimators
#DIREC_R="Results_rev3/"  ###folder where results are stored samples 201-500 for pencomp and 501-1000 for weighted estimators
DIREC_R="Results_rev4/"  ###folder where results are stored samples 501-1000 for pencomp

rhc=read.csv(paste0(DIREC, "rhc.csv"), header = T) ###write the dataset

table(rhc$cat1) ##primary disease category
#ARF, acute respiratory failure (ARF)
#CHF, congestive heart failure
#Cirrhosis, a chronic disease of the liver marked by degeneration of cells, inflammation, and fibrous thickening of tissue
#Colon Cancer, colon cancer metastiatic to the liver
#Coma, nontraumatic coma
#COPD, chronic obstructive pulmonary disease
#Lung Cancer non-small cell cancer of the lung (stage III or IV)  
#MOSF w/Malignancy, 
#MOSF w/Sepsis, multiorgan system failure (MOSF), 

#sepsis: a serious condition resulting from the presence of harmful microorganisms
#in the blood or other tissues and the body's response to their presence, 
#potentially leading to the malfunctioning of various organs, shock, and death.

sum(is.na(rhc$cat1==""))
rhc$cat1_copd=as.numeric(rhc$cat1=="COPD")
rhc$cat1_mosfsep=as.numeric(rhc$cat1=="MOSF w/Sepsis")
rhc$cat1_mosfmal=as.numeric(rhc$cat1=="MOSF w/Malignancy")
rhc$cat1_chf=as.numeric(rhc$cat1=="CHF")
rhc$cat1_coma=as.numeric(rhc$cat1=="Coma")
rhc$cat1_cirr=as.numeric(rhc$cat1=="Cirrhosis")
rhc$cat1_lung=as.numeric(rhc$cat1=="Lung Cancer")
rhc$cat1_colon=as.numeric(rhc$cat1=="Colon Cancer")
rhc$cat1_arf=as.numeric(rhc$cat1=="ARF")  ###remove this one

rhc$cat1_cancer=as.numeric(rhc$cat1 %in% c("Lung Cancer", "Colon Cancer"))
table(rhc$cat1_cancer)
table(rhc$cat1_lung)
table(rhc$cat1_colon)

table(rhc$cat2) ##secondary disease category
sum(is.na(rhc$cat2))
rhc$cat2_mosfsep=0
rhc$cat2_mosfsep[which(rhc$cat2=="MOSF w/Sepsis")]=1

rhc$cat2_coma=0
rhc$cat2_coma[which(rhc$cat2=="Coma")]=1

rhc$cat2_mosfmal=0
rhc$cat2_mosfmal[which(rhc$cat2=="MOSF w/Malignancy")]=1

rhc$cat2_lung=0
rhc$cat2_lung[which(rhc$cat2=="Lung Cancer")]=1

rhc$cat2_cirr=0
rhc$cat2_cirr[which(rhc$cat2=="Cirrhosis")]=1

rhc$cat2_colon=0
rhc$cat2_colon[which(rhc$cat2=="Colon Cancer")]=1

rhc$cat2_none=0
rhc$cat2_none[which(is.na(rhc$cat2))]=1  ###use this as baseline


rhc$cat2_cancer=as.numeric(rhc$cat2 %in% c("Lung Cancer", "Colon Cancer"))
table(rhc$cat2_cancer)
table(rhc$cat2_colon)
table(rhc$cat2_lung)


###no secondary disease category####
rhc$cat2RV=paste0(rhc$cat2)
rhc$cat2RV[which(rhc$cat2RV=="NA")]="none"
table(rhc$cat2RV)


table(rhc$ca)  ###cancer yes or no, metastiatic
sum(rhc$ca=="")
rhc$ca_yes=as.numeric(rhc$ca=="Yes") ##cancer-localized
rhc$ca_meta=as.numeric(rhc$ca=="Metastatic") ##cancer-mestastatic
###the baseline is no cancer


table(rhc$sadmdte) ##study admission date
table(rhc$dschdte) ##hospital discharge date
table(rhc$dthdte) ##date of death
table(rhc$lstctdte) ##date of last contact
table(rhc$death) ##binary indicator for death at any time up to 180 days

##Categories of comorbidities illness
sum(rhc$cardiohx=="") ##cardiovascular symptoms
sum(rhc$chfhx=="")  ###congestive heart failure
sum(rhc$dementhx=="") ##dementia, stroke or cerebral infarct parkinson's disease
sum(rhc$psychhx=="") ##psychiatric history, active psychosis or severe depression
sum(rhc$chrpulhx=="") ###chronic pulmonary disease, severe pulmondary disease
sum(rhc$renalhx=="") ##chronic renal disease, chronic hemodialysis or peritoneal dialysis
sum(rhc$liverhx=="") ##cirrhosis, hepatic failure
sum(rhc$gibledhx=="") ###upper GI bleeding
sum(rhc$malighx=="") ##solid tumor, mestastatic disease, chronic lekemia/myeloma, acute leukemia, lymphoma
sum(rhc$immunhx=="") ###immunosuppression, organ transplant, HIV, diabetes mellitus, connective tissue disease
sum(rhc$transhx=="") ## transfer (>24 hours) from another hospital
sum(rhc$amihx=="") ###definite myocardial infarction

##Categories of comorbidities illness
table(rhc$cardiohx) ##cardiovascular symptoms
table(rhc$chfhx)  ###congestive heart failure
table(rhc$dementhx) ##dementia, stroke or cerebral infarct parkinson's disease
table(rhc$psychhx) ##psychiatric history, active psychosis or severe depression
table(rhc$chrpulhx) ###chronic pulmonary disease, severe pulmondary disease
table(rhc$renalhx) ##chronic renal disease, chronic hemodialysis or peritoneal dialysis
table(rhc$liverhx) ##cirrhosis, hepatic failure
table(rhc$gibledhx) ###upper GI bleeding
table(rhc$malighx) ##solid tumor, mestastatic disease, chronic lekemia/myeloma, acute leukemia, lymphoma
table(rhc$immunhx) ###immunosuppression, organ transplant, HIV, diabetes mellitus, connective tissue disease
table(rhc$transhx) ## transfer (>24 hours) from another hospital
table(rhc$amihx) ###definite myocardial infarction



summary(rhc$age)
hist(rhc$age)
sum(is.na(rhc$age))  ###no missing value for age

table(rhc$sex)
sum(paste0(rhc$sex)=="")  ##gender
rhc$female=0
rhc$female[which(rhc$sex=="Female")]=1  ##indicator for female, no missing value
rhc$female=as.factor(rhc$female)

summary(rhc$edu)  ##year of education
sum(is.na(rhc$edu))  ##continuous, no missing

summary(rhc$surv2md1) ##estimate of probability of surviving 2 months
sum(is.na(rhc$surv2md1)) ##estimate of probability of surviving 2 months


summary(rhc$das2d3pc) ###DASI-duke activity status index
sum(is.na(rhc$das2d3pc))

summary(rhc$t3d30)
summary(rhc$dth30) ###I think it's indicator for death at day 30


summary(rhc$aps1)  ###APACHE score
summary(rhc$scoma1) ###glasgow coma score
summary(rhc$meanbp1) ##mean blood pressure
summary(rhc$wblc1) ###WBC   
summary(rhc$hrt1) ###heart rate
summary(rhc$resp1) ###respiratory rate
summary(rhc$temp1) ###temperature
summary(rhc$pafi1) ###PaO2/FIO2 ratio
summary(rhc$alb1) ###albumin
summary(rhc$hema1) ###hematocrit
summary(rhc$bili1)## bilirubin
summary(rhc$crea1) ###creatinine
summary(rhc$sod1) ###sodium
summary(rhc$pot1) ###potassium
summary(rhc$paco21) ###PaCO2
summary(rhc$ph1) ##PH
summary(rhc$swang1) ##right heart catheterization (RHC)
summary(rhc$wtkilo1) ##weight

###weight=0
rhc$wt0=as.numeric(rhc$wtkilo1==0)
table(rhc$wt0)

sum(is.na(rhc$aps1))  ###APACHE score
sum(is.na(rhc$scoma1)) ###glasgow coma score
sum(is.na(rhc$meanbp1)) ##mean blood pressure
sum(is.na(rhc$wblc1)) ###WBC   
sum(is.na(rhc$hrt1)) ###heart rate
sum(is.na(rhc$resp1)) ###respiratory rate
sum(is.na(rhc$temp1)) ###temperature
sum(is.na(rhc$pafi1)) ###PaO2/FIO2 ratio
sum(is.na(rhc$alb1)) ###albumin
sum(is.na(rhc$hema1)) ###hematocrit
sum(is.na(rhc$bili1))## bilirubin
sum(is.na(rhc$crea1)) ###creatinine
sum(is.na(rhc$sod1)) ###sodium
sum(is.na(rhc$pot1)) ###potassium
sum(is.na(rhc$paco21)) ###PaCO2
sum(is.na(rhc$ph1)) ##PH
sum(is.na(rhc$wtkilo1)) ##weight

summary(rhc$swang1) ##right heart catheterization (RHC)

table(rhc$dnr1) ###do not resuscitate status on day 1
sum(rhc$dnr1=="")

table(rhc$ninsclas) ###insurance type

rhc$ins_care=0
rhc$ins_care[which(rhc$ninsclas=="Medicare")]=1

rhc$ins_pcare=0
rhc$ins_pcare[which(rhc$ninsclas=="Private & Medicare")]=1

rhc$ins_caid=0
rhc$ins_caid[which(rhc$ninsclas=="Medicaid")]=1

rhc$ins_no=0
rhc$ins_no[which(rhc$ninsclas=="No insurance")]=1

rhc$ins_carecaid=0
rhc$ins_carecaid[which(rhc$ninsclas=="Medicare & Medicaid")]=1

###the reference category is private insurance



###categories of admission diagnosis
table(rhc$resp) ##respiratory diagnosis
table(rhc$card) ##cardiovscular diagnosis
table(rhc$neuro) ##neurological diagnosis
table(rhc$gastr) ##gastrointenstinal diagnosis
table(rhc$renal) ##renal diagnosis
table(rhc$meta) ##metabolic diagnosis
table(rhc$hema) ##hematological diagnosis
table(rhc$seps) ##sepsis diagnosis
table(rhc$trauma) ###trauma diagnosis
table(rhc$ortho) ###orthopedic diagnosis  ##


####here I turn the variables into binary 0, 1
rhc$ortho=as.numeric(rhc$ortho=="Yes")
rhc$resp=as.numeric(rhc$resp=="Yes") ##respiratory diagnosis
rhc$card=as.numeric(rhc$card=="Yes") ##cardiovscular diagnosis
rhc$neuro=as.numeric(rhc$neuro=="Yes") ##neurological diagnosis
rhc$gastr=as.numeric(rhc$gastr=="Yes") ##gastrointenstinal diagnosis
rhc$renal=as.numeric(rhc$renal=="Yes") ##renal diagnosis
rhc$meta=as.numeric(rhc$meta=="Yes") ##metabolic diagnosis
rhc$hema=as.numeric(rhc$hema=="Yes") ##hematological diagnosis
rhc$seps=as.numeric(rhc$seps=="Yes") ##sepsis diagnosis
rhc$trauma=as.numeric(rhc$trauma=="Yes") ###trauma diagnosis


table(rhc$treat, rhc$ortho)  ###rare events in both treated and controls, with 3 in control and 4 in treated groups


sum(rhc$resp=="") ##respiratory diagnosis
sum(rhc$card=="") ##cardiovscular diagnosis
sum(rhc$neuro=="") ##neurological diagnosis
sum(rhc$gastr=="") ##gastrointenstinal diagnosis
sum(rhc$renal=="") ##renal diagnosis
sum(rhc$meta=="") ##metabolic diagnosis
sum(rhc$hema=="") ##hematological diagnosis
sum(rhc$seps=="") ##sepsis diagnosis
sum(rhc$trauma=="") ###trauma diagnosis
sum(rhc$ortho=="") ###orthopedic diagnosis


rhc$adld3p
rhc$urin1
table(rhc$race) ##race categories: black, other, and white
sum(rhc$race=="") ##no missing value

rhc$raceblack=0
rhc$raceblack[which(rhc$race=="black")]=1

rhc$raceother=0
rhc$raceother[which(rhc$race=="other")]=1

###the reference class is white


summary(rhc$income)
table(rhc$income)  ###income categories: 11-25K, 25-50K, >50K, under 11K
sum(rhc$income=="")

rhc$income1=0
rhc$income1[which(rhc$income=="$11-$25k")]=1

rhc$income2=0
rhc$income2[which(rhc$income=="$25-$50k")]=1

rhc$income3=0
rhc$income3[which(rhc$income=="> $50k")]=1

###under $11K is the reference


propenVarList=c("age", "sex", "edu", "raceblack","raceother", "income1", "income2", "income3", 
                "ins_care","ins_pcare","ins_caid","ins_no","ins_carecaid",
            "cat1_copd","cat1_mosfsep","cat1_mosfmal","cat1_chf","cat1_coma","cat1_cirr","cat1_lung","cat1_colon",
            "cat2_mosfsep","cat2_coma","cat2_mosfmal","cat2_lung","cat2_cirr","cat2_colon", 
            
            "resp","card","neuro","gastr","renal","meta","hema","seps","trauma","ortho",
            "das2d3pc","dnr1","ca_yes","ca_meta",
            "surv2md1","aps1","scoma1","wtkilo1","temp1","meanbp1","resp1","hrt1","pafi1","paco21","ph1",
            "wblc1","hema1","sod1","pot1","crea1","bili1","alb1",
            
            "cardiohx","chfhx","dementhx","psychhx","chrpulhx","renalhx","liverhx","gibledhx","malighx",
            "immunhx","transhx","amihx","wt0")


rhc$treat=NA
rhc$treat[which(rhc$swang1=="RHC")]=1
rhc$treat[which(rhc$swang1=="No RHC")]=0
#rhc$treat=as.factor(rhc$treat)
table(rhc$treat)
table(rhc$swang1)


table(rhc$dth30) ###I think it's indicator for death at day 30
rhc$y30=NA
rhc$y30[which(rhc$dth30=="Yes")]=1
rhc$y30[which(rhc$dth30=="No")]=0
#rhc$y30=as.factor(rhc$y30)
table(rhc$y30)

table(rhc$death)
rhc$y180=NA
rhc$y180[which(rhc$death=="Yes")]=1
rhc$y180[which(rhc$death=="No")]=0
table(rhc$y180)

table(rhc$dth30, rhc$death)



#######################################
### 
dataOR=rhc
dataOR$id=1


#View(rhc[, propenVarList])


table(dataOR$treat, dataOR$raceblack)
table(dataOR$treat, dataOR$raceother)
table(dataOR$treat, dataOR$income1)
table(dataOR$treat, dataOR$income2)
table(dataOR$treat, dataOR$income3)
table(dataOR$treat, dataOR$ins_care)
table(dataOR$treat, dataOR$ins_pcare)
table(dataOR$treat, dataOR$ins_caid)
table(dataOR$treat, dataOR$ins_no)
table(dataOR$treat, dataOR$ins_carecaid)

table(dataOR$treat, dataOR$cat1_copd)
table(dataOR$treat, dataOR$cat1_mosfsep)
table(dataOR$treat, dataOR$cat1_mosfmal)
table(dataOR$treat, dataOR$cat1_chf)
table(dataOR$treat, dataOR$cat1_coma)
table(dataOR$treat, dataOR$cat1_cirr)
table(dataOR$treat, dataOR$cat1_lung)  ###rare in the treatment groups, 5 treated subject had lung cancer
table(dataOR$treat, dataOR$cat1_colon)  ###rate in the treatment groups, 1 treated subject had colon cancer
table(dataOR$treat, dataOR$cat1_arf)

table(dataOR$treat, dataOR$cat2_mosfsep)
table(dataOR$treat, dataOR$cat2_coma)
table(dataOR$treat, dataOR$cat2_mosfmal)
table(dataOR$treat, dataOR$cat2_lung)  ###rare in the treatment group, 2 treated subjects had lung cancer
table(dataOR$treat, dataOR$cat2_cirr)
table(dataOR$treat, dataOR$cat2_colon) ####rate in both groups, only 1 subject in each had colon cancer
table(dataOR$treat, dataOR$cat2_none) 

table(dataOR$treat, dataOR$ca_yes)
table(dataOR$treat, dataOR$ca_meta)

table(dataOR$treat, dataOR$cardiohx)
table(dataOR$treat, dataOR$chfhx)
table(dataOR$treat, dataOR$dementhx)
table(dataOR$treat, dataOR$psychhx)
table(dataOR$treat, dataOR$chrpulhx)
table(dataOR$treat, dataOR$renalhx)
table(dataOR$treat, dataOR$liverhx)
table(dataOR$treat, dataOR$gibledhx)
table(dataOR$treat, dataOR$malighx)
table(dataOR$treat, dataOR$immunhx)
table(dataOR$treat, dataOR$transhx)
table(dataOR$treat, dataOR$amihx)

table(dataOR$treat, dataOR$wt0)



table(dataOR$treat, dataOR$resp) ##respiratory diagnosis
table(dataOR$treat, dataOR$card)  ##cardiovscular diagnosis
table(dataOR$treat, dataOR$neuro)  ##neurological diagnosis
table(dataOR$treat, dataOR$gastr)  ##gastrointenstinal diagnosis
table(dataOR$treat, dataOR$renal)  ##renal diagnosis
table(dataOR$treat, dataOR$meta)  ##metabolic diagnosis
table(dataOR$treat, dataOR$hema)  ##hematological diagnosis
table(dataOR$treat, dataOR$seps)  ##sepsis diagnosis
table(dataOR$treat, dataOR$trauma)  ###trauma diagnosis
table(dataOR$treat, dataOR$ortho) ###orthopedic diagnosis




table(dataOR$treat, dataOR$ortho, dataOR$cat1_lung, dataOR$cat1_colon, dataOR$cat2_lung, dataOR$cat2_colon) 

#View(dataOR[which(dataOR$ortho=="Yes"), c("treat", "ortho", "cat1_lung", "cat1_colon", "cat2_lung", "cat2_colon")])


unique(dataOR[which(dataOR$treat==1), c("treat", "ortho", "cat1_lung", "cat1_colon", "cat2_lung", "cat2_colon")])

unique(dataOR$X[which(dataOR$treat==1 & dataOR$ortho=="Yes")])
unique(dataOR$X[which(dataOR$treat==0 & dataOR$ortho=="Yes")])

#> unique(dataOR$X[which(dataOR$treat==1 & dataOR$ortho=="Yes")])
#[1] 1615 3332 4677 5513
#> unique(dataOR$X[which(dataOR$treat==0 & dataOR$ortho=="Yes")])
#[1] 2718 3806 4769


table(dataOR$treat,dataOR$cat1_lung)
unique(dataOR$X[which(dataOR$treat==1 & dataOR$cat1_lung==1)])
#[1] 2611 2979 4864 5666 5704

table(dataOR$treat,dataOR$cat1_colon)
unique(dataOR$X[which(dataOR$treat==1 & dataOR$cat1_colon==1)])
unique(dataOR$X[which(dataOR$treat==0 & dataOR$cat1_colon==1)])
#> unique(dataOR$X[which(dataOR$treat==1 & dataOR$cat1_colon==1)])
#[1] 4783
#> unique(dataOR$X[which(dataOR$treat==0 & dataOR$cat1_colon==1)])
#[1]   92  861 2538 3170 3215 4776
 

table(dataOR$treat,dataOR$cat2_lung)
unique(dataOR$X[which(dataOR$treat==1 & dataOR$cat2_lung==1)])
#> unique(dataOR$X[which(dataOR$treat==1 & dataOR$cat2_lung==1)])
#[1] 1583 3400

table(dataOR$treat,dataOR$cat2_colon)
unique(dataOR$X[which(dataOR$treat==1 & dataOR$cat2_colon==1)])
unique(dataOR$X[which(dataOR$treat==0 & dataOR$cat2_colon==1)])
#> unique(dataOR$X[which(dataOR$treat==1 & dataOR$cat2_colon==1)])
#[1] 296
#> unique(dataOR$X[which(dataOR$treat==0 & dataOR$cat2_colon==1)])
#[1] 5655
#> 


######collapsing lung cancer and colon cancer together######
table(dataOR$treat,dataOR$cat1_cancer)
unique(dataOR$X[which(dataOR$treat==1 & dataOR$cat2_lung==1)])
#> unique(dataOR$X[which(dataOR$treat==1 & dataOR$cat2_lung==1)])
#[1] 1583 3400

table(dataOR$treat,dataOR$cat2_colon)
unique(dataOR$X[which(dataOR$treat==1 & dataOR$cat2_colon==1)])
unique(dataOR$X[which(dataOR$treat==0 & dataOR$cat2_colon==1)])
#> unique(dataOR$X[which(dataOR$treat==1 & dataOR$cat2_colon==1)])
#[1] 296
#> unique(dataOR$X[which(dataOR$treat==0 & dataOR$cat2_colon==1)])
#[1] 5655
#> 

varContin=  c("age", "edu","das2d3pc", "surv2md1", "aps1" , "scoma1","wtkilo1" , "temp1", "meanbp1", "resp1", "hrt1",
              "pafi1" , "paco21" ,"ph1", "wblc1", "hema1" , "sod1" ,"pot1","crea1",   "bili1", "alb1" )

####getting the t statistics#########
statResult=data.frame(var=propenVarList, tstats=NA)
for(k in 1:length(propenVarList)){
  
  print(k)
  
  if(propenVarList[k] %in% varContin){
    
    testR=t.test(dataOR[, propenVarList[k]] ~ dataOR[, "treat"])
    statResult$tstats[k]=as.numeric(testR$statistic)
    
  } else {
    
    testR=prop.test(table(dataOR[, propenVarList[k]], dataOR[, "treat"]), correct=FALSE)
    statResult$tstats[k]=sqrt(testR$statistic)
  }

}

statResult$tstatsAbs=abs(statResult$tstats)
sum(statResult$tstatsAbs>=2)
statResult$var[which(statResult$tstatsAbs>=2)]


#################################################################################################
#################################################################################################
###asymetric truncation, at quantile level
truncateVal=seq(0.01, 0.1, 0.01)
truncateQVal=seq(0, 0.03, 0.005)


########################
treat.varname = "treat"
outcome.varname = "y30"
modelSpec="all"
num.knot=30



#####################
treatID = which(dataOR[, treat.varname] == 1)
controlID = which(dataOR[, treat.varname] == 0)

####set initial outcome model
##might modify if some categorical variables do not have more than 1 level, remove those
outcomeVarList0=propenVarList[-c(which(propenVarList %in% c("wt0")))]
outcomeVarList1=propenVarList[-c(which(propenVarList %in% c("wt0")))]

###################################################
repNum=1000  ### number of multiple imputation
column.names=c("ATE",  "ATM", "ATM_w", "ATO",  "ATT", "ATT_w", "ATC", "ATC_w")
column.names2=paste0("truncate", truncateVal) 
column.names3=paste0("truncateQ", truncateQVal) 

row.names1=c("estimate", "var1", "var2", "var3")
matrix.names=paste0("boot", 1:repNum)

mulResultP=array(NA, dim = c(length(row.names1), length(column.names), repNum), dimnames = list(row.names1, column.names, matrix.names)) ###using truncated linear basis
mulResultP.truncate=array(NA, dim = c(length(row.names1), length(column.names2), repNum), dimnames = list(row.names1, column.names2, matrix.names)) ###using truncated linear basis
mulResultP.truncateQ=array(NA, dim = c(length(row.names1), length(column.names3), repNum), dimnames = list(row.names1, column.names3, matrix.names)) ###using truncated linear basis


for(k in 501:repNum){
  
      ##take a bootstrap sample, stratified bootstrap sampling
      set.seed(k)
      data = dataOR[c( sample(treatID, replace = T), sample(controlID, replace = T) ),]  
      
      ###some rare counts in treated or control groups
      ###check if the level does not exist, remove from the model
      checkVar=c("ortho", "cat1_lung", "cat1_colon", "cat2_lung", "cat2_colon")
      rmVar0=NULL
      rmVar1=NULL
      for(kvar in 1:length(checkVar)){
        
        temp00=sum(data$treat==0 & data[, checkVar[kvar]]==0)
        temp01=sum(data$treat==0 & data[, checkVar[kvar]]==1)
        
        temp10=sum(data$treat==1 & data[, checkVar[kvar]]==0)
        temp11=sum(data$treat==1 & data[, checkVar[kvar]]==1)
        
        
        if(temp00==0 | temp01==0){
          rmVar0=c(rmVar0, checkVar[kvar])
        }
        
        #########
        if(temp10==0 | temp11==0){
          rmVar1=c(rmVar1, checkVar[kvar])
        }
        
      }
      
      if(length(rmVar0)!=0){
        outcomeVarList0=propenVarList[-c(which(propenVarList %in% c("wt0", rmVar0)))]
      }
      
      if(length(rmVar1) != 0){
        outcomeVarList1=propenVarList[-c(which(propenVarList %in% c("wt0", rmVar1)))]
      }
      
      tempR=pencomp(dataOR, data, propenVarList, outcomeVarList0=outcomeVarList0, outcomeVarList1=outcomeVarList1,
                    treat.varname, outcome.varname, truncateQVal, truncateVal, num.knot)
    
      print(tempR)
      
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
ATE_raw=cbind(mulResultP[1,1,], mulResultP[2,1,], mulResultP[3,1,], mulResultP[4,1,])
ATM_raw=cbind(mulResultP[1,2,], mulResultP[2,2,], mulResultP[3,2,], mulResultP[4,2,])
ATM_w_raw=cbind(mulResultP[1,3,], mulResultP[2,3,], mulResultP[3,3,], mulResultP[4,3,])
ATO_raw=cbind(mulResultP[1,4,], mulResultP[2,4,], mulResultP[3,4,], mulResultP[4,4,])
ATT_raw=cbind(mulResultP[1,5,], mulResultP[2,5,], mulResultP[3,5,], mulResultP[4,5,])
ATT_w_raw=cbind(mulResultP[1,6,], mulResultP[2,6,], mulResultP[3,6,], mulResultP[4,6,])
ATC_raw=cbind(mulResultP[1,7,], mulResultP[2,7,], mulResultP[3,7,], mulResultP[4,7,])
ATC_w_raw=cbind(mulResultP[1,8,], mulResultP[2,8,], mulResultP[3,8,], mulResultP[4,8,])
write.table(ATE_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATE.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(ATM_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATM_raw.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(ATM_w_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATM_w.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(ATO_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATO.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(ATT_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATT.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(ATT_w_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATT_w.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(ATC_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATC.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(ATC_w_raw, paste(DIREC, DIREC_R, "/raw/pencompEst", "_", modelSpec, "_numknot", num.knot, "_ATC_w.txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")


for(k in 1:length(column.names2)){
  temp=cbind(mulResultP.truncate[1,k,], mulResultP.truncate[2,k,], mulResultP.truncate[3,k,], mulResultP.truncate[4,k,])
  write.table(temp, paste(DIREC, DIREC_R, "/raw/pencompEst.truncate", "_", modelSpec, "_numknot", num.knot, "_",column.names2[k], ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
}


for(k in 1:length(column.names3)){
  temp=cbind(mulResultP.truncateQ[1,k,], mulResultP.truncateQ[2,k,], mulResultP.truncateQ[3,k,], mulResultP.truncateQ[4,k,])
  write.table(temp, paste(DIREC, DIREC_R, "/raw/pencompEst.truncateQ", "_", modelSpec, "_numknot", num.knot, "_",column.names3[k], ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
}






##############################################################################################################
##############################################################################################################
###weighted estimators

####set initial outcome model
##might modify if some categorical variables do not have more than 1 level, remove those
outcomeVarList0=propenVarList
outcomeVarList1=propenVarList

est_weight=balanceWeight(data=dataOR, propenVarList, outcomeVarList0, outcomeVarList1, treat.varname, outcome.varname, truncateQVal, truncateVal)


#####################################################
repBoot=1000 ###number of bootstrap samples for weighted estimators 
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



for(k in 501:repBoot){
  ##take a bootstrap sample, stratified bootstrap sampling
  set.seed(k)
  data = dataOR[c( sample(treatID, replace = T), sample(controlID, replace = T) ),] 
  
  ###some rare counts in treated or control groups
  ###check if the level does not exist, remove from the model
  checkVar=c("ortho", "cat1_lung", "cat1_colon", "cat2_lung", "cat2_colon")
  rmVar0=NULL
  rmVar1=NULL
  for(kvar in 1:length(checkVar)){
    
    temp00=sum(data$treat==0 & data[, checkVar[kvar]]==0)
    temp01=sum(data$treat==0 & data[, checkVar[kvar]]==1)
    
    temp10=sum(data$treat==1 & data[, checkVar[kvar]]==0)
    temp11=sum(data$treat==1 & data[, checkVar[kvar]]==1)
    
    
    if(temp00==0 | temp01==0){
      rmVar0=c(rmVar0, checkVar[kvar])
    }
    
    #########
    if(temp10==0 | temp11==0){
      rmVar1=c(rmVar1, checkVar[kvar])
    }
    
  }
  
  if(length(rmVar0)!=0){
    outcomeVarList0=propenVarList[-c(which(propenVarList %in% c(rmVar0)))]
  }
  
  if(length(rmVar1) != 0){
    outcomeVarList1=propenVarList[-c(which(propenVarList %in% c(rmVar1)))]
  }
  
  
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









#########################################################################################################################
#########################################################################################################################
######## pencomp with just a spline on the propensity score, without covariates in outcome model ########################
modelSpec="null"
outcomeVarList0=NULL
outcomeVarList1=NULL


###################################################
repNum=50  ### number of multiple imputation
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
      
      tempR=pencomp(dataOR, data, propenVarList, outcomeVarList0=outcomeVarList0, outcomeVarList1=outcomeVarList1,
                    treat.varname, outcome.varname, truncateQVal, truncateVal, num.knot)
      
      print(tempR)
      mulResultP[,,k]=tempR$other
      mulResultP.truncate[,,k]=tempR$truncate
      mulResultP.truncateQ[,,k]=tempR$truncateQ
      
    }, error=function(e){"error"} )
  
}


pencompEst2=pencompEst
pencompEst.truncate2=pencompEst.truncate
pencompEst.truncateQ2=pencompEst.truncateQ


pencompEst=processPENCOMP(resultIn=mulResultP, estimand=c("ATE", "ATM","ATM_w", "ATO", "ATT", "ATT_w", "ATC", "ATC_w"))
pencompEst.truncate=processPENCOMP(resultIn=mulResultP.truncate, estimand=column.names2)
pencompEst.truncateQ=processPENCOMP(resultIn=mulResultP.truncateQ, estimand=column.names3)


sum(!is.na(mulResultP))
sum(is.na(mulResultP.truncate))
sum(is.na(mulResultP.truncate))



####output results
write.table(pencompEst, paste(DIREC, DIREC_R, "pencompEst", "_", modelSpec, "_numknot", num.knot, ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(pencompEst.truncate, paste(DIREC, DIREC_R, "pencompEst.truncate", "_", modelSpec, "_numknot", num.knot, ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")
write.table(pencompEst.truncateQ, paste(DIREC, DIREC_R, "pencompEst.truncateQ", "_", modelSpec, "_numknot", num.knot, ".txt", sep=""),row.names = TRUE, col.names=TRUE, quote = F, sep="\t")




hist(mulResultP[1,1,])



###### plot propensity score distributions#################################################
######## propensity score model ########################################################### 
propen.model=NULL
propen.model=formulaF(varList=propenVarList, y.name=treat.varname)


######## propensity score model ########################################################### 
model2a=glm(propen.model, data=dataOR, family="binomial", control = list(maxit = 50))
dataOR$pa=predict(model2a, newdata=dataOR, type="response")


##########################################################
propTreat=dataOR$pa
propControl=1-dataOR$pa
treatInd=dataOR[, treat.varname]

plot(density(propTreat[treatInd==1]))
lines(density(propTreat[treatInd==0]))
abline(v=qtreat[1])
abline(v=qtreat[2])


alpha.level=0.005  
qtreat=quantile(propTreat[treatInd==1], probs = c(alpha.level, 1-alpha.level))
qcontrol=quantile(propControl[treatInd==0], probs = c(alpha.level, 1-alpha.level))


######first time point 
set1a=which(propTreat >= qtreat[1] & propTreat <= qtreat[2])
set1b=which(propControl >= qcontrol[1] & propControl <= qcontrol[2])
setInt1=set1a[set1a %in% set1b]

c(length(setInt1)/nrow(dataOR), sum(treatInd[setInt1]),  sum( (treatInd[setInt1])==0 ))

