#### focus on the first cohort only, 1984-1985 cohorts
#### CREATED 6/19/2017
###including all visits between 7-21
### for survey questions or more informations about the study, visit this website https://statepi.jhsph.edu/macs/pdt.html


rm(list=ls())
DIREC="C:/Users/Tingting.Zhou/Desktop/paper2/Application/macs/rawDatasets/" ### location of raw datasets
DIRECOUT="C:/Users/Tingting.Zhou/Desktop/paper2/Application/macs/" ##location of where generated datasets will be stored


#############################################################################
#######HIV status
hivstats=read.csv(paste(DIREC, "hivstats.csv", sep=""), header=T)
length(unique(hivstats$CASEID))
names(hivstats)
dim(hivstats)  ###
length(unique(hivstats$CASEID))
table(hivstats$STATUS)

hivstats[, which(names(hivstats) %in% c("V1DATY", "NEGVIS", "POSVIS", "CASEID", "STATUS"))]
#1= Negative status                   
#2= Positive
#3= Positive, based on late update
#4= Converter
#5= Converter without known date of conversion
#6= Prevalent with known date of seroconversion

####subjects who were HIV+ #########

hivPos=hivstats[which(hivstats$STATUS!=1),]
table(hivPos$STATUS, hivPos$V2DATY)

sum(hivPos$V2DATY>=2000, na.rm = T)
table(hivPos$STATUS)
###2    4    5    6 
#3077  598   40  106 


################MACS ID PARTICIPANTS#################### 
###macsid.csv contains Participant IDs with Recruit status and DOB year
macsid=read.csv(paste(DIREC, "macsid.csv", sep=""), header=T)
names(macsid)
dim(macsid)  ###7343 subjects

####restrict to infected subjects only############
macsid=macsid[which(macsid$CASEID %in% hivPos$CASEID),]
macsid=merge(macsid, hivPos, by="CASEID", all.x = T)


####checking the codes
originalCohort=macsid$CASEID[macsid$MACSCODE==20 | macsid$MACSCODE==21]  ###1984-1985 cohorts, our analysis for the paper is restricted to this cohort only
newCohort1988=macsid$CASEID[macsid$MACSCODE==30 | macsid$MACSCODE==31 |  macsid$MACSCODE==36]  ###1988 new cohort
newCohort2001=macsid$CASEID[macsid$MACSCODE==40]  ###2001 new cohort
newCohort2010=macsid$CASEID[macsid$MACSCODE==50 | macsid$MACSCODE==55]  ###2010 new cohort


length(originalCohort)  
length(newCohort1988) 
length(newCohort2001)  
length(newCohort2010) 

table(macsid$STATUS02)  ###serostatus 2001-2003 recruits
#1: 2001-03 recruit seronegative
#2: 2001-03 recruit seropositive HAART-naive
#3: 2001-03 recruit seropositive HAART-using


table(macsid$STATUS10)  ###target enrollment group, 2010 recruits
#1: 2010 recruit seronegative
#2: 2010 recruit seroprevalent, ART-naive
#3: 2010 recruit, seroconverter (>1-5 years), ART-naive
#4: 2010 recruit, seroconverter (>1-5 years), ART-experienced
#5: 2010 recruit, incident SC (<= 1 year), ART-naive
#6: 2010 recruit, incident SC (<=1 year), ART-experienced
#7: 2010 recruit, seroprevalent, ART-experienced (begun 01/01/11 or later)

table(macsid$RTN)  ##5: returning censored seronegative

table(macsid$STATUS, macsid$STATUS02)
table(macsid$STATUS, macsid$STATUS10)


####################################################################################
############HIV status##############################################################
#########obtain HIV status from section 2 questionnaire
section2=read.csv(paste(DIREC, "section2.csv", sep=""), header=T)
length(unique(section2$CASEID))

######restrict to HIV positive subjects#############
section2=section2[which(section2$CASEID %in% hivPos$CASEID),]

dim(section2)
length(unique(section2$CASEID))  
table(section2$DAT2Y)  ###year of first visit 1984 (4172), 1985(782)
sum(is.na(section2$DAT2Y))  ###year of each visit
unique(section2$VISIT)  ##visit number
unique(section2$DAT2Y)  ##year of visit


section2$college=NA
section2$college[which(section2$EDUCA==5 | section2$EDUCA==6 | section2$EDUCA==7)]=1  ###have at least a college degree
section2$college[which(section2$EDUCA==1 | section2$EDUCA==2 | section2$EDUCA==3 | section2$EDUCA==4)]=0

section2$white=NA
section2$white[which(section2$RACE==1 | section2$RACE==2)]=1
section2$white[which(section2$RACE==3 | section2$RACE==4 | section2$RACE==5 | section2$RACE==6 | section2$RACE==7)]=0

section2$age=section2$DAT2Y-section2$BORNY ###age at each visit
table(section2$HELTH)  ###health status

###checking simple statistics
hist(section2$age)
summary(section2$age)
table(section2$BORNY)  ###one subject was born in year 2014


table(section2$BORNY)
section2$CASEID[which(section2$age<0)]  

length(unique(section2$CASEID[which(section2$white==0)]))   
length(unique(section2$CASEID[which(section2$white==1)]))  

length(unique(section2$CASEID[which(section2$college==0)]))   
length(unique(section2$CASEID[which(section2$college==1)]))  
unique(section2$VISIT)


############################################################################
############################################################################
#### antiretroval treatment ##############################################
drugf1=read.csv(paste(DIREC, "drugf1.csv", sep=""), header=T)
length(unique(drugf1$CASEID))
table(drugf1$AVQY)  ###date of visit, 
table(drugf1$VISIT)
table(drugf1$AVNW)  ##taking anti-viral drug now
table(drugf1$DRGAV)  ##anti-viral drug code

sum(!drugf1$CASEID %in% hivPos$CASEID)
sum(!hivPos$CASEID %in% drugf1$CASEID)

###taking only unique entries#######
drugf1_v2=drugf1[which(!duplicated(drugf1[,c("CASEID", "VISIT")])), c("CASEID", "VISIT","FORM", "AVQY")]


#View(table(drugf1$DRGAV, drugf1$VISIT))

table(drugf1$DRGAV, drugf1$AVQY)

##########treatment taken at each visit#########################
treatRegime=NULL
for(i in 1:nrow(drugf1_v2)){
  
  print(i)
  temp=drugf1[which(drugf1$CASEID==drugf1_v2$CASEID[i] & drugf1$VISIT==drugf1_v2$VISIT[i]),]
  temp2=data.frame(CASEID=drugf1_v2$CASEID[i], VISIT=drugf1_v2$VISIT[i])
  temp2$treatRegm=paste0(temp$DRGAV, collapse = "-")
  
  treatRegime=rbind(treatRegime, temp2)
  
}

table(drugf1_v2$treatInd)
table(treatRegime$VISIT)

x=table(treatRegime$treatRegm)
x[which(as.numeric(x)>=100)]

x=table(treatRegime$treatRegm[which(treatRegime$VISIT==130)])
x[which(as.numeric(x)>100)]



############################################################################
#######LAB TEST RESULTS#####################################################
#### obtain blood count measures from lab_rslt.csv
labTest=read.csv(paste(DIREC, "lab_rslt.csv", sep=""), header=T)
length(unique(labTest$CASEID))
labTest=labTest[which(labTest$CASEID %in% hivPos$CASEID),]   ###restrict to HIV positive subjects
names(labTest)
dim(labTest)

length(unique(labTest$CASEID))


summary(labTest$LEU3N) ###CD4 counts
labTest$WBC  ### white blood cell counts
labTest$RBC   ###red blood cell counts
labTest$PLATE   ###platelets
labTest$LEU2N ###CD8 counts
labTest$LEU3N ###CD4 counts
labTest$VLOAD


####################################################################
###########combine datasets: section2, treatRegime and labTest#################
dim(treatRegime)
length(unique(treatRegime$CASEID))

section2Pos=section2[which(section2$CASEID %in% hivPos$CASEID), c("CASEID","VISIT","DAT2Y", "college", "white", "age", "BORNY")]
section2Pos=merge(section2Pos, treatRegime, by=c("CASEID", "VISIT"), all.x = T)
section2Pos$treatRegm[which(is.na(section2Pos$treatRegm))]="none"



simdat=merge(section2Pos, labTest[, c("CASEID","VISIT","LEU3N","LEU2N", "WBC", "RBC", "PLATE", "VLOAD")], by=c("CASEID", "VISIT"), all.x = T)
simdat=merge(simdat, macsid[, c("CASEID", "BIRTHDTY")], by="CASEID", all.x = T)

simdat=merge(simdat, hivPos, by="CASEID", all.x = T)
simdat$hivStatus=0
simdat$hivStatus[which(simdat$STATUS==2)]=1
simdat$hivStatus[which(simdat$STATUS!=2 & simdat$VISIT >= simdat$POSVIS)]=1


########## the final dataset that we used in the analysis 
write.csv(simdat,paste(DIRECOUT, "simdat.csv", sep=""), row.names=F)

