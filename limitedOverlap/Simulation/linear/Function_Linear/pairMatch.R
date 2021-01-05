##pair matching based on logit of propensity score
###pslogit: logit of probability of being treated
###dataOR, original dataset, not the bootstrap dataset
###treat.varname, variable name of treatment indicator

pairMatch=function(pslogit, dataOR, treat.varname){
  
  ####### nearest neighbor matching without replacement within caliper width of 0.25 standard deviation of logit of propensity score
  caliper=0.25*sd(pslogit)
  ID=c(1:dim(dataOR)[1])
  
  treatGroup=ID[dataOR[,treat.varname]==1]  ###treated ID
  controlGroup=ID[dataOR[, treat.varname]==0]  ###control ID
  
  subsetT=data.frame(pslogit=pslogit[dataOR[,treat.varname]==1], ID=treatGroup)  ###treated samples
  subsetC=data.frame(pslogit=pslogit[dataOR[,treat.varname]==0], ID=controlGroup)  ###control samples
  
  ###randomly order treated subjects
  treatRand=sample(treatGroup,length(treatGroup))
  matchedPair=numeric(0)
  
  for (g in 1:length(treatRand)) {
    treatPropensity = subsetT$pslogit[which(subsetT$ID == treatRand[g])] 
    absDiff = abs(subsetC$pslogit-treatPropensity)
    index = which(absDiff <= caliper)  ###controls that are within the caliper width
    index2 = index[!(index %in% matchedPair)]
    if (length(index2) > 0) {
      chosen = which(absDiff == min(absDiff[index2]))[1]	###control that is closest to the treated
      matchedPair[g] = chosen
    } else {
      matchedPair[g] = 0
    }
  }
  
  matched=data.frame(treatRand, matchedPair)
  names(matched)=c("treatID", "controlIndex")
  matched2=matched[matched$controlIndex != 0,] ###remove unmatched treated subject
  matched2$controlID=subsetC$ID[matched2$controlIndex]
  
  return(matchset=matched2)
}