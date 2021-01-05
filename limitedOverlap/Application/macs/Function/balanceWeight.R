###############################################################################

#propenVarList=c("x1", "x2", "x3", "x4", "x5", "x6")
#outcomeVarList0=c("x1", "x2", "x3", "x4", "x5", "x6")
#outcomeVarList1=c("x1", "x2", "x3", "x4", "x5", "x6")
#treat.varname = "Z0"
#outcome.varname = "y"

#original=1


balanceWeight=function(data, propenVarList, outcomeVarList0, outcomeVarList1, treat.varname, outcome.varname, truncateQVal, truncateVal) {
  tryCatch ( 
    {
      
      treatInd = data[, treat.varname]  ###indicator of treated subjects
      Yobs = data[, outcome.varname]
      
      
      ######## propensity score model ########################################################### 
      ###fit the propensity, use them for all the estimates
      ##for propensity model
      propen.model=NULL
      propen.model=as.formula(paste(treat.varname, "~ ", paste(c(propenVarList), collapse = "+")))
      
      ######## propensity score model ##################################################################################
      ######## estimate propensity score model in the boostrap sample########################################################### 
      ps.fit=glm(propen.model, data=data, family="binomial", control = list(maxit = 50))
      probT=predict(ps.fit, newdata=data, type="response")

      ####fit the prediction model for all the models
      ###for the outcome model separate model y0 and y1
      modely0=NULL
      modely1=NULL
      
      
      if(length(outcomeVarList0)!=0){  ###has predictors
        modely0 = as.formula(paste(outcome.varname, "~ ", paste(c(outcomeVarList0), collapse = "+")))
      } else { ###no predictors
        modely0 = as.formula(paste(outcome.varname, "~ ", 1, collapse = "+"))
      }
      
      if(length(outcomeVarList0)!=0){  ###has predictors
        modely1 = as.formula(paste(outcome.varname, "~ ", paste(c(outcomeVarList1), collapse = "+")))
      } else {
        modely1 = as.formula(paste(outcome.varname, "~ ", 1, collapse = "+"))
      }
      

      ##############################################################
      ###prediction model for Y under control using observed data
      ###using mgcv package with B splines
      y0.model <- lm(modely0, data=data[treatInd==0,])
      #######################################################################
      ###prediction model for Y under treatment using treated data
      y1.model <- lm(modely1, data=data[treatInd==1,])
      
      pred0=predict(y0.model, newdata=data)
      pred1=predict(y1.model, newdata=data)
      
      
      ############## ATM matching weights ##################
      weight=NULL
      weightTop=(1-probT)*as.numeric( probT>0.5 ) + probT * as.numeric( probT<=0.5 )
      weight = weightTop/( treatInd*probT + (1-treatInd)*(1-probT) )
      
      ATM=sum( (Yobs*weight)[which(treatInd==1)] )/sum( weight[which(treatInd==1)] ) -
        sum( (Yobs*weight)[which(treatInd==0)] )/sum( weight[which(treatInd==0)] )
      
      ###doubly robust weighted estimator
      ATM.aug=sum( weightTop * (pred1 - pred0) ) / sum(weightTop) + sum( weight * treatInd * (Yobs - pred1) ) / sum(weight * treatInd) -
        sum( weight * (1-treatInd) * (Yobs - pred0) ) / sum(weight * (1-treatInd))

      
      ###########ATE##################
      weightTop=rep(1, length(probT))
      weight = weightTop/( treatInd*probT + (1-treatInd)*(1-probT) )
      
      ATE=sum( (Yobs*weight)[which(treatInd==1)] )/sum( weight[which(treatInd==1)] ) -
        sum( (Yobs*weight)[which(treatInd==0)] )/sum( weight[which(treatInd==0)] )
      
      ATE.aug=sum( weightTop * (pred1 - pred0) ) / sum(weightTop) + sum( weight * treatInd * (Yobs - pred1) ) / sum(weight * treatInd) -
        sum( weight * (1-treatInd) * (Yobs - pred0) ) / sum(weight * (1-treatInd))
      
       
      ############ATT#########################
      weightTop=NULL
      weight=NULL
      weightTop=probT
      weight = weightTop/( treatInd*probT + (1-treatInd)*(1-probT) )
      
      ATT=sum( (Yobs*weight)[which(treatInd==1)] )/sum( weight[which(treatInd==1)] ) -
        sum( (Yobs*weight)[which(treatInd==0)] )/sum( weight[which(treatInd==0)] )
      
      ATT.aug= sum( weightTop * (pred1 - pred0) ) / sum(weightTop) + sum( weight * treatInd * (Yobs - pred1) ) / sum(weight * treatInd) -
        sum( weight * (1-treatInd) * (Yobs - pred0) ) / sum(weight * (1-treatInd))
      
     
      ############ATC#########################
      weightTop=NULL
      weight=NULL
      weightTop=1-probT
      weight = weightTop/( treatInd*probT + (1-treatInd)*(1-probT) )
      
      ATC=sum( (Yobs*weight)[which(treatInd==1)] )/sum( weight[which(treatInd==1)] ) -
        sum( (Yobs*weight)[which(treatInd==0)] )/sum( weight[which(treatInd==0)] )
      
      ATC.aug=sum( weightTop * (pred1 - pred0) ) / sum(weightTop) + sum( weight * treatInd * (Yobs - pred1) ) / sum(weight * treatInd) -
        sum( weight * (1-treatInd) * (Yobs - pred0) ) / sum(weight * (1-treatInd))
      
      
      ############ATO#########################
      weightTop=NULL
      weight=NULL
      weightTop=(1-probT)*probT
      weight = weightTop/( treatInd*probT + (1-treatInd)*(1-probT) )
      
      ATO= sum( (Yobs*weight)[which(treatInd==1)] )/sum( weight[which(treatInd==1)] ) -
        sum( (Yobs*weight)[which(treatInd==0)] )/sum( weight[which(treatInd==0)] )
      
      ATO.aug=sum( weightTop * (pred1 - pred0) ) / sum(weightTop) + sum( weight * treatInd * (Yobs - pred1) ) / sum(weight * treatInd) -
        sum( weight * (1-treatInd) * (Yobs - pred0) ) / sum(weight * (1-treatInd))
      
       
      ############truncated weights################
      truncate=NULL
      truncate.aug=NULL
      truncate_rest=NULL
      truncate.aug_rest=NULL
      
      for(qval in truncateVal){
        
      weightTop=NULL
      weight=NULL
      weightTop=(probT >= qval & probT <= (1-qval))
      weight = weightTop/( treatInd*probT + (1-treatInd)*(1-probT) )
      
      truncate = c(truncate, sum( (Yobs*weight)[which(treatInd==1)] )/sum( weight[which(treatInd==1)] ) -
        sum( (Yobs*weight)[which(treatInd==0)] )/sum( weight[which(treatInd==0)] ) )
      
      truncate.aug=c(truncate.aug, sum( weightTop * (pred1 - pred0) ) / sum(weightTop) + sum( weight * treatInd * (Yobs - pred1) ) / sum(weight * treatInd) -
        sum( weight * (1-treatInd) * (Yobs - pred0) ) / sum(weight * (1-treatInd)) )
      
      
      #########re-estimated propensity scores, truncated weights##########
      ######## propensity score model ##################################################################################
      ps.fit_rest=glm(propen.model, data=data[which(weightTop),], family="binomial", control = list(maxit = 50))
      probT_rest=predict(ps.fit_rest, newdata=data, type="response")
      
      weight=NULL
      weight = weightTop/( treatInd*probT_rest + (1-treatInd)*(1-probT_rest) )
      
      truncate_rest= c(truncate_rest, sum( (Yobs*weight)[which(treatInd==1)] )/sum( weight[which(treatInd==1)] ) -
        sum( (Yobs*weight)[which(treatInd==0)] )/sum( weight[which(treatInd==0)] ) )
      
      truncate.aug_rest=c(truncate.aug_rest, sum( weightTop * (pred1 - pred0) ) / sum(weightTop) + sum( weight * treatInd * (Yobs - pred1) ) / sum(weight * treatInd) -
        sum( weight * (1-treatInd) * (Yobs - pred0) ) / sum(weight * (1-treatInd)) )
      
      }
      
 

      
      ############truncated weights truncation at quantile################
      truncateQ=NULL
      truncateQ.aug=NULL
      truncateQ_rest=NULL
      truncateQ.aug_rest=NULL
      
      for(qval in truncateQVal){   ###asymmetric can't trim too much, otherwise no samples left 
        
      weightTop=NULL
      weight=NULL
      
      #########truncate at quantile level#######
      qtreat=quantile(probT[treatInd==1], probs = c(qval, 1-qval))
      qcontrol=quantile((1-probT)[treatInd==0], probs = c(qval, 1-qval))
      
      ######first time point 
      setInt1=NULL
      set1a=which(probT >= qtreat[1] & probT <= qtreat[2])
      set1b=which((1-probT) >= qcontrol[1] & (1-probT) <= qcontrol[2])
      setInt1=set1a[set1a %in% set1b]
      
      weightTop=(1:length(probT)) %in% setInt1
      weight = weightTop/( treatInd * probT + (1-treatInd)*(1-probT) )
      
      truncateQ = c(truncateQ, sum( (Yobs*weight)[which(treatInd==1)] )/sum( weight[which(treatInd==1)] ) -
        sum( (Yobs*weight)[which(treatInd==0)] )/sum( weight[which(treatInd==0)] ) )
      
      truncateQ.aug= c(truncateQ.aug, sum( weightTop * (pred1 - pred0) ) / sum(weightTop) + sum( weight * treatInd * (Yobs - pred1) ) / sum(weight * treatInd) -
        sum( weight * (1-treatInd) * (Yobs - pred0) ) / sum(weight * (1-treatInd)) )
      
      
    
      #########re-estimated propensity scores, truncated weights##########
      ######## propensity score model ##################################################################################
      ps.fit_rest=glm(propen.model, data=data[which(weightTop),], family="binomial", control = list(maxit = 50))
      probT_rest=predict(ps.fit_rest, newdata=data, type="response")
      
      weight = weightTop/( treatInd * probT_rest + (1-treatInd) * (1-probT_rest) )
      
      truncateQ_rest= c(truncateQ_rest, sum( (Yobs*weight)[which(treatInd==1)] )/sum( weight[which(treatInd==1)] ) -
        sum( (Yobs*weight)[which(treatInd==0)] )/sum( weight[which(treatInd==0)] ) )
      
      truncateQ.aug_rest=c(truncateQ.aug_rest, sum( weightTop * (pred1 - pred0) ) / sum(weightTop) + sum( weight * treatInd * (Yobs - pred1) ) / sum(weight * treatInd) -
        sum( weight * (1-treatInd) * (Yobs - pred0) ) / sum(weight * (1-treatInd)) )
      
      }
      
      names(truncate)=names(truncate.aug)=names(truncate_rest)=names(truncate.aug_rest)=paste0("truncate", truncateVal)
      names(truncateQ)=names(truncateQ.aug)=names(truncateQ_rest)=names(truncateQ.aug_rest)=paste0("truncateQ", truncateQVal)
      
      return( list(other=c(ATE=ATE, ATE.aug=ATE.aug, ATM=ATM, ATM.aug=ATM.aug, ATT=ATT, ATT.aug=ATT.aug, ATC=ATC, ATC.aug=ATC.aug,
                ATO=ATO, ATO.aug=ATO.aug), 
                
                truncate=truncate, truncate.aug=truncate.aug,
                
                truncate_rest=truncate_rest, truncate.aug_rest=truncate.aug_rest,
                
                truncateQ=truncateQ, truncateQ.aug=truncateQ.aug,
                
                truncateQ_rest=truncateQ_rest, truncateQ.aug_rest=truncateQ.aug_rest  ) )
      
    }, error=function(e) return(NA) )
  
}
