#' pencomp Function
#'
#' This function allows you to calculate pencomp estimate on one dataset.
#' @param dataOR original dataset
#' @param propenVarList covariates included in the propensity score model
#' @param outcomeVarList0 covariates included in the outcome model for control group
#' @param outcomeVarList1 covariates included in the outcome model for treatment group
#' @param treat.varname variable name for treatment indicator
#' @param outcome.varname variable name for outcome
#' @param original indicator for whether models are fitted on a bootstrap sample or on the original dataset, default is on bootstrap sample
#' @param numKnot number of knots places on the splines, used trucated linear bases in this project 
#' @keywords pencomp
#' @export
#' @examples
#' pencomp()


########################################################################################
########################################################################################
#dataOR=mydata

#propenVarList=c("x1", "x2", "x3", "x4", "x5", "x6")
#outcomeVarList0=c("x1", "x2", "x3", "x4", "x5", "x6")
#outcomeVarList1=c("x1", "x2", "x3", "x4", "x5", "x6")
#treat.varname = "Z0"
#outcome.varname = "y"

#original=1
#numKnot=20

###truncateQval, truncateVal, truncation values


pencomp = function(dataOR, data, propenVarList, outcomeVarList0, outcomeVarList1, treat.varname, outcome.varname, truncateQVal, truncateVal) {
  tryCatch ( 
    {
      
      treatID = which(dataOR[, treat.varname] == 1)
      controlID = which(dataOR[, treat.varname] == 0)
      
      
      treatInd = dataOR[, treat.varname]  ###indicator of treated subjects in the original dataset
      treatBoot = data[, treat.varname]  ###indicator of treated subjects in the bootstrap sample
      Yobs = dataOR[, outcome.varname]
      
      ######## propensity score model ########################################################### 
      propen.model=NULL
      propen.model=formulaF(varList=propenVarList, y.name=treat.varname)
      
      
      ######## propensity score model ########################################################### 
      model2a=glm(propen.model, data=data, family="binomial", control = list(maxit = 50))
      prob.boot=predict(model2a, newdata=data, type="response")
      temp2a=NULL
      temp2a=as.numeric(treatBoot==1)*prob.boot + as.numeric(treatBoot==0)*(1-prob.boot)  ###estimate propensity of observed treatment
      data$pslogit = log(temp2a/(1-temp2a))   ####spline on the logit of propensity
      
      
      ####fit the prediction model for all the models
      ###for the outcome model separate model y0 and y1
      ##############################################################
      ##############################################################
      ###use equally spaced fixed knots assuming K knots
      data0=data[data[, treat.varname]==0,]
      
      ###imputing the missing potential outcomes
      newData0=dataOR
      newData0[, treat.varname]=0
      newData0$propControl=1 - predict(model2a, newdata=newData0, type="response")
      newData0$pslogit=log(newData0$propControl/(1-newData0$propControl))
      
      
      imputed0_fit = imputeF(dataFit=data0, newdata=newData0, outcome.varname=outcome.varname, outcomeVarList=outcomeVarList0)
      pspp0=imputed0_fit$modelOut
      imputed0=imputed0_fit$imputed
      
      
      
      ##########################################################################################################################
      ###prediction model for Y under treatment using treated data
      data1=data[data[, treat.varname]==1,]
      
      ###imputing the missing potential outcomes
      newData1=dataOR  ###imputation is done on the original dataset
      newData1[, treat.varname]=1
      newData1$propTreat=predict(model2a, newdata=newData1, type="response")
      newData1$pslogit=log(newData1$propTreat/(1-newData1$propTreat))
      
      imputed1_fit = imputeF(dataFit=data1, newdata=newData1, outcome.varname=outcome.varname, outcomeVarList=outcomeVarList1)
      pspp1=imputed1_fit$modelOut
      imputed1=imputed1_fit$imputed
      
      
      ##############keep the observed outcome the same#####################
      imputed1[which(treatInd==1)]=Yobs[which(treatInd==1)]
      imputed0[which(treatInd==0)]=Yobs[which(treatInd==0)]
      
      #######include everyone
      ATE=c(mean(imputed1-imputed0), var(imputed1-imputed0)/nrow(dataOR) )
      
      
      ###### alternative estimands ############
      #########truncate at quantile level######
      
      truncateQ=NULL
      for(qval in truncateQVal){ 
        
        qtreat=quantile(newData1$propTreat[treatInd==1], probs = c(qval, 1-qval))
        qcontrol=quantile(newData0$propControl[treatInd==0], probs = c(qval, 1-qval))
        
        ######first time point 
        setInt1=NULL
        set1a=which(newData1$propTreat >= qtreat[1] & newData1$propTreat <= qtreat[2])
        set1b=which(newData0$propControl >= qcontrol[1] & newData0$propControl <= qcontrol[2])
        setInt1=set1a[set1a %in% set1b]
        
        truncateQ=cbind(truncateQ, c(mean((imputed1-imputed0)[setInt1]), var((imputed1-imputed0)[setInt1])/length(setInt1)))
        
      }
      
      
      ###########################################################################
      #########truncate at propensity alpha level#####################
      
      truncate=NULL
      for(qval in truncateVal){ 
        
        setInt1=NULL
        setInt1=which(newData1$propTreat >= qval & newData1$propTreat <= 1-qval)
        truncate=cbind(truncate, c(mean((imputed1-imputed0)[setInt1]), var((imputed1-imputed0)[setInt1])/length(setInt1)))
        
      }
      
      
      ###restrict to the match pairs #####################
      ########propensity on the original dataset############################
      pslogit = predict(model2a, newdata=dataOR)
      
      ####### nearest neighbor matching without replacement within caliper width of 0.25 standard deviation of logit of propensity score
      matchset=pairMatch(pslogit=pslogit, dataOR=dataOR, treat.varname=treat.varname)
      inclusion=c(matchset$treatID, matchset$controlID)  ###the matched treated and control pairs
      
      
      #######################ATM#############################
      ###using matched weights###############################
      ############## ATM matching weights ###################
      probT=NULL
      probT=predict(model2a, newdata=dataOR, type="response")
      weight=NULL
      ATMWeight = (1-probT)*as.numeric( probT>0.5 ) + probT * as.numeric( probT<=0.5 )
      ATMWeight_std=ATMWeight/sum(ATMWeight)
      ATM2=NULL
      ATM2=c(sum(ATMWeight_std*(imputed1-imputed0)), var(imputed1-imputed0) * sum(ATMWeight_std^2))
      
      
      ###restricting to matched set
      ATM=NULL
      ATM=c(mean(imputed1[inclusion] - imputed0[inclusion]), var(imputed1[inclusion] - imputed0[inclusion])/length(inclusion))
      
      
      
      ##################### by weighting the treated and control different to obtain ATO estimand###########
      ATOWeight=(1-predict(model2a, newdata=dataOR, type="response")) * predict(model2a, newdata=dataOR, type="response")
      ATOWeight_std=ATOWeight/sum(ATOWeight)
      ATO=c(sum(ATOWeight_std *(imputed1-imputed0)), var(imputed1-imputed0) * sum(ATOWeight_std^2))
      
      
      
      ###############################ATT ##########################
      setInt1=NULL
      setInt1=which(treatInd==1)
      ATT=NULL
      ATT=c(mean((imputed1-imputed0)[setInt1]),var((imputed1-imputed0)[setInt1])/length(setInt1))
      
      
      ###############################using ATT weights##########################   
      probT=NULL
      probT=predict(model2a, newdata=dataOR, type="response")
      weight=NULL
      ATTWeight = probT
      ATTWeight_std=ATTWeight/sum(ATTWeight)
      ATT2=NULL
      ATT2=c(sum(ATTWeight_std*(imputed1-imputed0)), var(imputed1-imputed0) * sum(ATTWeight_std^2))
      
      
      ###############################ATC #####################################
      setInt1=NULL
      setInt1=which(treatInd==0)
      ATC=NULL
      ATC=c(mean((imputed1-imputed0)[setInt1]),var((imputed1-imputed0)[setInt1])/length(setInt1))
      
      
      
      ###############################using ATC weights##########################   
      probT=NULL
      probT=predict(model2a, newdata=dataOR, type="response")
      weight=NULL
      ATCWeight = (1-probT) 
      ATCWeight_std=ATCWeight/sum(ATCWeight)
      ATC2=NULL
      ATC2=c(sum(ATCWeight_std*(imputed1-imputed0)), var(imputed1-imputed0) * sum(ATCWeight_std^2))
      
      
      colnames(truncate)=paste0("truncate", truncateVal)
      colnames(truncateQ)=paste0("truncateQ", truncateQVal)
      
      
      return( list(other=cbind(ATE=ATE, ATM=ATM, ATM_w=ATM2, ATO=ATO, ATT=ATT, ATT_w=ATT2,
                               ATC=ATC, ATC_w=ATC2),
                   truncate=truncate, truncateQ=truncateQ)) ###estimate and variance of ATE
    }, error=function(e) return(NA) )
}



