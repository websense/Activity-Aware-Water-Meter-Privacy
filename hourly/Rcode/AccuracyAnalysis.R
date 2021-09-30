## AccuracyAnalysis
## Calculate accuracy (matthews correlation coefficient) for activity recognition 
## for all activities and aggregations
## Input: allAR.rds, allGT.rds
## Output: accuracy table
## Author: Rachel Cardell-Oliver
## Version: Sep 2021
 

{
  source("./ConfigurationSettings.R")
  allAR = readRDS(file="../Rdata/allAR.rds")
  allGT = readRDS(file="../Rdata/allGT.rds")
}


## Function to calculate accuracy (several metrics including mcc)
{
  #compare ground truth and prediction
  #both are 0,1 (boolean) vectors for corresponding set of days and meters
aggregatedActivityAccuracy <- function(gt,ar)
{
  TP = sum(gt & ar)
  FP = sum(!gt & ar)
  TN = sum(!gt & !ar) 
  FN = sum(gt & !ar)

  #Defns https://en.wikipedia.org/wiki/Precision_and_recall
  precision = TP/(TP+FP)
  recall  = TP/(TP+FN) 
  TPR = TP/(TP+FN) #true positive rate is recall
  TNR = TN/(TN+FP) #true negative rate
  balancedaccuracy = (TPR+TNR)/2
  mcc = mcc(TP=TP, FP=FP, TN=TN, FN=FN)
  fmeasure = (2*precision*recall)/(precision+recall)
  res = c(TP,FP,FN,TN,precision,recall,balancedaccuracy,fmeasure,mcc)
  names(res)=c("TP","FP","FN","TN","precision","recall",
               "balancedaccuracy","fmeasure","MCC")
  return(res)
}
}

## Accuracy Calculations for all activities and aggregations
{
  ## get accuracies for all activities
  accresall = c()
  for (ai in 1:3) { #3 activities
    for (agg in agghours) { #all aggregations
      gt = c(allGT[[ai]][[agg]])  #ground truth activity
      ar = c(allAR[[ai]][[agg]])  #activity recognised by rules
      accresall = rbind(accresall, 
                        c(ai,agg,aggregatedActivityAccuracy(gt,ar)))
    }
  }
  colnames(accresall)[1:2] = c("ActivityID","AggHours")
  
  #save for automating privacy settings later
  saveRDS(accresall,file="../Rdata/accresall.rds")
}



