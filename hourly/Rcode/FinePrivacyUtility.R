## Privacy vs Utility code
## for STREAM fine grained data by Harrison
## RCO 23 June 2021
## Updated Sep 2021 for BuildSys paper final version

# Libraries
{
  source("./ColoursandNames.R")
  require(data.table)
  require(RColorBrewer)
  require(xtable)
}

# Read synthesised data from Fdata directory
{
  #rotate all for 199 meters over 60K time stamps
  
  raw = t(read.csv("../Fdata/raw.txt",header=FALSE))
  taps = t(read.csv("../Fdata/Faucet.txt",header=FALSE))
  toilet = t(read.csv("../Fdata/Toilet.txt",header=FALSE))
  shower = t(read.csv("../Fdata/Shower.txt",header=FALSE))
  dishwasher = t(read.csv("../Fdata/Dishwasher.txt",header=FALSE))
  clotheswasher = t(read.csv("../Fdata/Clotheswasher.txt",header=FALSE))
  
  #assign timetamps
  finetimestamps = (1:60480)*10 #10 second time stamps
  rownames(raw)=finetimestamps
  rownames(taps)=finetimestamps
  rownames(toilet)=finetimestamps
  rownames(shower)=finetimestamps
  rownames(dishwasher)=finetimestamps
  rownames(clotheswasher)=finetimestamps

  NUM_FINE_HH = dim(raw)[2]
  fineactivitydata = list(taps,toilet,shower,dishwasher,clotheswasher,raw)
}



## make activity GT for different aggregations
## makes a list (activities) of lists (aggregations)
{

  #Input: activityAR = fineactivitydata[[ai]]
  #Input: agglist = aggselection, temporal aggregations
  #Output: list of 1,0 matrices of temporal agg upscaled GT volumes
  makeFineActivityGT <- function(activityAR, agglist)
  {
    activityGT = list()
    #ground truth time stamps with 1 unit = 10seconds
    gtts = as.numeric(rownames(activityAR))/10 
    for (a in agglist) {
      if (a==1) { #take 10 second AR data as ground truth
        activityGT[[1]] = (activityAR>0)*1
      } 
      else {
        # upscale for periods from GT
        pbounds = which(gtts %% a == 0) #index of period bounds in GT
        #make raw matrix for agg GT
        agt = matrix(nrow=length(pbounds)-1,ncol=dim(activityAR)[2]) 
        nperiods = dim(agt)[1]
        for (i in 1:nperiods) {
          oneperiod = activityAR[pbounds[i]:(pbounds[i]+a-1),]
          # activity true at agg scale if at least one event occurs within agg period
          agt[i,] = (apply(oneperiod,2,max)>0)
        }
        activityGT[[a]] = agt*1
      }
    }
    return(activityGT)
  }
  
  fineactivityGT=list()
  for (ai in 1:6)
  {
  fineactivityGT[[ai]] = 
    makeFineActivityGT(fineactivitydata[[ai]], 
                 aggselection)
  }
  
  saveRDS(fineactivityGT,file="../Rdata/fineactivityGT.rds")
}

## ACTIVITY RECOGNITION ACCURACY

{
  #input: base granularity volume and some aggregation
  #return: aggregated volume version of volumegt
  upscaleFineVolume <- function( volumegt, agg)
  {
    if (agg==1) { return(volumegt) } #base case
    #else perform aggregation from 10 second data
    timestamp = as.numeric(rownames(volumegt))/10
    baseagg = floor(timestamp/agg)*agg 
    uha = unique(baseagg)
    dagg = matrix(0,nrow=length(uha),ncol=dim(volumegt)[2])
    rownames(dagg) = uha*10 #seconds
    for (i in 1:length(uha)) {
      #if at least one activity hour in the period then mark agg period with activity
      pos = which(baseagg==uha[i])
      if (length(pos)==agg) { #only label full aggregated periods with the total volume in that period, should always be true
        dagg[i,] = colSums(volumegt[pos,]) 
      }
    }
    return(dagg)
  }
  
 #per activity, per aggregation learn volumes
  aggFineVolume = list()
  for (ai in 1:6) 
  {
    agv = list()
    for (agg in aggselection) {  
        agv[[agg]] = upscaleFineVolume( fineactivitydata[[ai]], agg)
    }
    aggFineVolume[[ai]] = agv
  }

}

## Accuracy for fine grain
## find # concurrent activities per time
## then calculate BA (balanced accuracy) for these
## TP when only one activity in period and TN when 0
## but what about FPs or FNs when there are multiple activities?
## Assumption: if multiple activities then assume FN (can't identify)
## but what about assigning the wrong label? (FP)
## In our assum TNR is 1.0 because FP=0 but TPR falls because of high FNs
## this is worst case since FN more sig for small case of Positives
## negative errors not so bad because Negatives are the unbalanced dom case

## generate fineactivityAR per activity list of per agg list of 0 or 1 activity recognised
{
  #count of concurrent activities per HH per aggregation 
  concurrentactivities = list() 
  for (i in 1:6) #aggregation 
  {
    agg=aggselection[i] #time period
    #any activity (except All (ai=6)!)
    concurrentactivities[[agg]] = fineactivityGT[[1]][[agg]]
    for (ai in 2:5) 
    {
      aimatrix = fineactivityGT[[ai]][[agg]] #activity present
      concurrentactivities[[agg]] = concurrentactivities[[agg]]+aimatrix
    }
  }
  


  
  ## for each activity and for each aggregation learn AR
  ## uses Nguyen 82% assumed accuracy for overlapping events
  ## and assumes perfect recognition for others
  ## allow better for clothes washer because distinctive
  ## use Nguyen Tabl 13 disagg accuracy per event
  ##  "Taps"          "Toilet"        "Shower"        "Dishwasher"    
  ## "Clotheswasher" "All"
  fineactivitydisagg = c(0.871, 0.821, 0.783, 0.833, 0.897, 0.82)
  
  fineactivityAR=list()
  for (ai in 1:6) {
    fineactivityAR[[ai]] = list()
    for (gi in 1:6) {
      agg=aggselection[gi] #time period
      #same as GT 0,1
      thisact = fineactivityGT[[ai]][[agg]] 
      #except, concurrency>1 make FN (can't recognise activity if multi )
      notmulti = (concurrentactivities[[agg]] <= 1)

      #according to Nguyen2013 best practice HMM alg, 82% accuracy for mult events
      #(type1 is better, but we'll take 82 as an avg)
      # using disagg per event from table 13 but only when overlap
      #overlapping activity periods for this activity
      ismulti = (concurrentactivities[[agg]] > 1 & thisact==1)*1
      isincorrect = sample(which(ismulti==1), 
                           round((1-fineactivitydisagg[ai])*sum(ismulti)))
      correct=ismulti
      correct[isincorrect]=0 #fail to recognise these

      fineactivityAR[[ai]][[agg]] = ((thisact  * notmulti + correct)>0)*1
    }
  }
  saveRDS(fineactivityAR,file="../Rdata/fineactivityAR.rds")
} 


# fineactivityAR 1:6 activities (6 is all) 
# then 6 aggregations from aggselection[i]
# assumes AR fails whenever concurrentactivities > 1 else GT
# maybe ambitious for distinctive sigs but vols probably oneweekdays
# checked they agree reasonably with Cominola and Carboni
## comparison: fineactivityGT and AR for accuracy
{
  fineactivityAR = readRDS(file="./fineactivityAR.rds")
  fineactivityGT = readRDS(file="./fineactivityGT.rds")
  {
  res = c()
  actsummary = c()
  for (ai in 1:6) 
    {
    for (gi in 1:6) {
    agg = aggselection[gi]
    gt = c(fineactivityGT[[ai]][[agg]])
    at = c(fineactivityAR[[ai]][[agg]])
    ct = c(concurrentactivities[[agg]])
    ## per activity stats
    {
      nacts = sum(gt)
      nperiods = length(gt)
      vol = sum(c(fineactivitydata[[ai]]))
      mix = table(ct[which(gt==1)])
      actsummary = rbind(actsummary, c(ai,gi,nacts,nperiods,vol,
                                       vol/nacts,round(100*nacts/nperiods),mix))
    }

    TN = sum(gt==0 & at==0) #no activity occuring or recognised
    FN = sum(gt==1 & at==0) #FN cant occur because neg is 0 always
    TP = sum(gt==1 & at==1)  #why is TP 0
    #if only 1 activity assume we can identify it BUT would fail for some grains
    FP = sum(gt==0 & at==1)   #cant identify activities if there is an overlap so assume we get it wrong
    precision = TP/(TP+FP)
    recall  = TP/(TP+FN) 
    TPR = TP/(TP+FN) #true positive rate is recall
    TNR = TN/(TN+FP) #true negative rate
    balancedaccuracy = (TPR+TNR)/2
    mcc = mcc(TP=TP, FP=FP, TN=TN, FN=FN)
    fmeasure = (2*precision*recall)/(precision+recall)
    res = rbind(res,
      c(ai,agg,TP,FP,FN,TN,precision,recall,balancedaccuracy,fmeasure,mcc))
  }
    }
  
  colnames(res)=c("Activity","Agg","TP","FP","FN","TN","precision","recall",
               "balancedaccuracy","fmeasure","MCC")
  res=as.data.table(res)
  colnames(actsummary)=c("Activity","Agg","ActivePeriods","AllPeriods",
                         "ActTotalVol",
                         "ActAvgVolperPeriod",
                         "ActivityPctofPeriods",
                         paste("C",1:4,sep=""))
  }
  saveRDS(res,file="../Rdata/finegrainedaccuracy.rds")
}

## function for unicity for fine grained activities
{
## similar to SideChannelAnalysis but without the 1 per day rule
fineunicity <- function(selectedgt, #ground truth for prior knowledge
                    selectedar, #published data with AR for attack matching
                    agg, #which temporal aggregation is being used
                    PK=MAXPK, NTRIES=10)
{
  N = dim(selectedar)[2] #number of hhs in the safe DS
  M = dim(selectedar)[1] #number of periods
  compromised = matrix(0,nrow=NTRIES,ncol=PK) #to be filled
  candidates = compromised #to be filled 
  
  for (k in 1:PK) {
    for (i in 1:NTRIES) {
      ck = 0 #count of compromised hhs for k, varies per try
      nc = N #baseline count of attackable households for this k
      # some HHs have none or <k activities found so num candidates may be lower
      for (h in 1:N) {
        #choose k activity points at random from the GT
        #points should be on separate days (must be for leaks and occ, so do for habits too)
        ap = which(selectedgt[,h]==1)
        if ( length(ap) < k) { #not enough matches for this hh
          nc = nc-1  
        } else { # we have enough (length(ap) >= k) 
          # choose any k activity occurrences to be matched
          sp = sample(ap,k) 
          #compare h with population on those points
          #how many same as h - if 1 then h only so h is exposed
          # if >1 then h is safe
          # how many houses share the same k occurrences
          # in their recorded activities?
          if (k==1) {
            matchcount = sum(selectedar[sp,]==k)
          } else {
            matchcount = length(which(colSums(selectedar[sp,])==k))
          }
          #if match is unique for this hh and k attack
          # then add h to the count of compromised hhs
          if (matchcount == 1) { ck = ck+1 }
          #else if matchcount>1 then h is safe for this choice of k
        }
      }
      compromised[i,k] = ck
      candidates[i,k] = nc
    }
    
  }
  colnames(compromised)=paste("pk",1:PK,sep="")
  rownames(compromised)=paste("trial",1:NTRIES,sep="")
  return(list(compromised,candidates)) 
}
}

## calculate unicity for all acts at at all aggregations
{
  fineuuAll = list()
  for (ai in 1:5) {
    fineuuAll[[ai]]=list()
    for (aggi in aggselection) {
      activitygt = (fineactivityGT[[ai]][[aggi]])*1 #ground truth
      activityar = (fineactivityAR[[ai]][[aggi]]>0)*1 #recognised activity
      
      fineuuAll[[ai]][[aggi]] = fineunicity(activitygt, activityar, aggi, MAXPK, 10)
    }
  }
  saveRDS(fineuuAll,file="../Rdata/fineuuAll.rds")
}
  
  ## make 6 fig files on fine grained unicity
  {
    
    fineuuAll =  readRDS(file="../Rdata/fineuuAll.rds")
    #ai activity ID
    #upchoice 3 indices from uplist eg 1,7,9
    #ub upper limit for bar graph
    #lcols,lnames finegrained activity names and cols for selected upchoice
    #lcols=fineactivitycols
    #lnames=fineactivitycols
    #    title = paste(aggselectionminutes[gi],"Minute Aggregation")
    showUnicityAgg2 <- function(title,upchoice,uplist,gi,MAXK=6,ub=NUM_HH,lcols,lnames)
    {
      aggi = aggselection[gi]
      allmedians = c()
      for (ic in upchoice) {
        allmedians = rbind(allmedians,
                           apply(uplist[[upchoice[ic]]][[aggi]][[1]],2,median))
      }
      bpk=barplot(allmedians,
                  beside=TRUE,
                  col=lcols[upchoice],
                  main=title,
                  ylab="Unicity (HHs)",
                  ylim=c(0,ub),
                  names.arg=1:MAXK,xlab="Known Events")
      legend("top",lnames[upchoice],fill=lcols, ncol=3 )
      axis(4)
      grid()
      #show error bars
      for (ic in upchoice) {
        boxplot(uplist[[upchoice[ic]]][[aggi]][[1]],
                at=bpk[ic,],range=0,boxwex=0.5,xaxt="n",yaxt="n",add=TRUE)
      }
    }
    
    par(mfrow=c(3,2))
    for (gi in 1:6) #aggregations
    {
      fname = sprintf("../Rresults/unicity%dAllFineGrained.pdf",gi)
      pdf(file=fname,width=6,height=5)
      {
        showUnicityAgg2(paste(aggselectionminutes[gi],"aggregation"),
                        1:5,fineuuAll,gi,MAXK=6,ub=250,
                        lcols = fineactivitycols[1:5],
                        lnames = fineactivitynames[1:5])
      }
      dev.off()
    }
    par(mfrow=c(1,1))
}
