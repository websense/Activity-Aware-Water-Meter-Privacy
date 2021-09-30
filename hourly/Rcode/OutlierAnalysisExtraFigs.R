## OutlierAnalysis
## Identify outlier households exposed by
## count of activities or activity frequecy at some time
## Input: kanon vector of k sizes and allAR to learn exposure of activities
## Output: list of outlier HHs to be pruned for each k,agg combination
## Author: Rachel Cardell-Oliver
## Version: Sep 2021

## S1. load libraries and data
{
source("./ConfigurationSettings.R")
##kanon = c(5,10,20,30,50,100) from ConfigurationSetttings.R
##agghours = c(1,2,3,4,6,8,12,24)  from ConfigurationSetttings.R
  allAR = readRDS(file="../Rdata/allAR.rds")
}

## TODO problems with k=300 eg 4291 excluded - whats happening??
## S2. safe k-anon: find exposed HHs to be removed
## countoutlierHHs = those with <k activities in histogram bin
## Reference: Li2012
{
  #set k,sampling rate b, eps and delta for safe k-anom, diff priv
  ## this uses bin size of sturges ##20, what if bin size 1 - is that safe?
  countoutlierres = c() #k*agg rows with number of outliers for later optimisation
  countoutlierslist = list() #list of list of outlierHHs
  for (k in kanon) {
    outliersagglist = list()
    for (agg in agghours) {
        countoutlierHHs = c()
        #count exposed HHs for each act and the (unique) total
        countperact = (1:4)*0 
        for (ac in 1:3) {
          # number of activities per HH (in total)
          actCountsPerHH = colSums(allAR[[ac]][[agg]]) 
          hh=hist(actCountsPerHH, 
                  #bin size by Sturges defauls
                  #breaks=0:max(actCountsPerHH), #bin size 1
                  #acperiodsintotal = 4296/acperiod
                  #binsize = 12
                  #breaks=(0:(acperiodsintotal+1/binsize))*binsize, #bin size 7 days (or hours)
                  #xlim=c(0,max(actCountsPerHH)),main="",
                  plot=FALSE)
          #title(paste(activitynames[ac],numexcl))
          #abline(h=k,lty="dotted")
          numexcl=sum(hh$counts[which(hh$counts<k)])
          ##identify which houses are in the <k bins
          #right=TRUE, the histogram cells are right-closed (left open) intervals.
          #but include 0 for first bracket
          hhexcl = c()
          if (numexcl>0) {
            nb=length(hh$breaks)
            lbs = hh$breaks[1:(nb-1)][which(hh$counts<k)]
            ubs = hh$breaks[2:(nb)][which(hh$counts<k)]
            bc = lbs*0
            for (b in 1:length(lbs)) {
              if (lbs[b]==0) {
                ainb = which(actCountsPerHH >= lbs[b] & actCountsPerHH <= ubs[b])
              } else {
                ainb = which(actCountsPerHH > lbs[b] & actCountsPerHH <= ubs[b])
              }
              bc[b] = length(ainb)
              hhexcl = c(hhexcl,ainb)
            }
           
            #print(rbind(lbs,ubs,bc,hh$counts[which(hh$counts<k)]))
            if (numexcl != length(hhexcl)) {
              print(sprintf("ERROR: %d != %d exclusions for k=%d agg=%d ac=%d",
                            numexcl,length(hhexcl),k,agg,ac))
            }
          }
          countperact[ac] = length(hhexcl)
          countoutlierHHs = c(countoutlierHHs,hhexcl)
        }
        countoutlierHHs = unique(sort(countoutlierHHs))
        countperact[4] = length(countoutlierHHs) #unique total
        countoutlierres = rbind(countoutlierres,
                           c(k,agg,countperact))
        outliersagglist[[agg]] = countoutlierHHs
    }
    countoutlierslist[[k]] = outliersagglist
    }
  colnames(countoutlierres)=c("k","AggHours",
                              activitynames,
                              "TotalHHExcl")
  
  #save for automating privacy settings later
  saveRDS(countoutlierres,file="../Rdata/countoutlierres.rds")
  saveRDS(countoutlierslist,file="../Rdata/countoutlierslist.rds")
}



## S3. safe k-anon: find exposed HHs to be removed
## timeoutlierHHs = those with <k activities at any time step
## Reference: Li2012
{
  #set k,sampling rate b, eps and delta for safe k-anom, diff priv
  ## this uses bin size of sturges ##20, what if bin size 1 - is that safe?
  timeoutlierres = c() #k*agg rows with number of outliers for later optimisation
  timeoutlierslist = list() #list of list of outlierHHs
  for (k in kanon) {
    outliersagglist = list()
    for (agg in agghours) {
      timeoutlierHHs = c()
      timeprunedperact = (1:4)*0 
      for (ac in 1:3) {
        #acperiod = agg 
        #acperiodsintotal = 4296/acperiod
        #binsize = 12
        # number of activities per HH (in total)
        HHCountsPerTime = as.vector(rowSums(allAR[[ac]][[agg]])) 
        exposedtimes=which(0<HHCountsPerTime & HHCountsPerTime<k)
        # identify which households are active at at least one of the exposed times
        hhexcl = which(colSums(allAR[[ac]][[agg]][exposedtimes,])>0)
        timeoutlierHHs = c(timeoutlierHHs,hhexcl)
        timeprunedperact[ac] = length(hhexcl)
      }
      timeoutlierHHs = unique(sort(timeoutlierHHs))
      timeprunedperact[4] = length(timeoutlierHHs)
      timeoutlierres = rbind(timeoutlierres,
                              c(k,agg,timeprunedperact))
      outliersagglist[[agg]] = timeoutlierHHs
    }
    timeoutlierslist[[k]] = outliersagglist
  }
  colnames(timeoutlierres)=c("k","AggHours",
                             activitynames,
                             "TotalExcl")
  
  #save for automating privacy settings later
  saveRDS(timeoutlierres,file="../Rdata/timeoutlierres.rds")
  saveRDS(timeoutlierslist,file="../Rdata/timeoutlierslist.rds")
}

## make all outliers list by merging time and count outlier households
{
  alloutlierslist = list()
  for (ka in kanon) {
    agglist = list()
    for (agg in agghours) {
      tout = timeoutlierslist[[ka]][[agg]]
      cout = countoutlierslist[[ka]][[agg]]
      agglist[[agg]] = unique(sort(c(tout,cout)))
    }
    alloutlierslist[[ka]] = agglist
  }
  saveRDS(alloutlierslist,file="../Rdata/alloutlierslist.rds")
}



## FIG FOR PAPER show the number that are pruned - combined outlier types
pdf("../Rresults/OutlierPrunedHHs.pdf",width=6,height=4.5) 
{
  alloutlierslist = readRDS(file="../Rdata/alloutlierslist.rds")
  
  nk = length(kanon)
  prunedcounts = matrix(0,nrow=4,ncol=nk)
  for(ki in 1:nk) {
    for (aj in 1:4) {
      prunedcounts[aj,ki] = length(alloutlierslist[[kanon[ki]]][[agghours[[aj]]]])
    }
  }
  barplot(prunedcounts,beside=TRUE,names.arg=kanon,
          col=aggcol[1:4],
          xlab="Privacy k-anonymity (k)",
          ylab="Number of Outlier HHs")
  axis(4)
  abline(h=(1:5)*500,lty="dotted",col="lightgray")
  legend("topleft",title="Aggregation",aggnames[1:4],
         fill=aggcol[1:4],bg="white",bty="n")
}
dev.off()


## show other way - number of inliers
if (runalltests) 
{
  pdf("../Rresults/OutlierRetainedHHs.pdf",width=6,height=4.5) 
  {
    reds=brewer.pal(9,"Reds")[9:2]
    blues=brewer.pal(9,"Blues")[9:2]
    plot(incvalC,type="b",pch=1,
         xaxt="n",ylim=c(500,NUM_HH),
         col=rep(blues,times=7),
         #main="Outlier Households",
         xlab="Privacy (k)",
         ylab="Utility (HHs retained)")
    axis(1,kanon,at=(1:7)*8-4) #,tick=FALSE)
    lines(incvalT,col=rep(reds,times=7),type="b",pch=4)
    abline(v=(1:7)*8,lty="dotted",col="gray")
    # legend("bottomleft", #this legend too clunky
    #        legend=c(paste("Counts",aggnames),
    #                 paste("Periods",aggnames)),
    #        title="K-anon (Agg.)",
    #        pch=c(rep(1,8),rep(4,8)),
    #        col=c(blues,reds),bg="white")
    legend("bottomleft",
           legend=c("Activity count","Per period"),
           title="Inlier HHs",
           pch=c(1,4),lty=c(1,1),
           col=c(blues[2],reds[2]),bg="white")
  }
  dev.off()
}

#privacy is k-crowd (bigger crowd is better) 
#vs utility (% of HHs retained) 
#If distortion (not retained) is too high then utility lost because it is not representative

if (runalltests) #REVISE THIS
{
  #only agg up to 4 hr because 6 and 24 too inaccurate
  # TODO maybe plot 1:5 ks with labels (ie eq space)
  pdf("../Rresults/OutliersPrivacyUtility.pdf",width=9,height=5) 
  
  par(mfrow=c(1,2))
  {
    {
      kval = timeoutlierres[,1]
      aggval = countoutlierres[,2]
      excval = countoutlierres[,6]
      incvalC = NUM_HH - excval
      plot(kval[1:8],incvalC[1:8],
           main="Count Outliers",
           xlab="Privacy (k)",
           ylab="Utility (HHs retained)",
           col=0,ylim=range(incval))
      for (i in 1:4) {
        lines(kval[which(aggval==agghours[i])],
              incvalC[which(aggval==agghours[i])],
              type="b",pch=as.character(agghours[i]),col=aggcol[i])
      }
      legend("bottomleft",title="Aggregation",
             aggnames[1:4],col=aggcol[1:4],
             pch=as.character(agghours[1:4]),
             lty=rep(1,times=4))
    }
    {
      kval = timeoutlierres[,1] #1:48
      aggval = timeoutlierres[,2]
      excval = timeoutlierres[,6]
      incvalT = NUM_HH - excval
      plot(kval[1:8],incvalT[1:8],col=0,
           main="Time Outliers",
           xlab="Privacy (k-crowd)",
           ylab="Utility (HHs retained)",
           ylim=range(incval))
      for (i in 1:4) {
        lines(kval[which(aggval==agghours[i])],
              incvalT[which(aggval==agghours[i])],
              type="b",col=aggcol[i],
              pch=as.character(agghours[i]))
      }
      #show privacy (scaled)
      #lines(kval,kval*3000/100,type="b",col="red")
      #axis(4,label=(0:5)*20,
      #     at=((0:5)*20)*3000/100)
      #mtext("Privacy (k-crowd)",side=4,line=2)
      
    }
  }
  par(mfrow=c(1,1))
  dev.off()
}


## FIGURE counts of outliers for 1 and 3 hour aggregations
## larger k -> more privacy but more exclusions 
## when means more distortion of the original dataset
if (runalltests) 
{
pdf("../Rresults/NumberPrunedagg1agg3.pdf",width=15,height=5) 
{
  par(mfrow=c(1,2))
  barplot(cbind(countoutlierres[which(countoutlierres[,2]==3),3:6],
                countoutlierres[which(countoutlierres[,2]==1),3:6]),
          col=c(rep(brewer.pal(7,"Reds"),times=4),
                rep(brewer.pal(7,"Blues"),times=4)),
          main="Count Outliers",
          ylab="Number of Pruned Households",
          xlab="K-anon for agg=1hr (red) agg=3hr (blue)",
          beside=TRUE)
  #legend()
  barplot(cbind(timeoutlierres[which(timeoutlierres[,2]==3),3:6],
                timeoutlierres[which(timeoutlierres[,2]==1),3:6]),
          col=c(rep(brewer.pal(7,"Reds"),times=4),
                rep(brewer.pal(7,"Blues"),times=4)),
          main="Time Outliers",
          ylab="Number of Pruned Households",
          xlab="K-anon for agg=1hr (red) agg=3hr (blue)",
          beside=TRUE)
  #legend()
  par(mfrow=c(1,1))
}
dev.off()
}
## FIGURES exposure of HHs vs Utility for all activities and aggregations
## Show number of HH per agg with some activity, and indicates (all) kanoms
if (runalltests) 
{
{
  
  #activityAR is list activity recognition results at diff aggs eg peakAR
  #activityID is the index of the activity type from this legend:
  #activitynames = c("Leak","Habit","Unoccupied")
  #accres is the matrix of utility results, eg accrespeak 
  #todo consider showing other accuracies eg precision and recall?
  #upper bound is the highest activityAR percentage
  #used to scale both axes
  #KANOMTHRESHOLDS indication on the graph.  List of thresholds to consider use c(-1) to show none
  #showutility=TRUE to include accuracy on the same figure (default FALSE)
  showPrivacyUtility <- function(activityAR,activityID,accres,ub=100,KANONTHRESHOLDS=kanon,showutility=FALSE) 
  {
    #agghours [1]  1  2  3  4  6  8 12 24
    par(mar=c(5, 4, 4, 5) + 0.1) #c(bottom, left, top, right)
    bb=boxplot(rowSums(activityAR[[1]]==TRUE), #*100/NUM_HH,
               rowSums(activityAR[[2]]==TRUE), #*100/NUM_HH,
               rowSums(activityAR[[3]]==TRUE), #*100/NUM_HH,
               rowSums(activityAR[[4]]==TRUE), #*100/NUM_HH,
               rowSums(activityAR[[6]]==TRUE), #*100/NUM_HH,
               rowSums(activityAR[[8]]==TRUE), #*100/NUM_HH,
               rowSums(activityAR[[12]]==TRUE), #*100/NUM_HH,
               rowSums(activityAR[[24]]==TRUE), #*100/NUM_HH,
               names=agghours,
               #ylim=c(0,ub),
               col=activitycols[activityID],
               main=activitynames[activityID],
               xlab="Aggregation Period (Hours)",
               ylab="Exposure (HHs %)")
    grid()
    ## add utility if required
    if (showutility) {
      lines(accres[,7]*10*ub/10,type="b",lwd=3,
            col=activitycols[activityID],pch=activitypch[activityID]) 
      axis(4,labels=(1:10)*0.1,at=(1:10)*ub/10)
      mtext("Balanced Accuracy", side = 4,line=3,col=activitycols[activityID])
    }
    #show threshold for k-anonymity
    abline(h=KANONTHRESHOLDS,col="red")
    #no space for legend - say in caption
    #legend("bottomright",c(paste(agghours,"hour"),"Utility"),
    #       lty=rep(1,times=7),lwd=rep(2,times=7),bg="white",
    #       pch=c(rep(NA,times=6),activitypch[activityID]),
    #       col=c(aggcol,activitycols[activityID]))
  }
  
  
  #K=20 is 20/3557 [1] 0.005622716 = 0.56\% of the population
  leakAR <- allAR[[1]]
  pdf("../../images/leakExposureUtility.pdf",width=6,height=6) #was 2%
  showPrivacyUtility(leakAR,1,accresleak,100,c(30),showutility=FALSE)
  dev.off()
  
  habitAR <- allAR[[2]]
  pdf("../../images/habitExposureUtility.pdf",width=6,height=6)
  showPrivacyUtility(habitAR,2,accreshabit,25,c(30),showutility=FALSE)
  dev.off()
  
  occupancyAR <- allAR[[3]]
  pdf("../../images/occupancyExposureUtility.pdf",width=6,height=6)
  showPrivacyUtility(occupancyAR,3,
                     accresoccupancy,15,c(30),showutility=FALSE)
  dev.off()
  
  
}
}
