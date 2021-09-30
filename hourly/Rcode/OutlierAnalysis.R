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
              if (lbs[b]==min(hh$breaks)) { #then include left break
                ainb = which(actCountsPerHH >= lbs[b] & actCountsPerHH <= ubs[b])
              } else {
                ainb = which(actCountsPerHH > lbs[b] & actCountsPerHH <= ubs[b])
              }
              bc[b] = length(ainb)
              hhexcl = c(hhexcl,ainb)
            }
            if (numexcl != length(hhexcl)) {
              print(sprintf("ERROR: COUNT %d != %d exclusions for k=%d agg=%d ac=%d",
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
