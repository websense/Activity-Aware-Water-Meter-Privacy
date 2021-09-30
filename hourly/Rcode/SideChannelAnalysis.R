## SideChannelAnalysis
## Identify unicity counts for various side channel attacks
## count of activities or activity frequecy at some time
## Input: alloutlierslist[[k]][[agg]] (for pruning), 
## Input: allGT activities that could be known by the adversary 
## Input: allAR activities the adversary will match to (may not be same)
## Output: unicity count for each pk,agg combination (pk is number of prior knowledge events) 
## Author: Rachel Cardell-Oliver
## Version: Sep 2021

## S1. load libraries and data
{
  source("./ConfigurationSettings.R")
  allGT = readRDS(file="../Rdata/allGT.rds") #activities the adv may know of
  allAR = readRDS(file="../Rdata/allAR.rds") #activities they can match
  alloutlierslist = readRDS(file="../Rdata/alloutlierslist.rds")
}

# UNICITY metric for attacks
{
  
  # UNICITY count for attacks
  # for each HH h in SAFE_HH normally taken from activityAR matrix) avoiding outliers
  ## activityAR is what the attacker deduces, even if the GT is different
  
  ## Montjoye
  # Unicity test and the likelihood of deductive disclosure. 
  #The considered dataset contains one trace T for each user. 
  #The traces spatio-temporal points contain the region in which the user was and the time of the interaction. 
  #We evaluate the uniqueness of a trace given a set Ip of p randomly chosen spatio-temporal points. 
  #A trace is said to be to be compatible with Ip if Ip(T [Fig. 2A]. 
  #Note that this notion of compatibility can easily be extended to noisier or richer data. 
  #A brute force characterization is performed by extracting from the entire dataset of 1.5 M users S(Ip), 
  #the set of users whose mobility traces T are compatible with Ip. 
  #All mobility traces in the dataset T are successively tested for compatibility with Ip. 
  #A trace is characterized ‘‘out of x’’, if the set of traces that are compatible 
  #with the points contains at most x users: jS(Ip)j # x. 
  #A trace is uniquely characterized if the set contains exactly one trace: jS(Ip)j 5 1. 
  #The uniqueness of traces is estimated as the percentage of2500 random traces that are unique given p spatio-temporal points. 
  #The p points composing Ip are taken at random among all the interactions the user had with the service. 
  #As discussed, we do not apply any constraints regarding the choice of Ip.
  
  
  # for each number of attacks pk
  # for pk in 1:MAXK
  # randomly choose k attack points for h 
  #    (knowledge of activity in ground truth)
  #    use a suitable aggregation (eg daily for leaks/occ, 
  #    hourly and upwards for habits, peaks)
  # check the uniqueness of those k points for h in the population:
  #   is h uniquely identified by these k pts?
  #   if so unicity c = c+1 
  # repeat this test NTRIES time
  #looks pretty stable (with 100) so 50 is enough
  # and report distribution of c (median etc) over NTRIES
  # return number of HHs successfully identified by a K attack
  # if a hh has fewer than k points, just choose their max [Culnane]
  # eg activityar = habitAR[[1]] what the attacker sees, even if it is not accurate
  # tested for SAFE_HH only (ie outliers suppressed)
  # 10 tries per house seems enough
  #default for safehh is 1:N but can have suppressed version
  # PK is the assumed maximum number of events MAXPK the attacker knows (from configurationsettings)
  # season is a subset sequence of the periods.  NULL default will be all
  # Care to select relevant periods depending on the agg

  
  ## selectedar = activityar[season, inliers] for selected season and inlier HHs
  ## ditto for selectedgt
  unicity <- function(selectedgt, #ground truth for safe dataset
                      selectedar, #published safe dataset
                      agg, #which aggregation is being used
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
            apdays = as.vector(floor(ap/(24/agg))) #get day of activity
            apdayids = ap[!duplicated(apdays)]
            ## TODO take this sample at diff hours for habits
            if ( length(apdayids) < k) { #not enough matches for this hh
              nc = nc-1  
            } else { # we have enough (length(ap) >= k) 
              # choose k activity occurrences to be matched, from diff days
              sp = sample(apdayids,k) 
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
    #per household freq of this activity
    #activityfreq = hist(colSums(activitygt),breaks=0:M,plot=FALSE)
    return(list(compromised,candidates)) #,activityfreq))
  }


## tests
if (runalltests) 
{
  ac=2
  agg=3
  k=20
  outliers = alloutlierslist[[k]][[agg]]
  selectedgt = allGT[[ac]][[agg]][,-outliers]
  selectedar = allAR[[ac]][[agg]][,-outliers]  
  PK=MAXPK
  u1=unicity(selectedgt,selectedar,agg,PK=MAXPK, NTRIES=10)
  bb=boxplot(u1[[1]])
}
}

## S3. calculate Unicity results for all times
{
unicityres = c()
for (ka in kanon[2:4]) {  #only
  for (agg in agghours[1:4]) { #only low aggs are accurate enough
    outliers = alloutlierslist[[ka]][[agg]]
    for (ac in 1:3) {
      selectedgt = allGT[[ac]][[agg]][,-outliers]
      selectedar = allAR[[ac]][[agg]][,-outliers]  
      uh=unicity(selectedgt,selectedar,agg,PK=MAXPK, NTRIES=10)
      bb=t(round(boxplot(uh[[1]],plot=FALSE)$stats[c(1,3,5),])) #min, median, max
      
      
      candidates = round(mean(uh[[2]][,MAXPK])) #average number of HHs with at least MAXPK events

      unicityres = rbind(unicityres,
                         cbind(ka, agg, ac, (cbind(1:MAXPK,bb)), candidates))
    }
  }
}
colnames(unicityres) = c("Ka", "AggHours", "ActivityID","pk",
                         "MinU","MedU","MaxU","NumAttackCandidates")

saveRDS(unicityres,file="../Rdata/unicityres.rds")
}

## S4. calculate Unicity results for the a shorter season
{
  alloutlierslist = readRDS("../Rdata/alloutlierslist.rds")
  hrlabels = as.numeric(as.vector(rownames(allAR[[2]][[1]])))
  #range(hrlabels) 7584 10271
  drystartpos = hrlabels[14*24]+1  #time stamp start 2 weeks in
  dryendpos = hrlabels[14*24 + (8*7*24)+1] #time stamp for 10 weeks in (next wk)

  unicityresdry = c()
  for (ka in kanon[2:4]) { 
    for (agg in agghours[1:4]) { #only low aggs are accurate enough
      outliers = alloutlierslist[[ka]][[agg]]
      ## get season labels for this temporal aggregation
      sellabels = as.numeric(as.vector(rownames(allGT[[1]][[agg]])))
      season = which(sellabels==drystartpos):which(sellabels==dryendpos)
      for (ac in 1:3) {
        selectedgt = allGT[[ac]][[agg]][season,-outliers]
        selectedar = allAR[[ac]][[agg]][season,-outliers]  

        uh=unicity(selectedgt,selectedar,agg,MAXPK, 10)
        bb=t(round(boxplot(uh[[1]],plot=FALSE)$stats[c(1,3,5),])) #min, median, max
        
        
        candidates = round(mean(uh[[2]][,MAXPK])) #average number of HHs with at least MAXPK events
        
        unicityresdry = rbind(unicityresdry,
                           cbind(ka, agg, ac, (cbind(1:MAXPK,bb)), candidates))
      }
    }
  }
  
  colnames(unicityresdry) = c("Ka", "AggHours", "ActivityID","pk",
                           "MinU","MedU","MaxU","NumAttackCandidates")
  
  saveRDS(unicityresdry,file="../Rdata/unicityresdry.rds")
  
}  


