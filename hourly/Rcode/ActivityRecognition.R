## ActivityRecognition
## Generate GroundTruth and ActivityRecognition matrics from volumeGT
## Input: create aggVolume.rds or read it if already done
## Output: allAR = list(leakAR, habitAR, occupancyAR)
## Output: allGT = list(leakGT, habitGT, occupancyGT)
## Output: figures to ../Rresults (see pdf(...))
## Author: Rachel Cardell-Oliver
## Version: Sep 2021

## Get configuration and saved aggregated volumes
{
  source("./ConfigurationSettings.R")
  if (file.exists("../Rdata/aggVolume.rds")) {
    aggVolume = readRDS(file="../Rdata/aggVolume.rds")
  }
}


# S3. Aggregated Volumes: scale up the volume to aggregated periods to be used for AR
if (!file.exists("../Rdata/aggVolume.rds")) 
{
  #volumeGT read in Main.R
  trialtimestamps = as.numeric(rownames(volumeGT))
  
  upscaleVolume <- function( volumegt, agg)
  {
    #perform aggregation from hourly data
    timestamp = as.numeric(rownames(volumegt))
    houragg = floor(timestamp/agg)*agg 
    uha = unique(houragg)
    dagg = matrix(0,nrow=length(uha),ncol=dim(volumegt)[2])
    rownames(dagg) = uha
    for (i in 1:length(uha)) {
      #if at least one activity hour in the period then mark agg period with activity
      pos = which(houragg==uha[i])
      if (length(pos)==agg) { #only label full aggregated periods with the total volume in that period, should always be true
        dagg[i,] = colSums(volumegt[pos,]) 
      }
    }
    return(dagg)
  }
  
  aggVolume = list()
  for (agg in agghours) {  
    if (agg==1) {
      aggVolume[[1]] = volumeGT
    } else {
      aggVolume[[agg]] = upscaleVolume(volumeGT, agg)
    }
  }
  saveRDS(aggVolume,file="../Rdata/aggVolume.rds")
  
  # TEST: various checks that times of day and activities are as expected
  if (runalltests) 
  {
    #these population demands look fine - high 7am and 7pm, low daytime, very low night # good - this looks fine too
    NS=1000
    NUM_HH = dim(volumeGT)[2]
    ss = sample(1:NUM_HH,NS)
    boxplot(c(aggVolume[[1]][,ss]) ~ rep(trialtimestamps %% 24, times=NS),outline=FALSE,ylab="Litres/hour",xlab="Hour of Day")
    
    boxplot(c(volumeGT[,ss]) ~ rep(trialtimestamps %% 24, times=NS),outline=FALSE,ylab="Litres/hour",xlab="Hour of Day")
    
    par(mfrow=c(2,2))
    for (m in c(2,13,3556,400,300,34,42,1000)) {
      boxplot(aggVolume[[1]][,m] ~ (as.numeric(rownames(aggVolume[[1]])))%%24,outline=FALSE)
    }
    par(mfrow=c(1,1))
  }
  
}


# S3.1 Leak Activity Recognition: detect leaks in GT and aggregated data
# input aggregated aggVolume list
# outputs list of results for each agg with per day leak (or not) matrices
#LEAK_LIM = 2 #>=2 L/h for last 24 hours defines a leak defined in ConfigurationSettings.R
{

leakDetection <- function( volumegt, agg)
{

if (agg==24) {  #daily data - no need to check min of all periods
    return( (volumegt > LEAK_LIM*agg)*1 ) 
}
#otherwise learn leak from min vol in all periods per dayd
aggperday = 24/agg
leaks = volumegt*0 #dummies to be filled
timestamp = as.numeric(rownames(volumegt))
dayagg = floor(timestamp/24) #same ID for all periods belonging to one day
dha = unique(dayagg)
for (i in 1:length(dha)) {
  #if at least one activity hour in the period then mark agg period with activity
  pos = which(dayagg==dha[i])
  if (length(pos)==aggperday) { #only label full aggregated periods with the total volume in that period, should always be true
    minvol = apply(volumegt[pos,],2,min) #find min flow/period in one day
    for (j in pos) { #copy to all periods of day
      leaks[j,] = minvol
    }
  }
}
#convert min flow = 2L/hour all day into min flow one day is XL/period 
#convert L/Hr to L/Period = L/Hr * Hr/Period
return( (leaks > LEAK_LIM*agg)*1 )
}
  
leakAR = list()
for (agg in agghours) { 
  leakAR[[agg]] = leakDetection(aggVolume[[agg]], agg)
}

saveRDS(leakAR,file="../Rdata/leakAR.rds")

}

# S3.3 Garden Watering Habits Activity Recognition: 
## detect irrigation hours in GT and aggregated data
{
  
  
  ## volumegt for whole population 
  ## already only agg hours, agg for info passed to seqHDA3
  ## detects HABITS 
  source('./seqHDA2021.R') #habit detection algorithm 
  habitDetection <- function( volumegt,  agg)
  {
    #choose habit threshold for each agg
    km=kmeans(c(volumegt),2,nstart=5)
    threshold=round(mean(km$centers)) #threshold
    matchratio=0.5 #how close is pattern of days?
    minsup=7  # at least 7 events to be a pattern
    
    habits = volumegt*0 #dummies to be filled
    habitslist = c()
    #learn habits for each household
    mts = as.numeric(row.names(volumegt))
    for (hh in 1:dim(volumegt)[2]) {
      hhres = seqHDA3(hh,volumegt[,hh],mts,agg,threshold,matchratio,minsup,
                      ploton=FALSE)
      habits[,hh] = hhres$habitevents
      habitslist = rbind(habitslist, hhres$habitdetails)
    }                 
    return(list("habits"=habits,"habitdetails"=habitslist))
  }
  
  # learn habits  for each aggregation
  habitAR = list()
  habitINFO = list()
  for (agg in agghours) { 
    hd = habitDetection(aggVolume[[agg]], agg)
    habitAR[[agg]] = hd$habits
    habitINFO[[agg]] = hd$habitdetails
  }
  
  saveRDS(habitAR,file="../Rdata/habitAR.rds")
  saveRDS(habitINFO,file="../Rdata/habitINFO.rds")
  
}



# S3.4 Occupancy Activity Recognition: detect empty days in GT and aggregated data
# input aggregated aggVolume list
# output list of results for each agg with per day leak (or not) matrices
{
  
  ## TODO could have say 20 hours empty to allow for auto activities such as garden irrigation system
  # defined in ConfigurationSettings.R 
  ##EMPTY_LIM = 0 #L/h for last 24 hours defines an unoccupied day 
 
  #takes vol per period matrix, finds leak vol (min flow in a day) 
  # returns per day T or F leak present based on max flow == EMPTY_LIM
  # does not allow for auto irrigation - TODO maybe 20 hours a day 0 is enough
  occupancyDetection <- function( volumegt, agg)
  {
    #perform aggregation from hourly data
    aggperday = 24/agg
    if (aggperday==1) {
      return( (volumegt == EMPTY_LIM) ) #no need to find max
    }
    #otherwise learn max vol in readings per day
    occupancy = volumegt*0 #dummies to be filled
    timestamp = as.numeric(rownames(volumegt))
    dayagg = floor(timestamp/24) #same ID for all periods belonging to one day
    dha = unique(dayagg)
    for (i in 1:length(dha)) {
      #if at least one activity hour in the period then mark agg period with activity
      pos = which(dayagg==dha[i])
      if (length(pos)==aggperday) { #only label full aggregated periods with the total volume in that period, should always be true
        maxvol = apply(volumegt[pos,],2,max) 
        ## TODO could count certain number of hours
        for (j in pos) { #copy to all periods of day
          occupancy[j,] = maxvol
        }
      }
    }
    return(occupancy == EMPTY_LIM )
  }

 
  # learn occupancy for each aggregation
  occupancyAR = list()
  for (agg in agghours) { 
    occupancyAR[[agg]] = occupancyDetection(aggVolume[[agg]], agg)
  }
  
  saveRDS(occupancyAR,file="../Rdata/occupancyAR.rds")
  
}



# S4. Save all ground truths and activity recognition by aggregations
{
# input: list of activity matrixes T/F for ground truth (1 hour) M agg times and ncol N individuals
  ## activity true in period ap iff 
  ## activity true in ground truth (activityAR[[1]]) for any p1 in pa
# input vector of the agg hours to be updated
# output: list of for new ground truth M/agg rows and N columns for each acc hours
# example: activityGT(peakAR,c(1,2,3,6,12,24))
  
makeActivityGT <- function(activityAR, agglist)
{
  activityGT = list()
  gtts = as.numeric(rownames(activityAR[[1]])) #ground truth time stamps
  for (a in agglist) {
   if (a==1) { #take 1 hour AR data as ground truth
     activityGT[[1]] = activityAR[[1]] 
   } else {
    # upscale for periods from GT
    pbounds = which(gtts %% a == 0) #index of period bounds in GT
    nperiods = length(pbounds)
    agt = activityAR[[a]] #make raw matrix for agg GT
    
    for (i in 1:nperiods) {
      endpi = (pbounds[i]+a-1)
      oneperiod = activityGT[[1]][pbounds[i]:endpi,]
      # activity true at agg scale if at least one event occurs within agg period
      agt[i,] = (apply(oneperiod,2,max)==1)
    }
    activityGT[[a]] = agt
   }
  }
  return(activityGT)
}

leakAR = readRDS("../Rdata/leakAR.rds")
habitAR = readRDS("../Rdata/habitAR.rds")
occupancyAR = readRDS("../Rdata/occupancyAR.rds")
allAR = list(leakAR,habitAR,occupancyAR)
saveRDS(allAR,file="../Rdata/allAR.rds")

leakGT = makeActivityGT(leakAR, agghours)
habitGT = makeActivityGT(habitAR, agghours)
occupancyGT = makeActivityGT(occupancyAR,agghours)
allGT = list(leakGT, habitGT, occupancyGT)
saveRDS(allGT,file="../Rdata/allGT.rds")
}

