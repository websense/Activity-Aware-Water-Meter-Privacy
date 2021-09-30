#======= SEQHDA ALGORITHM ============================================
#seqHDA algorithm finds all patterns from a meter trace
# using basic sequential clustering and pattern matching

## Author: Rachel Cardell-Oliver
## Version: Sep 2021


library(mclust) #GMM


##========= pattern matching ========================
patternmatch <- function(actual,expected) {
  
  a = length(intersect(actual,expected)) #match: A and E size  TP
  b = length(setdiff(actual,expected)) #miss: A and notE size  FP
  c = length(setdiff(expected,actual))  #extra: notA and E FN
  
  if (((a+b)==0)||((a+c)==0)) {
    matchval = 0
  } else {
    p=a/(a+b) #precision
    r=a/(a+c) #recall
    matchval = (2*p*r)/(p+r)  #F measure
  }
  #print (c(a,b,c,length(actual),length(expected),length(all)))
  return(matchval)
}

#new
#returns highest pattern above > MATCHRATIO with  pattern name and match ratio
#TODO write out which days
#observeddays = day of trial
#observedweek = day of week 0 to 6
#DONE deal with year wrap
checkpatterns3 <- function(observeddays,MATCHRATIO) 
{
  
  #what days is the habit actually observed on, and range
  minday=min(observeddays)
  maxday=max(observeddays)
  ndays=length(observeddays) #days in pattern
  
  gaps = c(observeddays[2:ndays]-observeddays[1:(ndays-1)])
  
  everyday = c()
  for (d in 1:(ndays-1)) {
    if (gaps[d]<BREAKDAYS) { 
      #then do count
      everyday = c(everyday, (observeddays[d]:(observeddays[d+1]-1)))
    }
  }
  everyday = c(everyday,maxday) #last one to join
  
  #generate expected patterns from minday to maxday to match against
  everydow=observeddays%%7  #may not be correct day of week though
  weekly=table(everydow)
  o = order(weekly,decreasing=TRUE)
  common = as.numeric(names(weekly[o]))[1:3]
  common = c(common,rep(-1,times=(7-length(common))))
  #wday 0–6 day of the week, starting on Sunday.
  commondaynames = paste(c("Su","Mo","Tu","We","Th","Fr","Sa","Su")[common+1],collapse="")
  # 
  oneweekdays = everyday[which(everydow==common[1])] #all of them
  # #selection of week days is based on most frequently occuring days in the observed pattern
  twoweekdays = c(oneweekdays, everyday[which(everydow==common[2])])
  threeweekdays = c(twoweekdays, everyday[which(everydow==common[3])])
  fourweekdays = c(threeweekdays, everyday[which(everydow==common[4])])
  odddays = subset(everyday, everyday%%2==1)
  evendays = subset(everyday, everyday%%2==0)
  # steps = observeddays[2:ndays]-observeddays[1:(ndays-1)] #TODO other periods eg every days
  
  #do the matching
  matches = round(
    c(
      patternmatch(observeddays,everyday),
      patternmatch(observeddays,evendays),
      patternmatch(observeddays,odddays),
      patternmatch(observeddays,oneweekdays) ,
      patternmatch(observeddays,twoweekdays),
      patternmatch(observeddays,threeweekdays),
      patternmatch(observeddays,fourweekdays),
      0), digits=2)
  
  names(matches) = c("everyday","evendays","odddays","onceaweek","twiceaweek","threeaweek","fouraweek","nomatch")
  
  #now find the strongest match
  bestmatch = which.max(matches)
  if (matches[bestmatch] > MATCHRATIO) {
    return( list(matches[bestmatch], commondaynames ) )
  } else {
    return( list(matches[length(matches)], "noregulardays" )) #none
  }
  
  
}

#===================================================
### suggested seqHDA paramters
LOWERTHRESHOLD = 500 #was 300  #min L/h for habits, for diff periods set this for the population
THETA = 150 #max radius allowed for vol clusters 
MINSUP = 7 #7 #min size (number of obs) for habit cluster to be interesting from 8 weeks of data
#not used for short sections 
BREAKDAYS = 10 #14 #number of days gap to trigger a split of cluster
MATCHRATIO = 0.5 #0.8 or 0.9 in practice, but low here for testing

#######=======  seqHDA algorithm ========================

#get meter data as data.frame of volumes with row.names as dates
#mt is an ID (integer) for the meter
# mvols is a vector of volumes per agg period
# mts is a vector of times, day*24 + hour (taken from rownames in aggVolumes)
# agg number of hours per period may be >1 hour for each volume
#ploton=FALSE,
# version 3 of the algorithm works for different time aggregations

## TODO QUESTION - should threshold be per meter or per population
## eg Meter312
## threshold = round(mean(kmeans(mvols,2)$centers))
seqHDA3 <- function(mt,mvols,mts,agg=1,threshold,matchratio,minsup,
                    ploton=FALSE)
{
  if (ploton) { 
    plot(mts/24,mvols,type="h",
                     ylab="L/period",xlab="Days",col="gray",
                     main=sprintf("Meter %d Agg Hours %d",mt,agg)) 
      abline(h=threshold)
    }
  habits = mvols*0 #habit vector to return
  habitresults = c() #table of results to return
  #highvol has selected all >LOWERTHRESHOLD observations
  habitpos1 = which(mvols>threshold) #keep this even for large agg because other non-habit vols hard to predict
  hvols1 = mvols[habitpos1]
  #lines(mts[habitpos1]/24,hvols1,type="h",col="yellow")
  # which periods do these high vols occur in
  periods = mts[habitpos1]%%24
  tab = table(periods)
  #1. select frequently occuring periods - habit always occurs at the same time of day
  candidates = tab[which(tab>minsup)]
  candidateperiods = as.integer(names(candidates))
  habitpos2 = habitpos1[which(is.element(periods,candidateperiods))]
  hvols2 = mvols[habitpos2]
  #hours with frequent recurrence
  #if (ploton) { lines(mts[habitpos2]/24,hvols2,type="h",col="orange") }

  #2. check recurrence patterns

  #find days for these highvol,hours pairs and look for patterns
  for (h in candidateperiods) 
  {
    hourpos = which(mts[habitpos2]%%24 == h)
    hourvols = mvols[habitpos2[hourpos]]
    #for high agg, no need to separate hours for similarity
    #maybe don't for 1 hr either - just assume other events may contribute
    #so habit volume is really irrigation + other activities
    
    observeddays = floor(mts[habitpos2]/24)[hourpos]
    firstday=observeddays[1]
    lastday=tail(observeddays,1)
    ## TODO split if there is a break in the middle of observed days
    pat = checkpatterns3(observeddays,matchratio)
    habitresults = rbind(habitresults,
                         c(mt,agg,h,range(hourvols),
                           firstday,lastday,length(hourvols),pat[[1]],pat[[2]]))
    if (pat[[1]] > matchratio) {
      #copy the habits for this hour and range into mdata
      pp = habitpos2[hourpos] #global positions of this habit
      habits[pp] = mvols[pp]
      if (ploton) { lines(mts[hourpos][pp]/24,habits[pp],type="h",col="red") }
    } 
  }
  
  if (ploton) { lines(mts/24,habits,type="h",col="red") }

  if (!is.null(habitresults)) {
    habitresults = as.data.table(habitresults)
    setnames(habitresults,c("MeterID","Agg","PeriodOfDay","volLB","volUB","firstday","lastday","Nevents",
                            "MatchRatio","Pattern"))
  }
  
  #boolean version of habit (or not) and table describing results
  return(list("habitevents"=(habits > 0),"habitdetails"=habitresults))
}



## TESTS seqHDA3 for different periods - looks ok
## OK some outliers get left in eg mt 50 - see day outliers - 
## they must be always regular
if (FALSE)
{
par(mfrow=c(3,2))
for (mt in c(35,42,50,100,245,312,500,612,723)) { #,1000,999,1400)) {
  for (i in 1:6)
  {
    thisagg = c(1,2,3,6,12,24)[i]
    threshold = c( 570, 813, 947, 1195, 1563, 2371)[i] #previously learned
    mts=as.numeric(rownames(aggVolume[[thisagg]]))
    mh = seqHDA3(mt,aggVolume[[thisagg]][,mt],
                 mts,thisagg,threshold,0.5,14,
                 ploton=TRUE)
    #print(mh$habitdetails)
  }
}
par(mfrow=c(1,1))
}
  
# OLD HDA stuff with volume ranges test clustering approach to use
if (FALSE) 
{
  #also tried    hc = hclust(dist(hourvols))
  #also tried Mclust and kmeans but hist is good for 1D data
  par(mfrow=c(1,2))
  h1 = hist(hourvols,breaks=(0:20)*THETA,plot=FALSE)
  plot(hourvols)
  abline(h=h1$breaks,col="blue")
  
  h2 = hist(hourvols,plot=FALSE)
  plot(hourvols)
  abline(h=h2$breaks,col="red")
  par(mfrow=c(1,1))
  
  #2. old method for group clusters by vol / TODO allow for seasons
  h2 = hist(hourvols,plot=FALSE)
  bb = h2$breaks
  for (b in 1:(length(bb)-1))
  {
    if (h2$counts[b] > MINSUP)
    {
      hourposB = which(hourvols >= bb[b] & hourvols < bb[b+1])
      
      observeddays = floor(mts[habitpos2]/24)[hourposB]
      firstday=observeddays[1]
      lastday=tail(observeddays,1)
      ## TODO split if there is a break in the middle of observed days
      #day of week 0 to 6
      observedwkday = (mts[habitpos2] %% 7)[hourposB]
      pat = checkpatterns2(observeddays,observedwkday,MATCHRATIO)
      habitresults = rbind(habitresults,
                           c(mt,agg,h,bb[b],bb[b],firstday,lastday,length(hourposB),pat[[1]],pat[[2]]))
      if (pat[[1]] > MATCHRATIO) {
        #copy the habits for this hour and range into mdata
        pp = habitpos2[hourpos[hourposB]] #global positions of this habit
        
        habits[pp] = mvols[pp]
        if (ploton) { lines(mts[hourpos][pp]/24,habits[pp],type="h",col="red") }
      } 
    } #else {
    #print("Error: count too low")
    #print(c(h,b,h2$counts[b]))
    #}
    
    
  } #else cluster too low
}


## Mon 10 May 2021 LOOKS OK but maybe need to cluster ranges
if (FALSE)
{
h272 = seqHDA2(272,labelLeaksPeaks(272,agg=1,ploton=FALSE))
h1500 = seqHDA2(1500,labelLeaksPeaks(1500,agg=1,ploton=FALSE))

  #end of old method from here - can be deleted
  if (FALSE)
  {
  #mc = Mclust(smts[,1])  #Gaussian mix cluster highvolumes into categories
 
  ## no, still select - considered separate leaks from high density patterns here 
  
  #highhours = unique(sort(smts[,4])) #find all hours with high use

  hlist=NULL
  for (h in highhours) { 
    for (c in 1:mc$G) {
      smtsH = subset(smts, smts[,3]==h & 
                       smts[,1] > LOWERTHRESHOLD & mc$classification==c)
      tsH = subset(ts, smts[,3]==h & 
                     smts[,1] > LOWERTHRESHOLD & mc$classification==c)
      nobs = length(smtsH[,1])
      print(c(h,c,nobs))
      #check cluster size and width
      if (nobs > MINSUP) {
       #print(smtsH)
       ss = summary(smtsH[,1])[c(1,3,6)] #min,median and max vol
       total = sum(smtsH[,1]) #all volumes for this pattern
       pat = checkpatterns2(smtsH,MATCHRATIO) #find best matching pattern
       #print(c(mt, h, nobs, ss, pat, names(pat)))
       if (pat > MATCHRATIO) {
        res =  c(mt, h, nobs, ss, total, pat, names(pat))
        
        if (ploton) {
          lines(tsH,smtsH[,1],type="h",col=rainbow(24)[h+1]) #h+1 else 0 col not shown
          lines(tsH,smtsH[,1],type="p",col=rainbow(24)[h+1],pch=h,cex=.5)
        }
        
        
        names(res) = c("meterID","Hour","NumEvents",
                           "MinVol","MedianVol","MaxVol","TotalVol",
                           "PatternMatch","Pattern")
        hlist = rbind( hlist, res )
       } #else match too low
      }
      
    #else insufficient size
    }
  }
  ## TODO make cases like user info for these returns
  if (!is.null(hlist)) {
    colnames(hlist) = c("meterID","Hour","NumEvents",
                        "MinVol","MedianVol","MaxVol","TotalVol",
                        "PatternMatch","Pattern")
    return(hlist)
  } else { 
    return (NULL) 
  }
  } 
}






#========== utility functions for Basic Sequential Clustering ==============
# dont think I need these for privacy analysis
if (FALSE)
{
#add a new observation obs to an existing candidate selection tuple
#return the new candidate selection
#obs = c(vol,day,hour) an observation
#can = c(v1,v2,d1,d2,h,s,c) a candidate selection with vol, day, hour, size and centre
## ASSUMES conditons on hour of day and day distance already checked
##if (h != obs[3]) { print("CanDist ERROR hour mismatch:"); print (c(obs,can)); return (NULL) }
CanUpdate <- function( obs, can ) 
{
  h=can[5]
  v1 = min(obs[1], can[1])
  v2 = max(obs[1], can[2]) #update volume bounds
  d1 = min(obs[2], can[3])
  d2 = max(obs[2], can[4]) #update time bounds
  s1 = can[6]+1 #updated size
  c1 = round((can[7]*can[6] + obs[1])/s1) #new centroid, to the nearest L
  
  return ( c(v1,v2,d1,d2,h,s1,c1) )
}

#initialise a candidate selection from one obs
CanInit <- function(obs) {
  v=obs[1]
  d=obs[2]
  h=obs[3]
  return ( c(v,v, d,d, h, 1, v) )
}

## TEST CASES
# CanUpdate ( c(455,95,06), c(400,700, 50,90, 6, 30, 450)) #400 700  50  95   6  31 450, inside, very near centroid
# CanUpdate ( c(350,100,06), c(400,700, 50,90, 6, 30, 450)) #350 700  50 100   6  31 447, outside but close
# CanUpdate ( c(350,100,10), c(400,700, 50,90, 6, 30, 450)) #350 700  50 100   6  31 447 NOTE  wrong hour (intentionally) NOT CHECKED


#calculate the distance between a single observation and a candidate selection
#use MAX to denote incomparable candidates
#obs = c(vol,day,hour) an observation
#can = c(v1,v2,d1,d2,h,s,c) a candidate selection with vol, day, hour, size and centre
CanDist <-function( obs, can, breakdays, max, ignorehours) {
  if ( !ignorehours & can[5] != obs[3] ) { return (max) } #hours dont match
  if ((obs[2]-can[4]) > breakdays | (obs[2] < can[4]) ) { return (max) } #time out of range, also check obs follows can
  if ( (can[1] <= obs[1]) & (obs[1] <= can[2]) ) 
  { return (0) }
  else 
  { return (abs( can[7]-obs[1] )) } #distance from ti to centroid of Sk
}

{
  # TEST CASES
  # CanDist ( c(500,100,06), c(400,700, 50,90, 6, 30, 450), BREAKDAYS, 1000) #0, inside
  # CanDist ( c(350,100,06), c(400,700, 50,90, 6, 30, 450), BREAKDAYS, 1000) #100, outside but close
  # CanDist ( c(350,100,10), c(400,700, 50,90, 6, 30, 450), BREAKDAYS, 1000) #MAX wrong hour
  # CanDist ( c(350,101,10), c(400,700, 50,90, 6, 30, 450), BREAKDAYS, 1000) #MAX since day too late
  # CanDist( c(421,  45,   6), c(421, 421, 125, 125,   6,   1, 421), BREAKDAYS, 1000) #MAX since day too early
}

#Input obs to be compared, hlist list of candidate selections and 
# m is the number of candidates currently initialised in hlist
#Return position p in hlist for the candidate with the for min dist to obs
#and the value of that distance
## only need to check recent times
FindMinDist <- function(obs,hlist,m,breakdays,max,ignorehours) {
  if (m==0) { #empty list case, no match
    return ( c(1,max) )
  }
  #else search for the closest match
  if (m>0) {
    dist = vector(length=m)  #+max #initialise to worst case
    #x= TODO #only consider recent times
    for (i in 1:m) {
      dist[i] = CanDist(obs,hlist[i,],breakdays,max,ignorehours)
    }
    p = which.min(dist) #dist to min cluster, first of those if a tie: should only happen for max
    v = min(dist)
    return(c(p,v))
  } 
}
# TEST CASES see below

#smst is a time-ordered smart meter time series: list of triples+1 obs = c(vol,day,hour, dayofweek)  #dow needed later
#THETA is vol radius for clusters, take as MAXWIDTH/2
BSAS <- function( smts, mt, timestamps, ploton, lower, 
                  minsup, breakdays, max, theta, matchratio, ignorehours) 
  {
  habitlist = matrix(ncol=7, nrow=500) #never more than 250 habits per user?? but watch out
  m = 0 #number of habits so far
  n = dim(smts)[1] #number of observations
  for (i in 1:n) {
    obs = smts[i,]
    v = obs[1]
    d = obs[2]
    h = obs[3]
    if (v > lower) {      
      pv = FindMinDist(obs,habitlist,m,breakdays,max,ignorehours) #return position p for min dist
      #print(c(obs,pv))
      if (pv[2] > theta) { #too far away from existing candidate selections
        m=m+1 #so add new candidate selection
        newhab = CanInit(obs)
        habitlist[m,] = newhab
        #print(99999)
      } else { # update existing
        newhab = CanUpdate(obs, habitlist[pv[1],]) #changes to new habit
        habitlist[pv[1],] = newhab
        #print(c( pv, obs, newhab, habitlist[pv[1],]))
      }
    }
  }
  # for (i in 1:m) delete candidates that are too small
  hl = subset(habitlist[1:m,],habitlist[1:m,6] > minsup)
  
  if (length(hl)==0) { return (NULL) }
  #otherwise check for pattern match
  if (length(hl) == 7) { #one row only
    #check patterns, returns matches abouve MINMATCH
    checklist = rbind(c(mt,hl[c(5,6,1,2,3,4,7)]),NULL) #make matrix
  } else { #more than one row
    checklist = cbind(mt, hl[,c(5,6,1,2,3,4,7)]) #include the cl centre
  } 
  #print(checklist)
  
  ##return checklist; #short stop -check for timing
  if (matchratio==0) { 
    return( checklist )
  } else {
  
  #else check it
  finalhabitlist =  checkpatterns(checklist,mt,ts,mdata[,1],matchratio)
  if (length(finalhabitlist) > 11) {
    o = order(finalhabitlist[,2]) #order by hour of day
    return (finalhabitlist[o,])
  } else {
    return( finalhabitlist )
  }
  }
  
}

}

#######=======  Jin's routines algorithm roughly ========================

routines <- function(mt,timestamps,voldata,ploton,LOWERTHRESHOLD,MINSUP,BREAKDAYS, THETA) 
{
  #print(c(mt,MATCHRATIO,LOWERTHRESHOLD,MINSUP,BREAKDAYS, THETA))
  ts = makeTS(mt,timestamps,voldata,-1) #all records
  o = order(ts$dayofyear*24+ts$hourofday) #get increasing order of time stamps
  smts = cbind( ts[o,4], ts$dayofyear[o], ts$hourofday[o] ) #, ts$dayofweek[o]
  if (ploton) {
    plot(smts[,2]+smts[,3]/24,smts[,1],main=sprintf("mt=%d",mt),col="gray")
  }

  hlist = BSAS(smts,mt,timestamps,voldata,ploton,LOWERTHRESHOLD,MINSUP,BREAKDAYS, 1000, THETA, 0, TRUE)

  return(hlist)
}
