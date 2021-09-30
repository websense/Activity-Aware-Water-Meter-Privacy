## ReadHourlyData
## PRIVATE Import WaterCorporation sample meter data
## Output: trialdates.rds, volumeGT.rds saved in ../Rdata/
## Author: Rachel Cardell-Oliver
## Version: Sep 2021
## Private not for publication of WaterCorp household data
## Use synthesised volumeGT.rds and trialdates.rds instead

source("./ConfigurationSettings.R")

# S1. Volume GT: load Karratha 3000+ meter readings
{
  ## get raw data
  data = readRDS("../hourlyData/CleanDataKarratha2016.rds")
  servicepoints = data$servicepoints
  serviceIDs = unique(servicepoints)
  datetime = data$datetime
  volumes = data$volumes #just one long vol seq 15008000

  #utility function
  #return readings for the ith serviceID
  getMeter <- function(i,serviceIDs,servicepoints,volumes,datetime) {
    sid = serviceIDs[i]
    sr = which(servicepoints==sid)
    dt = datetime[sr]
    return(data.frame(volumes[sr],row.names=dt) )
  }
}


## S2.
# Make volume ground truth matrix for all meters
# creates matrix volumeGT
# columns are meters
# rows are times corresponding to trialtimeID
{
  #wrap days around year 116 to 117
  trialday = (datetime$year-116)*366 + (datetime$yday)
  trialhour = datetime$hour
 
  trialtimeID = sort(unique(trialday*24+trialhour)) #ordered?
  firsttime = min(trialtimeID)
  lasttime = max(trialtimeID)
  
NUM_HH = length(unique(servicepoints)) #number of households  3557
NUM_OBS = length(trialtimeID) #unique time points 4310
}

## S2a. record GT per meter.  slow so only do once if volumeGT not already saved
#remove incomplete days at start and end
if (!file.exists("../Rdata/volumeGT.rds")) #only do this first time because v slow
{
volumeGT = matrix(0, nrow=NUM_OBS, ncol=NUM_HH)
for (hh in 1:NUM_HH)
{
  xi=getMeter(hh,serviceIDs,servicepoints,volumes,datetime)
  vols = xi$volumes.sr.
  vols[which(is.na(vols))] = 0 #default is no vol
  vols[which(vols<0)] = 0 #-ve flow errors replaced by 0, 0.016% of all vals
  dd = as.POSIXlt(rownames(xi))  #map times
  metertimeID =  ((dd$year-116)*366 + (dd$yday))*24 + dd$hour
  volumeGT[metertimeID-firsttime+1,hh] = vols
}

rownames(volumeGT) = trialtimeID #use this for selections later
daystarts = which(trialtimeID %% 24 == 0)
firstwhole = min(daystarts) #first full day hour 0
lastwhole =  max(daystarts)-1  #last full day hour 23
volumeGT = volumeGT[firstwhole:lastwhole,]

trialdates = unique(format(datetime[firstwhole:lastwhole],format="%Y-%m-%d"))
saveRDS(trialdates,file="../Rdata/trialdates.rds")
saveRDS(volumeGT,file="../Rdata/volumeGT.rds")
}


## END ReadHourlyData
