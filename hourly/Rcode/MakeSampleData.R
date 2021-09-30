## MakeSampleData
## Generate sample data for publication
## Author: Rachel Cardell-Oliver
## Version: Sep 2021

volumeGT = readRDS(file="../Rdata/volumeGT.rds")
trialdates = readRDS(file="../Rdata/trialdates.rds")

M = dim(volumeGT)[1] #number of observations
N = dim(volumeGT)[2] #number of meters

ss = sample(1:N,200) #select hhs
tt = (28*24) : ((28*24) + 12*7*24 -1) #12 week sample period

#TODO add noise? and scale back up to M x N (so old code works)
saveRDS(volumeGT[tt,ss],file="../Rdata/samplevolumeGT.rds") 
saveRDS(trialdates[tt],file="../Rdata/sampletrialdates.rds") 

