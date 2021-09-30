## Main.R
## Calls all modules for Activity-Aware Water Meter Privacy
## Author: Rachel Cardell-Oliver
## Version: Sep 2021

## Global settings and utility functions
source("./ConfigurationSettings.R")
source("./ColoursandNames.R")

## Get sample data (data for paper is private)
volumeGT = readRDS(file="../Rdata/samplevolumeGT.rds")
trialdates = readRDS(file="../Rdata/sampletrialdates.rds")

source("./ActivityRecognition.R")

## Privacy analysis
source("./AccuracyAnalysis.R")
source("./OutlierAnalysis.R")
source("./SafeKanonTables.R")
source("./SideChannelAnalysis.R")

