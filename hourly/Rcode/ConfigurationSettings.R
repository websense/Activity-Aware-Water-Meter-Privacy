## ConfigurationSettings
## Settings for running the whole application
## Author: Rachel Cardell-Oliver
## Version: Sep 2021

## all files
runalltests = FALSE  #set TRUE to run all optional tests and generate extra figures

## common libraries
source('./ColoursandNames.R') 
require(data.table)
require(mltools) #for mcc accuracy

## Aggregations
## uses agghours = c(1,2,3,4,6,8,12,24) from ColoursandNames


## ActivityRecognitoion.R
LEAK_LIM = 2 #>=2 L/h for last 24 hours defines a leak
EMPTY_LIM = 0 #L/h for last 24 hours defines an unoccupied day
source('./seqHDA2021.R') # habit settings in seqHDA2021 and ActivityRecognition.R


## AccuracyAnalysis.R
#uses agghours above

## OutlierAnalysis.R
kanon = c(5,10,20,30,50,100,300) #min size k to test for anonymity

## SideChannelAnalysis.R
MAXPK = 6 #max number of prior knowledge events known for one household
