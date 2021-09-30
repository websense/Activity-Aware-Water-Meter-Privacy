## ColoursandNames
## Colour palettes and labels for figures for the paper for hourly
## Author: Rachel Cardell-Oliver
## Version: Sep 2021

{
require(data.table)
require(RColorBrewer)
}
  

# colour palettes and names for figures for the paper
## Hourly activities
{

aggcol = brewer.pal(8,"Greens")[8:1] #need seq colours, not finishing too light
agghours = c(1,2,3,4,6,8,12,24)
aggnames =  paste(agghours,"hr",sep="")
  
activitycols = c("royalblue","forestgreen","orange")
activitynames = c("Leaks","Habits","Daily Occupancy") 
activitypch = c(6,5,1) #2,
}

## 10 second activites
{
  
}


