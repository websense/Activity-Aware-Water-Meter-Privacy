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

## Fine-grained 10 second activites
{
    fineactivitycols = brewer.pal(8,"Paired")
    fineactivitynames = c("Taps","Toilet","Shower",
                          "Dishwasher","Clotheswasher","All") 
    fineactivitypch = 1:5

    # in multiples of 10 seconds (so 6 is 1 min)
    aggselection = c(1,6,6*5,6*15,6*30,6*60)
    
    #labels for graphs
    aggselectionminutes = paste(as.character(round(aggselection*10/60)),"min")
    aggselectionminutes[1] = "10 sec" #10 second label
}


