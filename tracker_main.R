#'
#'@author Ram Narasimhan
#'Based on the problem idea by Krishna Alamuru
#'
rm(list=ls())
setwd("~/RStats/Blog_DataDoodler/TargetTracker/")

library(ggplot2) #needed for plotting
library(reshape2)
################
# If you wish to understand this code, it might be best to start
#' with the main loop where one replication of the catcher chasing the tracker
#' happens. Quite a number of the accompanying functions are called by the main loop.
#' 

#All the runtime parameters are set in the file CONTROLS.R
#This file should be in the same directory as the TargetTracker code.
source("constants.R") 
source("simParameters.R") #You can change Parameters here
source("trackerPlots.R")
source("tracker_functions.R")



#Main Loop
# Make multiple runs (Replication of simulation) and take the average of stats
stats_df <- reset_stats_data_frame(kNumReplications) 
for(i in 1:kNumReplications) {
  coords <- initialize_replication()
  
  replication <- chase(coords)
  
  time_replication_ended <- replication[[1]]
  coord_df <- replication[[2]]

  if(time_replication_ended==tEndSim) print("Unable to Catch")
  #instrumented metrics for this iterations
  stats_df[i,2] <- ifelse(time_replication_ended<tEndSim, 1, 0)
  stats_df[i,3] <- time_replication_ended
  stats_df[i,4] <- catcherDelay
  stats_df[i,5] <- rate[1]
  stats_df[i,6] <- rate[2]
}

summary(stats_df$repEndTime)


if(plot_Option) 
  print(drawXY_Over_Time_v2(coord_df, time_replication_ended))


if(create_Animation){
  create_GIFAnimation_of_Chase(coord_df, time_replication_ended, numImages=10)
  create_HTMLAnimation_of_Chase(coord_df, time_replication_ended, numImages=100)
    create_HTMLAnimation_of_Chase_Progress(coord_df, time_replication_ended)  
}
  
time_replication_ended

if(0) {
  create_GIFAnimation_of_Chase(coord_df, time_replication_ended, numImages=100)  
}











