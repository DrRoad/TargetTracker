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
#----------Initialization Functions ----#

#area_df (a global data frame) gets defined here
resetIteration <- function(tEndSim){
  # df is a data frame with lots of 0s
  targetX <- rep(0, tEndSim); targetY <- rep(0, tEndSim);
  catcherX <- rep(0, tEndSim); catcherY <- rep(0, tEndSim);
  df <- data.frame(time=1:tEndSim, targetX, targetY, catcherX, catcherY)  
  return(df)
}


outOfBoundsX <- function(newx){
  if(newx <0) return (TRUE)
  if(newx > (numBlocksX*blockLength)) return (TRUE)
  return (FALSE)
}

outOfBoundsY <- function(newy){
  if(newy <0) return (TRUE)                       
  if(newy > (numBlocksY*blockLength)) return (TRUE)
  return (FALSE)
}

#' @description Function returns a TRUE whenever an entity
#' is at an 'intersection' FALSE otherwise.
#' At an intersection, the entity has the option to change directions
is_entity_at_intersection <- function(xt, yt){
  if(!(xt %% blockLength) && !(yt %% blockLength)) return(TRUE)  
  return(FALSE)
}

getDirection <- function(xe,ye,dir) {
  done=FALSE
  if(is_entity_at_intersection(xe,ye)){
    #get a new direction. Option to change exists.
    if(debug_print) print("at intersection")
    while(!done){
      done = TRUE
      consider_dir <- sample(4,1)
      newx <- xe + orientationX[consider_dir]      
      newy <- ye + orientationY[consider_dir]      
      #Check if new direction is a legal direction
      if(outOfBoundsX(newx) || (outOfBoundsY(newy))) done=FALSE       
    }
    dir <- consider_dir #found a new direction to move
  }
  
  if(dir==0)   stop("Error: Direction is zero. Check initial position.")
  return (dir)    
}

getNextXYForEntity  <- function(tsim, entity, coords) {
  if( (entity==catcher) && (tsim < catcherDelay))  {
    if(debug_print) print("catcher can't move yet")
    return(coords) #catcher can't move yet    
  }
  
  if(!(tsim %% rate[entity])) { #moves only every rate beats
    #unpack
    xe <- coords[[entity]][1];     ye <- coords[[entity]][2]; 
    direction <- coords[[entity]][3]
    
    #move one step along the direction that entity is headed
    xe <- xe + orientationX[direction]      
    ye <- ye + orientationY[direction]    
    
    #get new direction (if at intersection)
    direction <- getDirection(xe, ye, direction)
    
    #update coordinates and dir for entity
    coords[[entity]] <- c(xe, ye, direction)    
  } #else just continue
  
  if(debug) print(paste("getNext", entity,coords[[entity]][1],coords[[entity]][2],coords[[entity]][3] )) #for debugging

  
  return(coords)
}



# This function checks for the termination condition.
#' @return TRUE if catcher has caught up with target
catcher_found_target <- function(tsim, df) {
  
  #have to be in the same (x,y) spot at the same time.
  if((df$targetX[tsim] == df$catcherX[tsim]) && 
       (df$targetY[tsim] == df$catcherY[tsim])) return(TRUE)
  return(FALSE)
}


### end of functions

#All the runtime parameters are set in the file CONTROLS.R
#This file should be in the same directory as the TargetTracker code.
source("controls.R")
source("trackerPlots.R")

df <- resetIteration(tEndSim)

#set an initial direction for both Catcher and Target
direction[target]  <- getDirection(x[target],  y[target], 0)
direction[catcher] <- getDirection(x[catcher], y[catcher], 0)

#initialize coords for catcher and Target
coords <- list(c(x[target],y[target],direction[[target]]),c(x[catcher],y[catcher],direction[[catcher]]))
if(debug_print) str(coords)


chase <- function() {

  return(df)
}

#one replication of the catcher chasing target
if(debug_print) print("start replications")
for(tsim in 1:tEndSim) {
  if(debug_print) if(!tsim %% 100) print(tsim) #progress report
  for (entity in target:catcher)  {
    
    #for each tsim, move the entity forward in its direction
    coords <- getNextXYForEntity(tsim, entity, coords) 

    #store it to help plotting
    df[tsim,colx[entity]] <- coords[[entity]][1] #store the x coord
    df[tsim,coly[entity]] <- coords[[entity]][2] #store the y coord
    
    if(debug_print)
      print(paste(tsim,"-", entity, ":", 
                  coords[[entity]][1], #x coord of [entity]
                  coords[[entity]][2], #y coord of [entity]
                  visualDir[coords[[entity]][3]] #direction of entity
                  )
            )
  }
  
  if(catcher_found_target(tsim, df)) {
    print(paste("Caught up at:", tsim))
    break #move on to next replication
  }
}


#hist(df$targetY)
drawXY_Over_Time(df)

head(df, 100)


# TTD:
#   Can get away without even storing df
# 
# Table Variables:
#   catcher delay
#   Rates 1 & 2
#   starting Coords
#   Geometry
# 
#  replication Geometry, stx, sty, enx, eny, delay, rate1, rate2, ended
#   1123, 5,4, 10,10, 20,20, delay=20, rate 4,2, ended




# # Make multiple runs (Replication of simulation) and take the average of stats
# st <- data.frame()
# st_row<- vector()
# for(i in 1:kNumReplications) {
#   area_df <- resetIteration() #initialize the Area (cells)
#   
#   seedAreaWithPioneers(numPioneers,seeding.opt)
#   simstats <- accommodateSettlers(kNumSettlers, settling.option)    #one run
#   found.home <- simstats[1]
#   max.look.around <- simstats[2]
#   #instrumented metrics for this iterations
#   st_row <- store_iteration_stats(i, kNumSettlers, found.home, max.look.around)
#   st <- rbind(st,st_row)
# }
# 
# #Render the plot
# p <- drawArea(area_df)
# p
# names(st) <- c("Iter", "FoundHome", "NumSettlers", "Percent")
# st



# Future ideas to explore
# directions more than 4?!
# more than one catcher
#get rid of bounds!


