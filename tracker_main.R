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

reset_coord_data_frame <- function(tEndSim){
  # df is a data frame with lots of 0s
  targetX <- rep(NA, tEndSim); targetY <- rep(NA, tEndSim);
  catcherX <- rep(NA, tEndSim); catcherY <- rep(NA, tEndSim);
  coord_df <- data.frame(time=1:tEndSim, targetX, targetY, catcherX, catcherY)  
  return(coord_df)
}

#this is an empty dataframe, where all the stats will be stored
reset_stats_data_frame <- function(kNumReplications){
  repStatus    <- rep(NA, kNumReplications) 
  repEndTime   <- rep(NA, kNumReplications)
  catcherDelay <- rep(NA, kNumReplications)
  catcherRate  <- rep(NA, kNumReplications)
  targetRate   <- rep(NA, kNumReplications)
  stats_df <- data.frame(time=1:kNumReplications, repStatus, 
                         repEndTime, catcherDelay, targetRate, catcherRate)
  return(stats_df)
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

#there are 4 corners for the grid
is_entity_at_grid_corner <- function(xt, yt){
  if(!(xt %% (blockLength*numBlocksX)) && 
       !(yt %% (blockLength*numBlocksY)) ) return(TRUE)  
  return(FALSE)
}

#Checking for being at the grid boundary
is_entity_at_grid_edge <- function(xt, yt){
  if(!(xt %% (blockLength*numBlocksX)) ||
       !(yt %% (blockLength*numBlocksY)) ) return(TRUE)  
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
catcher_found_target <- function(tsim, coords) {  
  #have to be in the same (x,y) spot at the same time.
  if( (coords[[catcher]][1] == coords[[target]][1]) &&
        (coords[[catcher]][2] == coords[[target]][2])) return(TRUE)
  return(FALSE)
}

### end of functions

#All the runtime parameters are set in the file CONTROLS.R
#This file should be in the same directory as the TargetTracker code.
source("controls.R")
source("trackerPlots.R")

initialize_replication <- function(){
  #set an initial direction for both Catcher and Target
  direction[target]  <- getDirection(x[target],  y[target], 0)
  direction[catcher] <- getDirection(x[catcher], y[catcher], 0)
  
  #initialize coords for catcher and Target
  coords <- list(c(x[target],y[target],direction[[target]]),c(x[catcher],y[catcher],direction[[catcher]]))
  if(debug_print) str(coords)  
  return(coords)
}

chase <- function(coords) { 
  
  coord_df <- NULL
  #set the dataframe of coords to Zero
  if(verbose)
    coord_df <- reset_coord_data_frame(tEndSim) 
  
  #one replication of the catcher chasing target
  if(debug_print) print("start replications")
  for(tsim in 1:tEndSim) {
    if(debug_print) if(!tsim %% 100) print(tsim) #progress report
    for (entity in target:catcher)  {
      
      #for each beat of tsim, move the entity forward in its direction
      coords <- getNextXYForEntity(tsim, entity, coords) 
      
    if(verbose) {
      #store it in a data frame to help plotting
      coord_df[tsim,colx[entity]] <- coords[[entity]][1] #store the x coord
      coord_df[tsim,coly[entity]] <- coords[[entity]][2] #store the y coord      
    }
      
      if(debug_print)
        print(paste(tsim,"-", entity, ":", 
                    coords[[entity]][1], #x coord of [entity]
                    coords[[entity]][2], #y coord of [entity]
                    visualDir[coords[[entity]][3]] #direction of entity
        ))
    }
    
    if(catcher_found_target(tsim, coords)) {
      print(paste("Caught up at:", tsim))
      end_time <- tsim
      break #move on to next replication
    }
  }
  
  return(list(end_time, coord_df))
}


# Make multiple runs (Replication of simulation) and take the average of stats
stats_df <- reset_stats_data_frame(kNumReplications) 
#  st_row<- vector()
for(i in 1:kNumReplications) {
  coords <- initialize_replication()
  replication <- chase(coords)
  time_replication_ended <- replication[[1]]
  coord_df <- replication[[2]]
  
  #instrumented metrics for this iterations
  stats_df[i,2] <- ifelse(time_replication_ended<tEndSim, 1, NULL)
  stats_df[i,3] <- time_replication_ended
  stats_df[i,4] <- catcherDelay
  stats_df[i,5] <- rate[1]
  stats_df[i,6] <- rate[2]
}

if(verbose) drawXY_Over_Time(coord_df, time_replication_ended)

summary(stats_df$repEndTime)

library(animation)

if(verbose){
  saveHTML({
    par(mar = c(4, 4, 0.5, 0.5)) #setting margings
    for (i in 1:20) {   
      xyp <- drawXY_Over_Time(coord_df, i*25)
      print(xyp)
      #    ggplot(mtcars[sample(nrow(mtcars), 3), ], aes(x=gear, y=mpg)) + geom_point()
      ani.pause() #default pause and flush device
    }
  }, img.name = "chase2_gg", imgdir = "chase2_dir", 
  htmlfile = "chaseProgress2.html", autobrowse = FALSE, 
  title = "Progress of Catcher Tracking a Target", 
  outdir = getwd(),
  description = c("Shows a catcher chasing a Target.\n\n", 
                  "Can Turn at any Intersection")
  )
  
}



# TTD:

# Table of Variables:
#   catcher delay
#   Rates 1 & 2
#   starting Coords
#   Geometry
# 
#  replication Geometry, stx, sty, enx, eny, delay, rate1, rate2, ended
#   1123, 5,4, 10,10, 20,20, delay=20, rate 4,2, ended

#Implementing Turning Schemes - can turn only at ends...

# Future ideas to explore
#  * directions more than 4?!
#  * more than one catcher
#  * get rid of bounds!








