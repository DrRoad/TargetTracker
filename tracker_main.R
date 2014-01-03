#'
#'@author Ram Narasimhan
#'Based on the problem idea by Krishna Alamuru
#'
rm(list=ls())
library(ggplot2) #needed for plotting
library(reshape2)


names(df)
melt(df[1:10,], id.vars="time")


drawProgress <- function(df){  
  useful_rows <- max(which(df$targetX != 0)) #cut out the bottom part which is all zeros
  useful_df <- df[1:useful_rows , ]
  p <- ggplot(useful_df, aes(x=time)) + geom_line(aes(y=targetX), color="darkred")+ geom_line(aes(y=targetY)) +
    geom_line(aes(y=catcherX), color="red")+ geom_line(aes(y=catcherY), color="darkgreen")
  p <- p + facet_grid()
}



### CONSTANTS
orientationX <- c(-1,0,1,0)
orientationY <- c(0,1,0,-1)

#this is just used for visual debugging
visualDir <- c("Left", "Up", "Right", "Down")

#####
#' Set up the starting parameters here
target <- 1 #just an index to refer to the entity
catcher <- 2  #just an index to refer to the catcher

direction <- c(0,0) #dir[1] refers to the targets direction. 
#' dir[2] refers to catcher's direction
colx <- c(0,0); coly <- c(0,0) # initialize with zeros
colx[target] <- 2 # x-coord of target is stored in this column
coly[target] <- 3
colx[catcher] <- 4
coly[catcher] <- 5

#geography
numBlocksX  <- 5
numBlocksY  <- 4
blockLength <- 10


####
debug <- 0 #1 if debug mode is on
#####

#time parameters
tEndSim <- 5000
rate <- c(3,2) #target, catcher
#rate <- c(1,1) #target, catcher
#the above means that the target moves at some speed. once every t beats
#the catcher moves at a different speed. Once every c beats
catcherDelay <- 20 #number of beats after which catcher starts search

x <- c(0,0); y <- c(0,0) # initialize with placeholders
x[target] <- 20 #starting point for target
y[target] <- 20 #starting point for target
x[catcher] <- 10 #starting point for catcher
y[catcher] <- 10 #starting point for catcher


################

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

is_entity_at_intersection <- function(xt, yt){
  if(!(xt %% blockLength)) return(TRUE)
  if(!(yt %% blockLength)) return(TRUE)  
  return(FALSE)
}

getDirection <- function(xe,ye,dir) {
  done=FALSE
  if(is_entity_at_intersection(xe,ye)){
    #get a new direction. Option to change exists.
    print("at intersection")
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

getNextXYForEntity  <- function(tsim, entity, xe, ye, direction) {
  
  if( (entity==catcher) && (tsim < catcherDelay))  
    return(c(xe, ye, direction)) #catcher can't move yet
  
  if(!(tsim %% rate[entity])) { #moves only every rate beats
    xe <- xe + orientationX[direction]      
    ye <- ye + orientationY[direction]      
    direction <- getDirection(xe, ye, direction)
  } #else just continue
  
  if(debug) print(paste("getNext", entity, xe, ye, direction)) #for debugging
  
  return(c(xe, ye, direction))
}

# This is the termination condition.
catcher_found_target <- function(tsim, df) {
  #have to be in the same (x,y) spot at the same time.
  if((df$targetX[tsim] == df$catcherX[tsim]) && 
       (df$targetY[tsim] == df$catcherY[tsim])) return(TRUE)
  return(FALSE)
}


targetX <- rep(0, tEndSim); targetY <- rep(0, tEndSim);
catcherX <- rep(0, tEndSim); catcherY <- rep(0, tEndSim);
df <- data.frame(time=1:tEndSim, targetX, targetY, catcherX, catcherY)


#set an initial direction for both Catcher and Target
direction[target] <- getDirection(x[target], y[target], 0)
direction[catcher] <- getDirection(x[catcher], y[catcher], 0)


for(tsim in 1:tEndSim) {
  for (entity in target:catcher)  {
    xyd <- getNextXYForEntity(tsim, entity, x[entity], y[entity], direction[entity]) 
    x[entity] <- xyd[1]; y[entity] <- xyd[2]; 
    df[tsim,colx[entity]] <- xyd[1] #store the x coord
    df[tsim,coly[entity]] <- xyd[2] #store the y coord
    direction[entity]     <- xyd[3]
    print(paste(tsim,":", x[entity],y[entity],visualDir[direction[entity]]))
  }
  
  if(catcher_found_target(tsim, df)) {
    break
  }
}


#hist(df$targetY)
drawProgress(df)

head(df)

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


