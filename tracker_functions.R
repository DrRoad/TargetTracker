#----------Initialization Functions ----#

reset_coord_data_frame <- function(tEndSim){
  # df is a data frame with lots of 0s
  targetX <- rep(NA, tEndSim); targetY <- rep(NA, tEndSim);
  catcherX <- rep(NA, tEndSim); catcherY <- rep(NA, tEndSim);
  targetDir <- rep(NA, tEndSim); catcherDir <- rep(NA, tEndSim);
  coord_df <- data.frame(time=1:tEndSim, targetX, 
                         targetY, catcherX, catcherY,
                         targetDir, catcherDir)  
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

#Checking for being at the STREET END
is_entity_at_grid_edge <- function(xt, yt){
  #xt is on the permimeter & yt at intersection.
  px <- 0; py <- 0
  if((xt == (blockLength*numBlocksX)) || (xt == 0)) px <- 1
  if (px &&   !(yt %% (blockLength)))  return(TRUE)  
  
  if((yt == (blockLength*numBlocksY)) || (yt == 0)) py <- 1
  if (py &&   !(xt %% (blockLength)))  return(TRUE)  
  
  return(FALSE)
}



#' @return {"P", "I", "E", "C"}
#' P is a regular point on the street 
#' I is an intersection, E is a street End
#' C is the area CORNER
get_type_of_point <- function(xe, ye){
  if(is_entity_at_grid_corner(xe,ye)) return("C")
  if(is_entity_at_grid_edge(xe,ye)) return("E")
  if(is_entity_at_intersection(xe,ye)) return("I")
  return("P")    
}


getDirection <- function(entity, xe,ye, dir) {
  done=FALSE
  u_turn <- TRUE #by default U-turns are allowed
  loopCount <- 0
  
  pt_Type <- get_type_of_point(xe,ye)
  if(pt_Type == "P") return(dir) #no change needed     

  #at a special point.
  
  if(debug_print) print(paste(pt_Type, entity))
  if((pt_Type == "C") && (!uturns[entity,"Corner"]))       u_turn <- FALSE
  if((pt_Type == "E") && (!uturns[entity,"End"]))          u_turn <- FALSE
  if((pt_Type == "I") && (!uturns[entity,"Intersection"])) u_turn <- FALSE
  
    #At a special point. Option to change exists.
    while(!done){
      loopCount <- loopCount + 1
      #in this while loop, we keep trying until we get a new direction that works.
      done = TRUE
      consider_dir <- sample(4,1)
      
      #check if NO U-Turns allowed. Can't use the reverse of current dir
      if(!u_turn && (consider_dir == reverse[dir])) done=FALSE
      newx <- xe + orientationX[consider_dir]      
      newy <- ye + orientationY[consider_dir]      
      #Check if new direction is a legal direction
      if(outOfBoundsX(newx) || (outOfBoundsY(newy))) done=FALSE       
      
      if(loopCount > 1000) stop(WHILELOOP_ERROR) #new direction not found
    }
  if(entity==catcher)
    ifelse(dir == consider_dir, same<<-same+1, new <<- new+1)
  dir <- consider_dir #found a new direction to move along
  
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
    direction <- getDirection(entity, xe, ye, direction)
    
    #update coordinates and dir for entity
    coords[[entity]] <- c(xe, ye, direction)    
  } #else just continue
  
  if(debug) print(paste("getNext", entity,coords[[entity]][1],coords[[entity]][2],coords[[entity]][3] )) #for debugging

  
  return(coords)
}


# This function checks for the termination condition.
#' @return TRUE if catcher has caught up with target
catcher_found_target <- function(tsim, coords) {  
  
  if(tsim<catcherDelay) return(FALSE)
  if(tsim<rate[target] || tsim < rate[catcher]) return(FALSE)
  #have to be in the same (x,y) spot at the same time.
  if( (coords[[catcher]][1] == coords[[target]][1]) &&
        (coords[[catcher]][2] == coords[[target]][2])) return(TRUE)
  return(FALSE)
}

initialize_replication <- function(){  
  #Some error checking
  for (entity in target:catcher)  {
    if(outOfBoundsX(x[entity])) stop(OB_ERROR)
    if(outOfBoundsX(y[entity])) stop(OB_ERROR)    
  }
  
  #set an initial direction for both Catcher and Target
  direction[target]  <- sample(4,1)
  direction[catcher] <- sample(4,1)
  
  #initialize coords for catcher and Target
  coords <- list(c(x[target],y[target],direction[[target]]),c(x[catcher],y[catcher],direction[[catcher]]))
  if(debug_print) str(coords)  
  return(coords)
}

chase <- function(coords) { 
  
  coord_df <- NULL
  end_time <- tEndSim # If 0 is returned, unable to catch
  #set the dataframe of coords to Zero
  if(verbose_Output)
    coord_df <- reset_coord_data_frame(tEndSim) 
  
  #one replication of the catcher chasing target
  if(debug_print) print("start replications")
  for(tsim in 1:tEndSim) {
    if(debug_print) if(!tsim %% 100) print(tsim) #progress report
    for (entity in target:catcher)  {
      
      #for each beat of tsim, move the entity forward in its direction
      coords <- getNextXYForEntity(tsim, entity, coords) 
      
    if(verbose_Output) {
      #store it in a data frame to help plotting
      coord_df[tsim,colx[entity]] <- coords[[entity]][1] #store the x coord
      coord_df[tsim,coly[entity]] <- coords[[entity]][2] #store the y coord      
      coord_df[tsim,cold[entity]] <- visualDir[coords[[entity]][3]] #store the dir      
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