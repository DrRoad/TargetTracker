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
#' 
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
debug_print <- 0
#####

#time parameters
tEndSim <- 10000
rate <- c(4,2) #target, catcher
#rate <- c(1,1) #target, catcher
#the above means that the target moves at some speed. once every t beats
#the catcher moves at a different speed. Once every c beats
catcherDelay <- 10 #number of beats after which catcher starts search

# Initialization for this run
x <- c(0,0); y <- c(0,0) # initialize with placeholders
x[target] <- 20 #starting point for target
y[target] <- 20 #starting point for target
x[catcher] <- 10 #starting point for catcher
y[catcher] <- 10 #starting point for catcher

