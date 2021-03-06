#-----------------------
#The following parameters can be modified

#How many replications of the Catcher Target Chase?
#Make kNumReplications to be 1 if using Plots
kNumReplications <- 1

# RUNTIME PARAMETERS
# verbose_flag is an indicator
# If it is 1, then the details of each replication are stored
# If it is 0, then only the top-level stats of the Chase are stored
verbose <- 1
debug <- 0 #1 if debug mode is on
debug_print <- FALSE #Will spit out values if TRUE
#####

# Geography of the "Grid Network"
# How many 'streets' are there? Along X and Along Y?
numBlocksX  <- 5
numBlocksY  <- 4
#This parameter sets the distance between Streets
blockLength <- 10
# This means that the highest legal (x,y) value is: 
# (numBlocksX * blockLength, numBlocksX * blockLength)


#time parameters
tEndSim <- 10000

#the above means that the target moves at some speed. once every t beats
#the catcher moves at a different speed. Once every c beats
#Set the rates.
rate[target] <- 5 #higher value means slower movement.
rate[catcher] <- 1

#In the version of the Problem we are simulating, the Catcher Doesn't
# start until a certain has passed since the Target Started.
catcherDelay <- 100 #number of beats after which catcher starts search

#Where are the starting positions of the Starter? The Target?
#The two could be the same (x,y) Location, but they don't have to be.
#Make sure that the (x,y) lies within the Overall Area
#TODO: Need to check for this with OutofBounds
x[target] <- 20 #starting point for target
y[target] <- 20 #starting point for target
x[catcher] <- 10 #starting point for catcher
y[catcher] <- 10 #starting point for catcher


#TURNING RULES
# Turning means that the Direction is changed.
# So this parameter refers to where and when the Target and Catcher can turn
# We define an (interection, end, and Corner)

#Definition:
#  Intersection - where two streets meet
#  End - Where one of the streets has a deadend.
#  Corner - One of the 4 corners of the Area
# U-TURN - Where the entity makes a 180 deg turn
Uturn[1] <- c()

turns <- 

turns[entity, point]
  
  
### CONSTANTS --------------
# You wouldn't change these typically
### CONSTANTS --------------

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
x <- c(0,0); 
y <- c(0,0) # initialize with placeholders
rate <- c(0,0)

