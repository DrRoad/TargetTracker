#-----------------------
#The following parameters can be modified

#How many replications of the Catcher Target Chase?
#Make kNumReplications to be 1 if using Plots
kNumReplications <- 1

# RUNTIME PARAMETERS
# verbose_flag is an indicator
# If it is 1, then the details of each replication are stored
# If it is 0, then only the top-level stats of the Chase are stored
verbose_Output <- 1
debug <- 0 #1 if debug mode is on
debug_print <- FALSE #Will spit out values if TRUE
plot_Option <- TRUE
create_Animation <- FALSE
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
catcherDelay <- 50 #number of beats after which catcher starts search

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
#  "Intersection" - where two streets meet
#  "End" - Where one of the streets has a deadend.
#  "Corner" - One of the 4 corners of the Area
# U-TURN - Where the entity makes a 180 deg turn

#By default, U-TURNS are allowed at all special points.
#Set anything to be FALSE to stop that.
# for example turns[target, "End"] <- FALSE
uturns[target, "Intersection"] <- FALSE
uturns[target, "End"] <- FALSE
uturns[target, "Corner"] <- FALSE

  
# TTD:

# Table of Variables:
#   catcher delay
#   Rates 1 & 2
#   starting Coords
#   Geometry
# 
#  replication Geometry, stx, sty, enx, eny, delay, rate1, rate2, ended
#   1123, 5,4, 10,10, 20,20, delay=20, rate 4,2, time_ended


# Future ideas to explore
#  * directions more than 4?!
#  * more than one catcher
#  * get rid of bounds - 


