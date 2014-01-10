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

##Initialize The TURNS Data Frame
turns <- data.frame(matrix(TRUE, ncol = 3, nrow = 2))
names(turns) <- c("Intersection", "End", "Corner")
turns
