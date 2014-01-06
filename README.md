Target Tracker
===============


This project is an R implementation of the following problem.
The problem was posed to me by Krishan Alamuru.

He wrote:
##Background: 
On a recent day, my wife went for a walk in the neighborhood. I needed to find her (she didn't have her phone with her). I am familiar with the general area she walks but not sure of the exact path and where she is on that path.  The question is: Not knowing exactly where she is and where she is headed - how do I find her?  My only advantage is that I'll be driving and she is walking.

##General problem
Imagine a moving target on a network of connected streets. (To begin with we can imagine streets aligned to a rectangular grid). A searching agent travelling at a different speed (say x times that of moving target) needs to find the moving target.

###Things to think about:  

    Is there a simple formulation that'll make it trivial to solve problems like these?
        I'm guessing not. It may be trivial in some instances but may not be trivial even to find a feasible solution in other instances.
    What are some interesting strategies?
        If the speed 'x' is very large compared to '1' and the total length of the network is not big, guess just random driving itself is not a bad strategy.
    Seems like some layouts (of the streets) lend themselves to better strategies than others.  Any general characterization where we can be sure of having a good strategy?
    Are there layouts where it is possible that the target may never be found in some instances?
        You might think of a simple loop and 'x' being less than '1'.  But even in that case the searching agent can go along the loop in clockwise direction for sufficiently long and the reverse the course for sufficiently long to make sure target can definitely be found.
        I think the answer is 'no' - but I don't have a quick proof.
    Assuming a complete random path by the moving target - are there strategies to minimize the time to find the target?
    Assuming that a search agent can look along any straightline without having to traverse it - how can a strategy take advantage of this (especially at intersections, looking around in all directions.).
    Can you think of a good way to turn this game into an App based game?

## Discrete Event Simulation

The problem above has been implemented in R, using DES (Discrete Event Simulation) Concepts.

To run this, you need to install all the R files in one directory.

Inside R:
<code>
setwd("~/<path to your directory")
source("~/tracker_main.R")
</code>

###TODO:
Add animation
Add Rules for when the target (or catcher) turns around.
Collect Stats under different parameters.
Things to vary:
1.   catcher delay
2.  Rates 1 & 2
3.  starting Coords
4.  Geometry of the road network
