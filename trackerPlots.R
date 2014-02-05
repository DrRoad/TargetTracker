#' @description This function is just for plotting the progress of the 
#' target and the catcher with X. (It is a time-series plot)
#' Faceting is used just to make it visually easier to see how the 
#' two entities are moving around.
#' When the two x-coords and the two y-coords are the same, the target has been 'caught'
#' 
#' @param rowsToPlot tells the function to plot only the first rowsToPlot number of rows
library(ggplot2)
library(animation)


getGridlines <- function(numBlocksX, numBlocksY, blockLength) {
  #horizontal lines...
  xs<- rep(0, numBlocksY+1)
  ys <- seq(0,blockLength*(numBlocksY), by=blockLength)
  xe<- rep(blockLength*numBlocksX, numBlocksY+1)

  df1<- data.frame(xs,ys,xe,ys) #y value doesnt change for horiz lines  
  names(df1) <- c("xs","ys", "xe", "ye")

  #vertical lines
  ys<- rep(0, numBlocksX+1)
  ye<- rep(blockLength*numBlocksY, numBlocksX+1)
  x <- seq(0,blockLength*(numBlocksX), by=blockLength)
  df2<- data.frame(x, ys, x, ye)
  names(df2) <- c("xs","ys", "xe", "ye")
  
  return(rbind(df1,df2))
}
plotBaseGrid <- function(numBlocksX, numBlocksY, blockLength, fill="gray10") {  
#   p <- NULL
#   centers <- getCellCentersXY()
#   p.df <- data.frame(centers[[1]], centers[[2]])
#   p.df$cell <- 1:(side*side)
#   term.df <- merge(p.df, terminal.cells[c(3,5)], by.x=c("cell"), by.y=c("tcell"))    
  grid.df <- getGridlines(numBlocksX, numBlocksY, blockLength)
  
  #   names(p.df) <- c("cx", "cy", "cell")
  #   names(term.df) <- c("cell", "cx", "cy", "color")
  p <- ggplot() #+ geom_segment(aes(x, y, xend=xend, yend=yend, color=factor(color)), size=4)
  p <- p + geom_rect(aes(xmin=0, xmax=blockLength*numBlocksX, ymin=0, ymax=blockLength*numBlocksY), fill=fill)  # box bg
  p <- p + geom_segment(data=grid.df, aes(x=xs, y=ys, xend=xe, yend=ye), color="gray50", size=2)
  #  p <- p + geom_point(data=term.df, aes(x=cx, y=cy, color=factor(color)), size=12) + scale_colour_manual(values = colorpalette) # terminal nodes
  p <- p + guides(color=FALSE) + theme(panel.background=element_rect(fill="white"), panel.grid.minor=element_blank())
  p <- p + scale_x_continuous(breaks=seq(0, blockLength*numBlocksX, blockLength)) + scale_y_continuous(breaks=seq(0, blockLength*numBlocksY, blockLength))
  return(p)
}



plotChaseTimeSnip <- function (coord_df, start_t, end_t, bgcolor="gray10") {
  p <- plotBaseGrid(numBlocksX, numBlocksY, blockLength, fill=bgcolor)
  
  ptarget  <- geom_point(data=coord_df[start_t:end_t, ], 
                         aes(x=targetX, y=targetY), 
                         color="yellow", size=3)
  colscale <-  scale_color_gradient(low="orange", high="red")
  
  pcatcher <- geom_point(data=coord_df[start_t:end_t, ], 
                         aes(x=catcherX, y=catcherY, color=time), 
                         size=4)
  catcherHead <- geom_point(data=coord_df[end_t, ], 
                         aes(x=catcherX, y=catcherY), 
                         size=5, color="white")
  
  p + ptarget + colscale + pcatcher + catcherHead
}



drawXY_Over_Time_v2 <- function(df, time_replication_ended, rowsToPlot=time_replication_ended){  
  useful_df <- df[1:rowsToPlot , 1:5 ] #subset to what is needed
  #Pad the rows from activeRows to rowsToPlot to be Zero
  #useful_df[rowsToPlot+1:time_replication_ended, ] <- 0    
  useful_df <- melt(useful_df, id.vars="time")
  useful_df <- useful_df[complete.cases(useful_df), ] #drop all the NA's
  #introduce two new indicator columns. They are useful to split the graph using facet_grid
  #useful$X is X for targetX or CatcherX rows, 'Y' othw in the melted df
  useful_df$X <- ifelse( useful_df$variable=="targetX"|useful_df$variable=="catcherX", "X", "Y")
  useful_df$entity <- ifelse( useful_df$variable=="targetX"|useful_df$variable=="targetY", "Target", "Catcher")
  
  vertical_boundary <- max(numBlocksX, numBlocksY)*blockLength + 10
  p <- ggplot(useful_df, aes(x=time, y=value, group=variable)) + 
    geom_line(aes(color=factor(entity)), size=1)
  p <- p + scale_color_manual(values=c("Red", "Yellow"))
  p <- p + theme(legend.title=element_blank())
  p <- p + facet_grid(X ~ . )
  p <- p + coord_cartesian(xlim = c(-5, time_replication_ended+100),
                           ylim = c(-5, vertical_boundary)) 
  p <- p + theme(panel.background= element_rect("black") )
  p
}

# a simpler plotting function. But the interpretation is less clearer
drawProgress2 <- function(df){  
  useful_rows <- max(which(df$targetX != 0)) #cut out the bottom part which is all zeros
  useful_df <- df[1:useful_rows , ]
  p <- ggplot(useful_df, aes(x=time)) + geom_line(aes(y=targetX), color="darkred")+ geom_line(aes(y=targetY)) +
    geom_line(aes(y=catcherX), color="red")+ geom_line(aes(y=catcherY), color="darkgreen")
  p <- p + facet_grid()
}


########----------
# Animation
#-----------------

create_HTMLAnimation_of_Chase_Progress <- function (coord_df, time_replication_ended, numImages=20) {
  #time increment to skip forward for each image
  timePerImage <- round(time_replication_ended+100,-2)/numImages
  saveHTML({
    par(mar = c(4, 0.5, 0.5, 0.5)) #setting margings
    for (i in 1:20) {   
      print(paste("animation: Generating Image:", i))
      xyp <- drawXY_Over_Time_v2(coord_df, time_replication_ended, i*timePerImage)
      print(xyp)
      ani.pause() #default pause and flush device
    }
  }, img.name = "chase3_gg", imgdir = "chase3_dir", 
  htmlfile = "chaseProgress3.html", autobrowse = FALSE, 
  title = "Progress of Catcher Tracking a Target", 
  outdir = getwd(),
  description = c("Shows a catcher chasing a Target.\n\n", 
                  "Can Turn at any Intersection")
  )
  
  
}

create_HTMLAnimation_of_Chase <- function (coord_df, 
                                       time_replication_ended, 
                                       numImages=20)   {
  #time increment to skip forward for each image
  timePerImage <- round(time_replication_ended+100,-2)/numImages
  saveHTML({
    par(mar = c(4, 0.5, 0.5, 0.5)) #setting margings
    for (i in 1:numImages) {   
      print(paste("animation: Generating Image:", i))
      start_t <- (i-1)*timePerImage
      end_t <- start_t + timePerImage
      xyp <- plotChaseTimeSnip(coord_df, start_t, end_t)
      print(xyp)
      ani.pause() #default pause and flush device
    }
  }, img.name = "chase_gg", imgdir = "chase_dir", 
  htmlfile = "chase3.html", autobrowse = FALSE, 
  title = "Catcher Tracking a Target", 
  outdir = getwd(),
  description = c("Shows a catcher chasing a Target.\n\n", 
                  "Can Turn at any Intersection")
  )
  
  
}  

create_GIFAnimation_of_Chase <- function (coord_df, 
                                       time_replication_ended, 
                                       numImages=20)   {
  #time increment to skip forward for each image
  timePerImage <- round(time_replication_ended+10,-2)/numImages
  saveGIF({
    par(mar = c(4, 0.5, 0.5, 0.5)) #setting margings
    for (i in 1:numImages) {   
      print(paste("animation: Generating GIF:", i))
      start_t <- (i-1)*timePerImage
      end_t <- start_t + timePerImage
      xyp <- plotChaseTimeSnip(coord_df, start_t, end_t)
      print(xyp)
      ani.pause() #default pause and flush device
    }
    print(plotChaseTimeSnip(coord_df, 
                            time_replication_ended, 
                            time_replication_ended,
                            bgcolor="gray30"))
  }, movie.name = "chase.gif", 
  interval = 0.2, 
  ani.width = 500, 
  ani.height = 400,
  outdir = getwd(),
  loop=TRUE
  )
  
  
}  




