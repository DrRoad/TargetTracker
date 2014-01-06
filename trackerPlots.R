#' this function is just for plotting the progress of the 
#' target and the catcher with X. (It is a time-series plot)
#' Faceting is used just to make it visually easier to see how the 
#' two entities are moving around.
#' When the two x-coords and the two y-coords are the same, the target has been 'caught'
#' 
drawXY_Over_Time <- function(df){  
  useful_rows <- max(which(df$targetX != 0)) #cut out the bottom part which is all zeros
  useful_df <- df[1:useful_rows , ]
  useful_df <- melt(useful_df, id.vars="time")
  useful_df$entity <- (useful_df$variable=="targetX")
  
  #introduce two new indicator columns. They are useful to split the graph using facet_grid
  #useful$X is X for targetX or CatcherX rows, 'Y' othw in the melted df
  useful_df$X <- ifelse( useful_df$variable=="targetX"|useful_df$variable=="catcherX", "X", "Y")
  useful_df$entity <- ifelse( useful_df$variable=="targetX"|useful_df$variable=="targetY", "Target", "Catcher")
  
  p <- ggplot(useful_df, aes(x=time, y=value, group=variable)) + geom_line(aes(color=factor(entity)))
  p <- p + theme(legend.title=element_blank())
  p <- p + facet_grid(X ~ . )
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
