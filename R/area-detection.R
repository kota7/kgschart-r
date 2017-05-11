detect_graph_area <- function(arr)
{
  # detect top, bottom, left, right indices of the graph
  #
  # args:
  #   arr: array of size (3, nrow, ncol)
  #
  # returns:
  #   vector of size 4 of (top, bottom, left, right) location
  #   these are indices of enclosing box of the graph
  #   [1,0,1,0] if no graph is detected

  thres_dist <- 0.05
  distance <- rgb_dist(arr[,,dim(arr)[3] %/% 2, drop=FALSE],
                       .WHITE)
  white_rows = which(distance < thres_dist)
  if (length(white_rows) != 2) return(c(1,0,1,0))

  distance <- rgb_dist(arr[,dim(arr)[2] %/% 2,, drop=FALSE],
                       .WHITE)
  white_cols = which(distance < thres_dist)
  if (length(white_cols) != 2) return(c(1,0,1,0))

  c(white_rows, white_cols)
}
