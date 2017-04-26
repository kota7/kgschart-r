detect_graph_area <- function(arr)
{
  # detect top, bottom, left, right indices of the graph
  #
  # args:
  #   arr: array of size (3, nrow, ncol)
  #
  # returns:
  #   vector of size 4
  #   [1,1,1,1] if a graph is not detected

  thres_dist <- 0.05
  distance <- rgb_dist(arr[,,dim(arr)[3] %/% 2, drop=FALSE],
                       .WHITE)
  white_rows = which(distance < thres_dist)

  distance <- rgb_dist(arr[,dim(arr)[2] %/% 2,, drop=FALSE],
                       .WHITE)
  white_cols = which(distance < thres_dist)
  c(white_rows, white_cols)
}
