rgb_dist <- function(arr, color)
{
  # Compute cell-wise distance between array and a color
  #
  # args:
  #   arr:   array of size (3, nrow, ncol)
  #   color: vector of size 3
  #
  # return:
  #   matrix of size (nrow, ncol)
  #
  # Note:
  #   distance is computed by weighted Euclidean distance
  #   with weight [2,4,9]
  #   https://en.wikipedia.org/wiki/Color_difference

  apply(((arr-color)^2)*c(2/9, 4/9, 3/9),
        FUN=sum, MARGIN=c(2,3))^0.5
}



image_plot <- function(arr)
{
  grid::grid.newpage()
  grid::grid.raster(aperm(arr, c(2,3,1)))
}
