get_line_index <- function(x)
{
  # Obtain the indices of line plot
  #
  # args:
  #   x: array of size (3, nrow, ncol) representing the graph image
  #
  # returns:
  #   vector of size equal to ncol

  if (is.null(x)) return(NULL)
  stopifnot(is.array(x))
  stopifnot(length(dim(x)) == 3)
  stopifnot(dim(x)[1] == 3)

  dist_green <- rgb_dist(x, .GREEN)
  thres_dist <- 0.35

  dist_green[dist_green > thres_dist] <- 1

  #min_dist <- apply(dist_green, FUN=min, MARGIN=2)
  min_dist <- matrixStats::colMins(dist_green)
  no_line <- which(min_dist >= 1)

  out <- apply(t(dist_green) == min_dist, MARGIN=1,
               FUN=function(a) mean(which(a)))

  out[no_line] <- NA_real_
  out
}


get_num_grids <- function(x)
{
  # Obtain the indices of line plot
  #
  # args:
  #   x: array of size (3, nrow, ncol) representing the graph image
  #
  # returns:
  #   integer of the number of y-axis grids

  if (is.null(x)) return(NULL)
  stopifnot(is.array(x))
  stopifnot(length(dim(x)) == 3)
  stopifnot(dim(x)[1] == 3)

  thres_dist <- 0.1
  thres_frac <- 0.9

  # this perhaps very slightly slower
  # mostly_gray <- (rgb_dist(x, .GRAY) < thres_dist)  |
  #                (rgb_dist(x, .DGRAY) < thres_dist) |
  #                (rgb_dist(x, .LGRAY) < thres_dist)
  a1 <- rgb_dist(x, .GRAY)
  a2 <- rgb_dist(x, .DGRAY)
  a3 <- rgb_dist(x, .LGRAY)
  mostly_gray <- (pmin(a1, a2, a3) < thres_dist)

  frac <- rowMeans(mostly_gray)
    #apply(mostly_gray, MARGIN=1, FUN=mean)
  gray_index <- which(frac > thres_frac)

  if (length(gray_index) == 0) return(0L)
  1L + sum(diff(gray_index) > 1)
}



