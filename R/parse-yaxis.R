extract_axis_letters <- function(x)
{
  # Extract all letters from a yaxis image
  #
  # args:
  #   x: array of size (3, nrow, ncol) representing yaxis image
  #
  # return:
  #   list of array of size (nrow, ncol), representing gray scale images

  if (is.null(x)) return(NULL)
  stopifnot(is.array(x))
  stopifnot(length(dim(x)) == 3)
  stopifnot(dim(x)[1] == 3)

  thres_dist <- 0.1
  thres_frac <- 0.95

  dist <- rgb_dist(x, .BEIGE)
  is_beige = (dist < thres_dist)

  frac <- apply(is_beige, FUN=mean, MARGIN=1)
  is_bg_row <- (frac >= thres_frac)
  tmp <- detect_consecutive_false(is_bg_row)
  row1 <- tmp[,1]
  row2 <- tmp[,2]

  split_letters <- function(i1, i2)
  {
    frac <- apply(is_beige[i1:i2,], FUN=mean, MARGIN=2)
    is_bg_col <- (frac >= thres_frac)
    tmp <- detect_consecutive_false(is_bg_col)
    start <- tmp[,1]
    end <- tmp[,2]
    Map(function(j1,j2) x[,i1:i2, j1:j2], start, end)
  }

  out <- Map(split_letters, row1, row2)

  # flattens "list of lists of arrays" into list of arrays
  out <- do.call(c, out)

  # convert to gray scale
  lapply(out, to_gray, white=.BEIGE, black=.GRAY)
}
