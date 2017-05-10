extract_caption_letters <- function(x)
{
  # extract all letters from caption image
  #
  # args:
  #   x: array of size (3, nrow, ncol), that represents caption area
  #
  # returns:
  #   list of matrices, each represents a gray scale images

  if (is.null(x)) return(list())
  stopifnot(is.array(x))
  stopifnot(length(dim(x)) == 3)
  stopifnot(dim(x)[1] == 3)

  thres_dist <- 0.25
  thres_frac <- 1.0

  dist <- rgb_dist(x, .BEIGE)
  is_beige <- (dist < thres_dist)

  # find rows that include non-background
  not_bg_row <- (apply(is_beige, MARGIN=1, FUN=mean) < thres_frac)
  if (all(!(not_bg_row))) return(list()) # all background
  # the first and last index of non-background
  tmp <- range(which(not_bg_row))
  i1 <- tmp[1]
  i2 <- tmp[2]

  # split into letters
  frac <- apply(is_beige[i1:i2, , drop=FALSE], MARGIN=2, FUN=mean)
  is_bg_col <- (frac >= thres_frac)
  tmp <- detect_consecutive_false(is_bg_col)
  if (nrow(tmp) < 1) return(list())
  out <- lapply(1:nrow(tmp), function(k) x[, i1:i2, tmp[k,1]:tmp[k,2], drop=FALSE])

  # to gray scale
  lapply(out, to_gray, white=.BEIGE, black=.GRAY)
}
