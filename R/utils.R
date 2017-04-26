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



image_plot <- function(arr, plot=FALSE)
{
  # Create graphic object of an image
  #
  # args:
  #   arr: array of size either (3, nrow, ncol) or (nrow, ncol).
  #        the former is RGB and the latter gray scale
  #   plot: if TRUE, show image
  #
  # returns:
  #   grid::rastergrob

  if (length(dim(arr))==3) {
    # change to channel-last order
    arr <- aperm(arr, c(2,3,1))
  } else {
    # scale to [0,1]
    x <- range(arr, na.rm=TRUE)
    y <- c(max(x[1], 0), min(x[2], 1))
    b <- diff(y)/diff(x)
    a <- y[1]-b*x[1]
    arr <- a + b*arr
  }
  out <- grid::rasterGrob(arr)

  if (plot) {
    grid::grid.newpage()
    grid::grid.draw(out)
  }
  invisible(out)
}


to_gray <- function(arr, white, black)
{
  # Make RGB image into gray scale
  #
  # args:
  #   arr: array of size (3, nrow, ncol)
  #   white: vector of size 3
  #   black: vector of size 3
  #
  # return:
  #   matrix of size (nrow, ncol)
  weights <- c(2/9, 4/9, 3/9)^0.5
  white <- white * weights
  black <- black * weights
  arr <- arr * weights

  denom <- as.numeric(crossprod(black - white))
  out <- apply(arr - black, MARGIN=c(2,3), FUN=crossprod, y=white - black)
  out/denom
}

detect_consecutive_true <- function(arr)
{
  # Find start/end indices of consecutive true's
  #
  # args:
  #   arr: logical vector
  #
  # return:
  #   matrix of two columns, start and end
  stopifnot(is.logical(arr))
  if (length(arr)==0) return(NULL)
  L <- length(arr)
  start <- which(arr & !c(FALSE, head(arr, L-1)))
  end <- which(arr & !c(tail(arr, L-1), FALSE))
  stopifnot(length(start) == length(end))
  cbind(start, end)
}

detect_consecutive_false <- function(arr)
{
  detect_consecutive_true(!arr)
}



pad_crop_image <- function(arr, target_rows, target_cols, value)
{
  # Pad and/of crop image to target size
  #
  # args:
  #   arr: array of size (3, nrow, ncol) or matrix of size (nrow, ncol)
  #   target_rows, target_cols: output shape
  #   value: value to pad
  #
  # returns:
  #   array of matrix with target size
  original_dim <- dim(arr)
  if (length(dim(arr)) == 2) arr <- array(arr, dim=c(1, dim(arr)))
  stopifnot(length(value) == dim(arr)[1])

  rows_toadd <- target_rows - dim(arr)[2]
  if (rows_toadd >= 0) {
    top <- 1 + (rows_toadd %/% 2)
    bottom <- top - 1 + dim(arr)[2]
  } else {
    # crop rows
    i1 <- -rows_toadd %/% 2
    i2 <- i1 + target_rows
    arr <- arr[,i1:i2,]
    top <- 1
    bottom <- dim(arr)[2]
  }

  cols_toadd <- target_cols - dim(arr)[3]
  if (cols_toadd >= 0) {
    left <- 1 + (cols_toadd %/% 2)
    right <- left - 1 + dim(arr)[3]
  } else {
    # crop cols
    j1 <- -cols_toadd %/% 2
    j2 <- j1 + target_cols
    arr <- arr[,,j1:j2]
    left <- 1
    right <- dim(arr)[3]
  }

  out <- array(value, dim=c(dim(arr)[1], target_rows, target_cols))
  out[,top:bottom,left:right] <- arr
  if (length(original_dim)==2) out <- drop(out)
  out
}
