# packages deepnet, nnet, stats are only used within the pretrained models
# hence the R check generates a note:
# "Namespace in Imports field not imported from"
# following import-from would avoid the note

#' @importFrom deepnet nn.predict nn.train
NULL
#' @importFrom nnet class.ind
NULL
#' @importFrom stats prcomp
NULL


#' @importFrom magrittr %>%
NULL


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

  stopifnot(is.array(arr))
  stopifnot(length(dim(arr))==3)
  stopifnot(dim(arr)[1]==3)

  if (length(color)!=3) color <- rep(color, length=3)

  R <- 2/9*((arr[1,,] - color[1])^2)
  G <- 4/9*((arr[2,,] - color[2])^2)
  B <- 3/9*((arr[3,,] - color[3])^2)
  sqrt(R+G+B)

  # old implementation, slower
  #apply(((arr-color)^2)*c(2/9, 4/9, 3/9),
  #      FUN=sum, MARGIN=c(2,3))^0.5
}



image_plot <- function(arr, show=FALSE, eps=1e-10)
{
  # Create graphic object of an image
  #
  # args:
  #   arr: array of size either (3, nrow, ncol) or (nrow, ncol).
  #        the former is RGB and the latter gray scale
  #   show: if TRUE, show image
  #   eps: very small value shifted to avoid rgb range error
  #
  # returns:
  #   ggplot

  if (length(dim(arr))==3) {
    # change to channel-last order
    arr <- aperm(arr, c(2,3,1))
  } else {
    x <- range(arr, na.rm=TRUE) + c(-eps, +eps)
    y <- c(max(x[1], 0), min(x[2], 1))
    b <- diff(y)/diff(x)
    a <- y[1]-b*x[1]
    arr <- a + b*arr

    # truncate to [0,1]
    #arr[arr > 1] <- 1
    #arr[arr < 0] <- 0
  }
  out <- grid::rasterGrob(arr)

  # put the graphic on ggplot
  out <- ggplot2::qplot(0.5, 0.5, xlim=c(0,1), ylim=c(0,1)) +
    ggplot2::theme_void() + ggplot2::xlab('') + ggplot2::ylab('') +
    ggplot2::annotation_custom(out)

  if (show) graphics::plot(out)

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
  # old ver. maybe slightly slower?
  #out <- apply(arr - black, MARGIN=c(2,3), FUN=crossprod, y=white - black)
  tmp <- (arr-black) * (white-black)
  out <- tmp[1,,] + tmp[2,,] + tmp[3,,]
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
  start <- which(arr & !c(FALSE, utils::head(arr, L-1)))
  end <- which(arr & !c(utils::tail(arr, L-1), FALSE))
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
    i2 <- i1 + target_rows-1
    arr <- arr[,i1:i2,, drop=FALSE]
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
    j2 <- j1 + target_cols-1
    arr <- arr[,,j1:j2, drop=FALSE]
    left <- 1
    right <- dim(arr)[3]
  }

  out <- array(value, dim=c(dim(arr)[1], target_rows, target_cols))
  out[,top:bottom,left:right] <- arr
  if (length(original_dim)==2) out <- drop(out)
  out
}



str_to_num_rank <- function(s)
{
  # convert str rating to numeric
  #
  # args:
  #   s: character vector of ratings, such as 3d, 2k
  #
  # returns:
  #   integer vector. NA for invalid strings
  #
  # note:
  #   2d=1, 1d=0, 1k=-1, 2k=-2 ...,
  #   i.e., dan's are positive and kyu's are negative

  tmp <- stringr::str_match(s, '([0-9]+)([dk])')
  n <- as.integer(tmp[,2])
  l <- tmp[,3]
  n[which(l=='k')] <- -n[which(l=='k')]
  n[which(l=='d')] <- n[which(l=='d')] - 1
  n
}


num_to_str_rank <- function(n)
{
  # convert numeric rating to str
  #
  # args:
  #   n: numeric vector
  #
  # returns:
  #   character vector
  s <- rep(NA_character_, length(n))
  s[which(n<0)]  <- paste(-n[which(n<0)], 'k', sep='')
  s[which(n>=0)] <- paste(1+n[which(n>=0)], 'd', sep='')
  s
}
