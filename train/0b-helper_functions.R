shift_matrix <- function(m, rows=0, cols=0, value=0)
{
  # shift matrix and pad arbitrary value
  #
  # args:
  #   m: matrix
  #   rows: integer, vertical shift
  #   cols: integer, horizontal shift
  #   value: padded value
  #
  # returns:
  #   matrix

  if (abs(rows) >= nrow(m) || abs(cols) >= ncol(m)) {
    m[] <- value
    return(m)
  }

  if (rows > 0) {
    m <- rbind(
      matrix(value, nrow=rows, ncol=ncol(m)),
      m[1:(nrow(m)-rows),]
    )
  } else if (rows < 0) {
    m <- rbind(
      m[(1-rows):nrow(m),],
      matrix(value, nrow=-rows, ncol=ncol(m))
    )
  }

  if (cols > 0) {
    m <- cbind(
      matrix(value, nrow=nrow(m), ncol=cols),
      m[,1:(ncol(m)-cols)]
    )
  } else if (cols < 0) {
    m <- cbind(
      m[,(1-cols):ncol(m)],
      matrix(value, nrow=nrow(m), ncol=-cols)
    )
  }

  m
}


generate_augmented_data <- function(
  n, X, Y,
  avg_shift_rows=0.05, max_shift_rows=2,
  avg_shift_cols=0.05, max_shift_cols=2,
  noise_sd=0.03)
{
  # Generate augmented image data, associated with labels
  #
  # args:
  #   n: number of generated data
  #   X,Y: original image and label data
  #        X must be a 3d-array of size (N, nrow, ncol)
  #        Y must be a vector of target labels
  #   avg_shift_rows,avg_shift_cols:
  #        average shift sizes in fraction to the original size
  #   max_shift_rows,max_shift_cols:
  #        maximum shift size
  #   noise_sd: degree of noise to be added
  index <- sample.int(dim(X)[1], n, replace=TRUE)

  out_X <- X[index,,, drop=FALSE]
  out_Y <- Y[index]


  # shift
  if (avg_shift_rows > 0 | avg_shift_cols > 0) {
    lambda_row <- dim(X)[2]*avg_shift_rows
    lambda_col <- dim(X)[3]*avg_shift_cols

    shift_rows <- pmin(rpois(n, lambda_row), max_shift_rows)
    shift_cols <- pmin(rpois(n, lambda_col), max_shift_cols)
    shift_rows <- shift_rows*(1-2*as.integer(runif(n)>0.5))
    shift_cols <- shift_cols*(1-2*as.integer(runif(n)>0.5))
    for (i in 1:n)
      out_X[i,,] <- shift_matrix(out_X[i,,], shift_rows[i], shift_cols[i], 1)
  }

  # add noise
  if (noise_sd > 0) {
    noise <- array(rnorm(prod(dim(out_X)), sd=noise_sd),
                   dim=dim(out_X))
    out_X <- out_X + noise
  }

  list(X = out_X, Y = out_Y)
}




random_plot <- function(X, Y, Y2=NULL)
{
  n <- min(15, length(Y))
  index <- sample.int(length(Y), n)
  grob_list <- lapply(index, function(i) {
    label <- Y[i]
    if (!is.null(Y2)) label <- paste(label, Y2[i], sep='/')
    #qplot(0.5, 0.5, xlim=c(0,1), ylim=c(0,1)) +
      #theme_void() + xlab('') + ylab('') +
      #annotation_custom(kgschart:::image_plot(X[i,,])) +
    kgschart:::image_plot(X[i,,]) +
      geom_text(aes(x, y, label=label),
                data=data.frame(x=0.1, y=1, label=label))
  })

  out <- arrangeGrob(grobs=grob_list, nrow=3, ncol=5)
  plot(out)
}


random_plot_inaccurate <- function(p, X, Y)
{
  # randomly plot inaccurate cases
  pre <- p$prediction(X)
  ind <- (pre != Y)
  random_plot(X[ind,,], Y[ind], pre[ind])
}




ngram <- function(s, n)
{
  # return n-gram from string s
  #
  # args:
  #   s: a character
  #   n: n
  #
  # returns:
  #   character vector
  out = character(0)
  for (i in n:nchar(s))
  {
    out <- c(out, substring(s, i-n+1, i))
  }
  unique(out)
}
