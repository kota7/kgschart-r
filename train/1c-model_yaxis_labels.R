library(kgschart)
library(abind)
library(grid)
library(gtable)
library(gridExtra)
library(ggplot2)
library(OpenImageR)
library(magrittr)

set.seed(87)

datadir <- 'train/data/yaxis/'
X <- dget(file.path(datadir, 'X'))
Y <- dget(file.path(datadir, 'Y'))

# combine X into array (N, nrow, ncol)
X <- abind(X, along=0)


generate_augmented_data <- function(
  n, X, Y, avg_shift_rows=0.02, avg_shift_cols=0.02, noise_sd=0.03)
{
  # Generate augmented image data, associated with labels
  #
  # args:
  #   n: number of generated data
  #   X,Y: original image and label data
  #        X must be an 3d-array of size (N, nrow, ncol)
  #        Y must be a vector of target labels
  #   avg_shift_rows,avg_shift_cols:
  #        average shift sizes in fraction to the original size
  #   noise_sd: degree of noise to be added
  index <- sample.int(dim(X)[1], n, replace=TRUE)

  out_X <- X[index,,, drop=FALSE]
  out_Y <- Y[index]

  # add noise
  if (noise_sd > 0) {
    noise <- array(rnorm(prod(dim(out_X)), sd=noise_sd),
                   dim=dim(out_X))
    out_X <- out_X + noise
  }

  # shift
  if (avg_shift_rows > 0 | avg_shift_cols > 0) {
    lambda_row <- dim(X)[2]*avg_shift_rows
    lambda_col <- dim(X)[3]*avg_shift_cols

    shift_rows <- (1-2*as.integer(runif(n)>0.5)) * rpois(n, lambda_row)
    shift_cols <- (1-2*as.integer(runif(n)>0.5)) * rpois(n, lambda_col)
    # add -1 if negative since -1 is treated as 0, and so forth
    # in Augmentation function
    #shift_rows <- shift_rows - as.integer(shift_rows<0)
    #shift_cols <- shift_cols - as.integer(shift_cols<0)

    # flip the image color temporarily 0 <-> 1
    # since Augmentation function pads 0 (black) in.
    #out_X <- 1 - out_X
    for (i in 1:n)
      out_X[i,,] <- Augmentation(out_X[i,,],
                                 shift_rows=shift_rows[i],
                                 shift_cols=shift_cols[i],
                                 padded_value=1)
    #out_X <- 1 - out_X
  }
  list(X = out_X, Y = out_Y)
}

# generate train and test data
tmp <- generate_augmented_data(10000, X, Y)
X_tr <- tmp$X
Y_tr <- tmp$Y

tmp <- generate_augmented_data(10000, X, Y)
X_te <- tmp$X
Y_te <- tmp$Y



random_plot <- function(X, Y, Y2=NULL)
{
  n <- min(15, length(Y))
  index <- sample.int(length(Y), n)

  grob_list <- lapply(index, function(i) {
    label <- Y[i]
    if (!is.null(Y2)) label <- paste(label, Y2[i], sep='/')
    qplot(0.5, 0.5, xlim=c(0,1), ylim=c(0,1)) +
      theme_void() + xlab('') + ylab('') +
      annotation_custom(kgschart:::image_plot(X[i,,])) +
      geom_text(aes(x, y, label=label),
                data=data.frame(x=0.1, y=1, label=label))
  })

  out <- arrangeGrob(grobs=grob_list, nrow=3, ncol=5)
  plot(out)
}
random_plot(X_tr, Y_tr)
random_plot(X_te, Y_te)



# create pipeline like object
OneHot <- function(...)
{
  fit <- function(data) {}
  transform <- function(data)
  {
    data$y <- nnet::class.ind(data$y)
    data
  }
  self <- environment()
  self
}

Flatten <- function(...)
{
  fit <- function(data) {}

  transform <- function(data)
  {
    dim(data$x) <- c(dim(data$x)[1], dim(data$x)[2]*dim(data$x)[3])
    data
  }

  self <- environment()
  self
}

PCA <- function(n, ...)
{
  model <- NULL
  fit <- function(data)
  {
    model <<- prcomp(data$x, ...)
  }

  transform <- function(data)
  {
    data$x <- predict(model, data$x)[, 1:n]
    data
  }

  self <- environment()
  self
}

MLP <- function(hidden, output, ...)
{
  model <- NULL
  labels <- NULL
  fit <- function(data)
  {
    if (is.null(model)) {
      #model <<- nnet::nnet(data$x, data$y, ...)
      model <<- deepnet::nn.train(data$x, data$y, hidden=hidden, output=output, ...)
      if (output=='softmax') labels <<- colnames(data$y)
    } else {
      #model <<- nnet::nnet(data$x, data$y, Wts=model$wts, ...)
      model <<- deepnet::nn.train(data$x, data$y,
                                  initB=model$B, initW=model$W,
                                  hidden=hidden, output=output, ...)
    }
  }

  pred <- function(x, ...)
  {
    #if (is.null(x)) predict(model, ...) else predict(model, x, ...)
    p <- deepnet::nn.predict(model, x)
    if (output=='softmax') labels[max.col(p)] else p
  }

  self <- environment()
  self
}

Pipeline <- function(...)
{
  steps <- list(...)

  fit <- function(x, y, incr=FALSE)
  {
    data <- list(x=x, y=y)
    for (i in seq_along(steps))
    {
      if (!incr | i != length(steps)) steps[[i]]$fit(data)
      if (i != length(steps)) data <- steps[[i]]$transform(data)
    }
  }

  pred <- function(x=NULL, ...)
  {
    data <- list(x=x, y=NULL)
    for (i in seq_along(steps))
    {
      if (i != length(steps)) {
        data <- steps[[i]]$transform(data)
      } else {
        return(steps[[i]]$pred(data$x, ...))
      }
    }
  }
  self <- environment()
  self
}

p <- Pipeline(oh=OneHot(),
              fl=Flatten(),
              pc=PCA(50),
              #ml=MLP(size=50, MaxNWts=1e+4, softmax=TRUE),
              ml=MLP(hidden=c(50,50), output='softmax'))

p$fit(X_tr, Y_tr)

tbl <- table(Y_tr, p$pred(X_tr))
cat(100*sum(diag(tbl))/sum(tbl), '%')
tbl <- table(Y, p$pred(X))
cat(100*sum(diag(tbl))/sum(tbl), '%')
tbl <- table(Y_te, p$pred(X_te))
cat(100*sum(diag(tbl))/sum(tbl), '%')

