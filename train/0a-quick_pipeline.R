# quick implementation of pipeline-like functionality


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
    # set retx=FALSE, to avoid keeping the rotated data inside
    if (is.null(model)) model <<- stats::prcomp(data$x, retx=FALSE, ...)

    # to save storage, cut the rotation matrix above the required size (n)
    model$rotation <<- model$rotation[, 1:n]
  }

  transform <- function(data)
  {
    #data$x <- predict(model, data$x)[, 1:n]
    data$x <- scale(data$x, model$center, model$scale) %*% model$rotation
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
    data$y <- format_y(data$y)
    if (is.null(model)) {
      model <<- deepnet::nn.train(data$x, data$y,
                                  hidden=hidden, output=output, ...)
      if (output=='softmax') labels <<- colnames(data$y)
    } else {
      model <<- deepnet::nn.train(data$x, data$y,
                                  initB=model$B, initW=model$W,
                                  hidden=hidden, output=output, ...)
    }

    # remove unnecessary big attributes, to make the object size small
    model$post <<- NULL
    model$pre  <<- NULL
    model$e    <<- NULL
    model$vW   <<- NULL
    model$vB   <<- NULL
  }

  format_y <- function(y)
  {
    # convert y into one-hot format, if it is not a matrix

    # already matrix, return as is
    if (is.matrix(y)) return(y)

    # currently a vector, convert to a factor
    # use the predefiend labels if any
    if (is.vector(y)) {
      if (is.null(labels)) {
        y <- factor(y)
      } else {
        y <- factor(y, levels=labels)
      }
    }
    nnet::class.ind(y)
  }

  prediction <- function(x, ...)
  {
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

  prediction <- function(x, ...)
  {
    data <- list(x=x, y=NULL)
    for (i in seq_along(steps))
    {
      if (i != length(steps)) {
        data <- steps[[i]]$transform(data)
      } else {
        return(steps[[i]]$prediction(data$x, ...))
      }
    }
  }
  self <- environment()
  self
}
