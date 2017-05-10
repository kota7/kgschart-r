library(kgschart)
library(abind)
library(grid)
library(gtable)
library(gridExtra)
library(ggplot2)
library(OpenImageR)
library(magrittr)
library(tidyr)
library(reshape2)
library(yaml)

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
# train data is to create the PCA decomposition and the initial MLP model
# test data is used for performance check
tmp <- generate_augmented_data(10000, X, Y)
X_tr <- tmp$X
Y_tr <- tmp$Y

tmp <- generate_augmented_data(2000, X, Y)
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

p <- Pipeline(#oh=OneHot(),
              fl=Flatten(),
              pc=PCA(60),
              ml=MLP(hidden=c(30,30), output='softmax'))


# initial fit, this will fix PCA transformer
p$fit(X_tr, Y_tr)

accuracy <- function(p)
{
  list(augmented = mean(Y_te==p$prediction(X_te)),
       original = mean(Y==p$prediction(X))
  )
}


# update the model incrementally
result <- as.data.frame(accuracy(p))
consec_perfect <- 0
for (i in 1:5000)
{
  newdata <- generate_augmented_data(500, X, Y)

  p$fit(newdata$X, newdata$Y)

  acc <- accuracy(p)
  result <- rbind(result, as.data.frame(acc))

  consec_perfect <- if (all(unlist(acc) >= 1-1e-6)) consec_perfect + 1 else 0
  cat(sprintf('iter %4d: augmented=%5.1f%%, original=%5.1f%%, consec=%d',
              i, acc$augmented*100, acc$original*100, consec_perfect), '\n')

  if (consec_perfect >= 10) {
    cat('DONE!\n')
    break
  }
}


tmp <- result %>% mutate(iter=0:(nrow(.)-1)) %>%
  melt(id.vars='iter', value.name='accuracy', variable.name='data')
ggplot(tmp, aes(iter, accuracy, color=data, linetype=data)) +
  geom_line(size=1)


# save the pretrained model
saveRDS(p, 'train/outcome/yaxis-predictor.rds')

# and config file, that records the input shape
list(input_shape=dim(X)[2:3]) %>% as.yaml() %>%
  write('train/outcome/yaxis-config.yml')


# saved model is a prediction model which takes
# 3d-array (N, nrow, ncol),
# where nrow and ncol is recorded in "yaxis-config.yml"

# later, the objects will be registered as interal data,
# so they can be used within the package

