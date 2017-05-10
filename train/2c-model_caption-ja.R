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
library(dplyr)


source('train/0a-quick_pipeline.R')
source('train/0b-helper_functions.R')

set.seed(87)

datadir <- 'train/data/caption-ja//'
X <- dget(file.path(datadir, 'X'))
Y <- dget(file.path(datadir, 'Y'))

# combine X into array (N, nrow, ncol)
X <- abind(X, along=0)

# we only need to classify numbers, '/', '~', ':' and others, so remove
# other letters from the sample.
# in application, other letters are randomly assigned to one of the relevent letters
# this confusion would not matter since we can look for "yy/mm/dd hh:mm"
# expression to identify the period information

ind <- which(Y %in% c(0:9, '/', '~', ':'))
X <- X[ind,,]
Y <- Y[ind]

# crop X as possible
i1 <- +Inf
j1 <- +Inf
i2 <- -Inf
j2 <- -Inf
for (i in dim(X)[1])
{
  flg <- (X[i,,] < 1)
  row_range <- range(which(apply(flg, MARGIN=1, any)))
  col_range <- range(which(apply(flg, MARGIN=2, any)))
  i1 <- min(i1, row_range[1])
  j1 <- min(j1, col_range[1])
  i2 <- max(i2, row_range[2])
  j2 <- max(j2, col_range[2])
}
# allow 20% margins
target_rows <- ceiling((i2-i1+1)*1.2)
target_cols <- ceiling((j2-j1+1)*1.2)

X <- X[, i1:i2, j1:j2]
X <- lapply(1:dim(X)[1], function(i) {
  kgschart:::pad_crop_image(
    X[i,,],  target_rows=target_rows, target_cols=target_cols, value=1)
})
X <- abind::abind(X, along=0)
random_plot(X, Y)


# generate train and test data
# train data is to create the PCA decomposition and the initial MLP model
# test data is used for performance check
tmp <- generate_augmented_data(10000, X, Y)
X_tr <- tmp$X
Y_tr <- tmp$Y

tmp <- generate_augmented_data(2000, X, Y)
X_te <- tmp$X
Y_te <- tmp$Y


random_plot(X_tr, Y_tr)
random_plot(X_te, Y_te)






p <- Pipeline(fl=Flatten(),
              #pc=PCA(50),
              ml=MLP(hidden=c(50,50), output='softmax'))


# initial fit, this will fix PCA transformer
p$fit(X_tr, Y_tr)

accuracy <- function(p)
{
  list(augmented = mean(Y_te==p$prediction(X_te)),
       original = mean(Y==p$prediction(X))
  )
}
accuracy(p)

# update the model incrementally
result <- as.data.frame(accuracy(p))
consec_perfect <- 0
for (i in 1:5000)
{
  newdata <- generate_augmented_data(1000, X, Y)

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
p$input_size <- dim(X)[2:3]
saveRDS(p, 'train/outcome/caption-ja-classifier.rds')



# saved model is a prediction model which takes
# 3d-array (N, nrow, ncol)
# later, the objects will be registered as interal data,
# so they can be used within the package

