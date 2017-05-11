library(kgschart)
library(abind)
library(grid)
library(gtable)
library(gridExtra)
library(ggplot2)
#library(OpenImageR)
library(magrittr)
library(tidyr)
library(reshape2)
library(dplyr)


source('train/0a-quick_pipeline.R')
source('train/0b-helper_functions.R')

set.seed(87)

datadir <- 'train/data/yaxis/'
X <- dget(file.path(datadir, 'X'))
Y <- dget(file.path(datadir, 'Y'))

# combine X into array (N, nrow, ncol)
X <- abind(X, along=0)


# generate train and test data
# train data is to create the PCA decomposition and the initial MLP model
# test data is used for performance check
tmp <- generate_augmented_data(10000, X, Y)
X_tr <- tmp$X
Y_tr <- tmp$Y

tmp <- generate_augmented_data(2000, X, Y)
X_te <- tmp$X
Y_te <- tmp$Y



random_plot(X, Y)
random_plot(X_tr, Y_tr)
random_plot(X_te, Y_te)






p <- Pipeline(fl=Flatten(),
              pc=PCA(30),
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
success <- FALSE
for (i in 1:1000)
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
    success <- TRUE
    break
  }
}

tmp <- result %>% mutate(iter=0:(nrow(.)-1)) %>%
  melt(id.vars='iter', value.name='accuracy', variable.name='data')
ggplot(tmp, aes(iter, accuracy, color=data, linetype=data)) +
  geom_line(size=1)


if (!success) {
  random_plot_inaccurate(p, X_te, Y_te)
} else {
  # save the pretrained model
  p$input_size <- dim(X)[2:3]
  saveRDS(p, 'train/outcome/yaxis-classifier.rds')
}


# saved model is a prediction model which takes
# 3d-array (N, nrow, ncol)
# later, the objects will be registered as interal data,
# so they can be used within the package

