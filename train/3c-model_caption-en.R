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

datadir <- 'train/data/caption-en/'
X <- dget(file.path(datadir, 'X'))
Y <- dget(file.path(datadir, 'Y'))

# combine X into array (N, nrow, ncol)
X <- abind(X, along=0)


# We allow some confusions of letters with numbers
# we will handle when interpreting the results

# ignore parentheses
Y <- gsub('[\\(\\)]', '', Y)
# allowed confusions
fuzz <- c('I', '1',
          'l', '1',
          'O', '0',
          'b', '6',
          'g', '9')
i <- 1
while (i < length(fuzz))
{
  Y <- gsub(fuzz[i], fuzz[i+1], Y)
  i <- i + 2
}


# we only need to classify relevent letters and nunmbers
month_names <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
relevant_strs <- character(0)
for (m in month_names)
  for (n in 1:3) relevant_strs <- c(relevant_strs, ngram(m, n))
relevant_strs <- unique(relevant_strs)
relevant_strs <- c(0:9, ',', relevant_strs)
incl <- (Y %in% relevant_strs)

X <- X[incl,,]
Y <- Y[incl]


# crop X as possible
i1 <- +Inf
j1 <- +Inf
i2 <- -Inf
j2 <- -Inf
for (i in 1:dim(X)[1])
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
              pc=PCA(50),
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
success <- FALSE
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
  p$fuzzy <- matrix(fuzz, byrow=TRUE, ncol=2)
  saveRDS(p, 'train/outcome/caption-en-classifier.rds')
}


# saved model is a prediction model which takes
# 3d-array (N, nrow, ncol)
# later, the objects will be registered as interal data,
# so they can be used within the package

