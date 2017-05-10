library(kgschart)

datadir <- 'train/data/caption-en/'

X <- dget(file.path(datadir, 'X'))
Y <- rep('', length(X))
for (i in seq_along(X))
{
  kgschart:::image_plot(X[[i]], plot=TRUE)
  ans <- readline("What's this? > ")
  Y[i] <- ans
}

dput(Y, file.path(datadir, 'Y'))
