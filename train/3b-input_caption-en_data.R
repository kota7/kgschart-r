library(kgschart)

datadir <- 'train/data/caption-en/'

X <- dget(file.path(datadir, 'X'))
Y <- rep('', length(X))
for (i in seq_along(X))
{
  kgschart:::image_plot(X[[i]], show=TRUE)
  ans <- readline(sprintf("%d/%d What's this? > ", i, length(X)))
  if (ans == 'exit') stop()
  Y[i] <- ans
}

dput(Y, file.path(datadir, 'Y'))
