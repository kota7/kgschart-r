library(kgschart)

datadir <- 'train/data/images/'
savedir <- 'train/data/yaxis/'
letters <- list()
for (fn in dir(datadir, 'png$', recursive=TRUE, full.names=TRUE))
{
  cat(fn, '\n')
  x <- kgschart(fn, keep_image=TRUE)
  letters <- c(letters, kgschart:::extract_axis_letters(x$yaxis))
}

letters <- unique(letters)

maxsize <- apply(sapply(letters, dim), MARGIN=1, max)
targetsize <- ceiling(maxsize * 1.2)

letters <- lapply(letters, kgschart:::pad_crop_image,
                  target_rows=targetsize[1], target_cols=targetsize[2], value=1)
grobs <- lapply(letters, kgschart:::image_plot)
plot(gridExtra::arrangeGrob(grobs=grobs))

dput(letters, file.path(savedir, 'X'))
