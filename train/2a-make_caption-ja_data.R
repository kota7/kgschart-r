library(kgschart)

datadir <- 'train/data/images/'
savedir <- 'train/data/caption-ja/'
if (!dir.exists(savedir)) dir.create(savedir)
letters <- list()
for (d in file.path(datadir, c('batch1-ja', 'batch2-ja')))
{
  for (fn in dir(d, 'png$', recursive=TRUE, full.names=TRUE))
  {
    cat(fn, '\n')
    x <- kgschart(fn, keep_image=TRUE)
    letters <- c(letters, kgschart:::extract_caption_letters(x$caption))
  }
}

letters <- unique(letters)

maxsize <- apply(sapply(letters, dim), MARGIN=1, max)
targetsize <- ceiling(maxsize * 1.2)

letters <- lapply(letters, kgschart:::pad_crop_image,
                  target_rows=targetsize[1], target_cols=targetsize[2], value=1)
grobs <- lapply(letters, kgschart:::image_plot)
plot(gridExtra::arrangeGrob(grobs=grobs))

dput(letters, file.path(savedir, 'X'))
