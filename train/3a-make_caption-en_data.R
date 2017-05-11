library(kgschart)
library(png)

datadir <- 'train/data/images/'
savedir <- 'train/data/caption-en/'
if (!dir.exists(savedir)) dir.create(savedir)
letters <- list()
for (d in file.path(datadir, c('batch3-en', 'batch4-en', 'batch5-en')))
{
  for (fn in dir(d, 'png$', recursive=TRUE, full.names=TRUE))
  {
    cat(fn, '\n')
    x <- readPNG(fn)
    x <- aperm(x, c(3,1,2))
    tblr <- kgschart:::detect_graph_area(x)
    t <- tblr[1]
    b <- tblr[2]
    l <- tblr[3]
    r <- tblr[4]
    if (b-t >= 0 & r-l >= 0) {
      caption <- x[,1:(t-1),(l+1):(r-1)]
      letters <- c(letters, kgschart:::extract_caption_letters(caption))
    }
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
