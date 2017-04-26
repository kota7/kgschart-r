
#' KGS Rank Graph Parser
#' @return NULL
#' @examples
#' x <- kgschart(system.file("extdata/leela-ja_JP.png",
#'                           package = "kgschart"))
#' @export
kgschart <- function(src, ...)
{
  x <- png::readPNG(src, ...)
  x <- aperm(x, c(3,1,2)) # convert to channel-first ordering
  #image_plot(x)
  tblr <- detect_graph_area(x)
  t <- tblr[1]
  b <- tblr[2]
  l <- tblr[3]
  r <- tblr[4]


  # split into parts
  if (b-t >= 0 & r-l >= 0) {
    graph <- x[,(t+1):(b-1),(l+1):(r-1)]
    yaxis   <- x[,,1:(l-1)]
    caption <- x[,1:(t-1),(l+1):(r-1)]
  } else {
    graph <- NULL
    yaxis <- NULL
    caption <- NULL
  }
  list(src=x, graph=graph, yaxis=yaxis, caption=caption)
}



