
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

  structure(list(src=x, graph=graph, yaxis=yaxis, caption=caption),
            class = 'kgschart')
}



#' @export
plot.kgschart <- function(x, y=NULL, ...)
{
  caption_grob <- image_plot(x$caption)
  yaxis_grob <- image_plot(x$yaxis)
  graph_grob <- image_plot(x$graph)
  out <- gridExtra::arrangeGrob(
    yaxis_grob, caption_grob, graph_grob,
    layout_matrix=matrix(c(1,1,2,3), nrow=2, ncol=2),
    widths=c(1,12), heights=c(1,18))
  plot(out)
  invisible(out)
}
