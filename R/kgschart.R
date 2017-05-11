
#' KGS Rank Graph Parser
#'
#' @param src png file name
#' @param ... other arguments for \code{png::readPNG}
#' @return \code{kgschart} class object
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



  #line_index <- get_line_index(graph)
  #plot(-line_index, type='l')

  #ngrids <- get_num_grids(graph)
  #print(ngrids)
  #label_positions <- round(seq(t, b, length.out=ngrids+2))
  #print(label_positions)
  #rank_range <- get_rank_range(yaxis, label_positions)
  #print(rank_range)

  # letters <- extract_axis_letters(yaxis)
  # print(length(letters))
  # grobs <- lapply(letters, image_plot)
  # plot(gridExtra::arrangeGrob(grobs=grobs))

  # letters <- extract_caption_letters(caption)
  # print(length(letters))
  # grobs <- lapply(letters, image_plot)
  # plot(gridExtra::arrangeGrob(grobs=grobs))


  time_range <- get_time_range(caption)
  print(time_range)

  structure(list(src=x, graph=graph, yaxis=yaxis, caption=caption),
            class = 'kgschart')
}




#' Plot Method for kgschart object
#' @param x object
#' @param y not in use
#' @param ... not in use
#' @return \code{gtable} object
#' @export
plot.kgschart <- function(x, y=NULL, ...)
{
  caption_grob <- if (is.null(x$caption)) grid::textGrob('NO CAPTION') else image_plot(x$caption)
  yaxis_grob <- if (is.null(x$yaxis)) grid::textGrob('NO YAXIS', rot=90) else image_plot(x$yaxis)
  graph_grob <- if (is.null(x$graph)) grid::textGrob('NO GRAPH') else image_plot(x$graph)
  out <- gridExtra::arrangeGrob(
    yaxis_grob, caption_grob, graph_grob,
    layout_matrix=matrix(c(1,1,2,3), nrow=2, ncol=2),
    widths=c(1,12), heights=c(1,18))
  plot(out)
  invisible(out)
}
