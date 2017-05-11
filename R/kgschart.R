
#' KGS Rank Graph Parser
#'
#' @param src png file name
#' @param keep_image if TRUE, keep image arrays, otherwise discarded
#' @param ... other arguments for \code{png::readPNG}
#' @return \code{kgschart} class object
#' @examples
#' x <- kgschart(system.file("extdata/leela-ja_JP.png",
#'                           package = "kgschart"))
#' @export
kgschart <- function(src, keep_image=TRUE, ...)
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


  # initialize output
  out <- if (keep_image) list(src=x, graph=graph, yaxis=yaxis, caption=caption) else list()

  # if not graph, return with data=NULL
  if (is.null(graph)) {
    out$data <- NULL
    out <- structure(out, class='kgschart')
    return(out)
  }


  # index of line height
  line_index <- get_line_index(graph)

  # min/max rank
  ngrids <- get_num_grids(graph)
  label_positions <- round(seq(t, b, length.out=ngrids+2))
  rank_range <- get_rank_range(yaxis, label_positions)

  # min/max time
  time_range <- get_time_range(caption)


  # scale x (time)
  if (is.null(time_range)) {
    warning('failed to retrive horizontal axis scale')
    x <- seq_along(line_index)
  } else {
    d <- diff(time_range) %>% as.numeric(units='days')
    x <- time_range[1] +
      as.difftime((seq_along(line_index) - 0.5)*d/length(line_index),
                  units='days')
  }

  # scale y (rating)
  if (is.null(rank_range)) {
    warning('failed to retrive vertical axis scale')
    y <- -line_index
  } else {
    num_rank_range <- str_to_num_rank(rank_range)
    z2 <- 1
    z1 <- tblr[1] - tblr[2]

    slope <- diff(num_rank_range)/(z2-z1)
    const <- num_rank_range[1] - slope*z1
    y <- line_index*slope + const
  }

  out$data <- data.frame(time=x, rate=y)
  out$time_range <- time_range
  out$rank_range <- rank_range
  structure(out, class = 'kgschart')
}




#' Plot Method for kgschart object
#' @param x \code{kgschart} class object
#' @param y not in use
#' @param image if TRUE, plot the image, otherwise plot the data
#' @param ... not in use
#' @return \code{gtable} object if \code{image} is TRUE,
#' \code{ggplot} object otherwise
#' @export
plot.kgschart <- function(x, y=NULL, image=FALSE, ...)
{
  if (image) {
    caption_grob <- if (is.null(x$caption)) grid::textGrob('NO CAPTION') else image_plot(x$caption)
    yaxis_grob <- if (is.null(x$yaxis)) grid::textGrob('NO YAXIS', rot=90) else image_plot(x$yaxis)
    graph_grob <- if (is.null(x$graph)) grid::textGrob('NO GRAPH') else image_plot(x$graph)
    out <- gridExtra::arrangeGrob(
      yaxis_grob, caption_grob, graph_grob,
      layout_matrix=matrix(c(1,1,2,3), nrow=2, ncol=2),
      widths=c(1,12), heights=c(1,18))
    plot(out)
    return(invisible(out))
  } else {
    if (is.null(x$data)) {
      warning('x does not have data')
      return(NULL)
    } else {
      out <- ggplot2::ggplot(x$data, ggplot2::aes(time, rate)) +
        ggplot2::geom_line(size=1)
      plot(out)
      return(invisible(out))
    }
  }
}
