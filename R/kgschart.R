
#' KGS Rank Graph Parser
#'
#' Parse a KGS rank graph and recover undelining numeric data.
#' @param src png file name
#' @param keep_image if \code{TRUE}, keep image arrays, otherwise discarded
#' @param ... other arguments for \code{\link[png]{readPNG}}
#' @return \code{kgschart} class object
#' @examples
#' x <- kgschart(system.file("extdata/leela-ja_JP.png",
#'                           package = "kgschart"))
#' head(x$data)
#' \dontrun{
#' plot(x)}
#' @export
kgschart <- function(src, keep_image=FALSE, ...)
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
  out <- list(src=src)
  if (keep_image) out <- c(out, list(image=x, graph=graph,
                                     yaxis=yaxis, caption=caption))

  # if not graph, return with data=NULL
  if (is.null(graph)) {
    out$data <- NULL
    out <- structure(out, class='kgschart')
    return(out)
  }


  # collect pieces of information
  ## index of line height
  line_index <- get_line_index(graph)

  ## min/max rank
  ngrids <- get_num_grids(graph)
  label_positions <- round(seq(t, b, length.out=ngrids+2))
  rank_range <- get_rank_range(yaxis, label_positions)

  ## min/max time
  time_range <- get_time_range(caption)


  # compile data
  ## scale x (time)
  if (is.null(time_range)) {
    warning('failed to retrive horizontal axis scale')
    xx <- seq_along(line_index)
  } else {
    # (0.5, N+0.5) <-> time_range
    d <- diff(time_range) %>% as.numeric(units='days')
    xx <- time_range[1] +
      as.difftime((seq_along(line_index) - 0.5)*d/length(line_index),
                  units='days')
  }

  ## scale y (rating)
  if (is.null(rank_range)) {
    warning('failed to retrive vertical axis scale')
    yy <- -line_index
  } else {
    num_rank_range <- str_to_num_rank(rank_range)
    z2 <- 0.5
    z1 <- dim(graph)[2] + 0.5
    # (z1, z2) <-> num_rank_range

    slope <- diff(num_rank_range)/(z2-z1)
    const <- num_rank_range[1] - slope*z1
    yy <- line_index*slope + const
  }

  out$data <- data.frame(time=xx, rate=yy)
  out$time_range <- time_range
  out$rank_range <- rank_range
  structure(out, class = 'kgschart')
}


#' Print method for kgschart object
#'
#' Display basic information
#' @param x \code{kgschart} class object
#' @param ... not in use
#' @return Nothing
#' @export
print.kgschart <- function(x, ...)
{
  c('kgschart object',
    '',
    #sprintf('source    : %s', x$src),
    sprintf('rank range: %s', if (is.null(x$rank_range)) 'NA' else paste(x$rank_range, collapse=' ~ ')),
    sprintf('time range: %s', if (is.null(x$time_range)) 'NA' else paste(as.character(x$time_range), collapse=' ~ ')),
    sprintf('data size : %d', if (is.null(x$data)) NA else nrow(x$data))) %>%
    paste0(collapse='\n') %>%
    cat()
  invisible(NULL)
}


#' Plot method for kgschart object
#'
#' Plot data or image
#' @param x \code{kgschart} class object
#' @param y not in use
#' @param image if \code{TRUE}, draw the image, otherwise plot the data
#' @param separate if \code{TRUE}, and \code{image} is TRUE, draw the image separated by
#' component. Useful for debugging.
#' @param ... not in use
#' @return \code{gtable} object if \code{image} and \code{separate} are both \code{TRUE},
#' \code{ggplot} object otherwise
#' @export
plot.kgschart <- function(x, y=NULL, image=FALSE, separate=FALSE, ...)
{
  if (image) {
    if (separate) {
      if (is.null(x$graph)) {
        warning('x does not store images. try "kgschart" with "keep_image=TRUE"')
        return(NULL)
      }
      caption_grob <- if (is.null(x$caption)) grid::textGrob('NO CAPTION') else image_plot(x$caption)
      yaxis_grob <- if (is.null(x$yaxis)) grid::textGrob('NO YAXIS', rot=90) else image_plot(x$yaxis)
      graph_grob <- if (is.null(x$graph)) grid::textGrob('NO GRAPH') else image_plot(x$graph)
      out <- gridExtra::arrangeGrob(
        yaxis_grob, caption_grob, graph_grob,
        layout_matrix=matrix(c(1,1,2,3), nrow=2, ncol=2),
        widths=c(1,12), heights=c(1,18))
      graphics::plot(out)
      return(invisible(out))
    } else {
      if (is.null(x$image)) {
        warning('x does not store images. try "kgschart" with "keep_image=TRUE"')
        return(NULL)
      }
      out <- image_plot(x$image, show=TRUE)
      return(invisible(out))
    }
  } else {
    if (is.null(x$data)) {
      warning('x does not have data')
      return(NULL)
    } else {
      out <- ggplot2::ggplot(x$data, ggplot2::aes_string('time', 'rate')) +
        ggplot2::geom_line(size=1)
      graphics::plot(out)
      return(invisible(out))
    }
  }
}
