#' Download KGS rank graph
#'
#' Access KGS server and download the latest rank graph of the specified player.
#' Requires internet connection.
#' Returns the path to the downloaded file if succeeded.
#'
#' @param id KGS ID
#' @param dst destination file path or directory
#' @param lang language code, a lower case character of length 2
#' @param country country code, an upper case character of length 2
#' @param method method to be used for downloading files. See \code{\link[utils]{download.file}} for details
#' @param ... optional arguments for \code{\link[utils]{download.file}} other than
#' \code{method} and \code{mode}
#'
#' @details Default setting downloads the English version. Another good option is
#' to set \code{lang='ja'} and \code{country='JP'}, which downloads the Japanese version.
#' @seealso \code{\link[utils]{download.file}}
#' @return path to the saved file
#' @export
#' @examples
#' \dontrun{
#' f <- download_graph('twoeye')
#' p <- png::readPNG(f)
#' plot.new()
#' rasterImage(p,0,0,1,1)}
download_graph <- function(id, dst=tempfile(), lang='en', country='US',
                           method='libcurl', ...)
{
  url <- sprintf('http://www.gokgs.com/servlet/graph/%s-%s_%s.png',
                 id, lang, country)
  destfile <- if (dir.exists(dst)) file.path(dst, basename(url)) else dst
  res <- utils::download.file(url, destfile=destfile, mode='wb', method=method, ...)
  if (res != 0) return(NULL)
  destfile
}

