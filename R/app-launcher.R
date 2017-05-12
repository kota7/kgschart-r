#' Launch shiny app
#'
#' Start "KGS Rank Graph Parser" applicaiton.
#' @param ... optional arguments for \code{\link[shiny]{runApp}}
#' @examples
#' \dontrun{
#' kgschart_app()}
#' @export
kgschart_app <- function(...)
{
  shiny::runApp(system.file('shiny/kgschart-app/', package='kgschart'), ...)
}
