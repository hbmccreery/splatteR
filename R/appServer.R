#' Server for shiny app
#'
#' @export
appServer <- function(input, output, session) {
  weaponTableServer("weaponTable")
  teamComparisonServer("teamComp")
  calloutPageServer("calloutPage")
}
