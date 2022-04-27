#' UI for shiny app
#'
#' @export
#'
#' @importFrom shiny fluidPage tabsetPanel tabPanel
appUI <- function() {
  fluidPage(
    tabsetPanel(
      tabPanel(
        "Map Callouts",
        calloutPageUI("calloutPage")
      ),
      tabPanel(
        "Weapon Info",
        weaponTableUI("weaponTable")
      ),
      tabPanel(
        "Team Comp Analyzer",
        teamComparisonUI("teamComp")
      )
    )
  )
}
