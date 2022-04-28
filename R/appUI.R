#' UI for shiny app
#'
#' @export
#'
#' @importFrom shiny fluidPage tabsetPanel tabPanel
appUI <- function() {
  fluidPage(
    tabsetPanel(
      tabPanel(
        "Weapon Info",
        weaponTableUI("weaponTable")
      ),
      tabPanel(
        "Map Callouts",
        calloutPageUI("calloutPage")
      ),
      tabPanel(
        "Team Comp Analyzer",
        teamComparisonUI("teamComp")
      )
    )
  )
}
