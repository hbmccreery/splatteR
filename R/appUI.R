#' UI for shiny app
#'
#' @importFrom shiny fluidPage tabsetPanel tabPanel
#'
appUI <- function() {
  fluidPage(
    tabsetPanel(
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

appServer <- function(input, output, session) {
  weaponTableServer("weaponTable")
  teamComparisonServer("teamComp")
}
