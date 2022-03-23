#' App to compare teams
teamCompApp <- function() {
  ui <- fluidPage(teamComparisonUI("teamComp"))
  server <- function(input, output, session) {
    teamComparisonServer("teamComp")
  }
  shinyApp(ui, server)
}
