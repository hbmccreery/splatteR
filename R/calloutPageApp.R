#' App to compare teams
calloutPageApp <- function() {
  ui <- fluidPage(calloutPageUI("calloutPageApp"))
  server <- function(input, output, session) {
    calloutPageServer("calloutPageApp")
  }
  shinyApp(ui, server)
}
