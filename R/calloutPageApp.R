#' App to compare teams
calloutPageApp <- function() {
  ui <- fluidPage(calloutPageUI("calloutPage"))
  server <- function(input, output, session) {
    calloutPageServer("calloutPage")
  }
  shinyApp(ui, server)
}
