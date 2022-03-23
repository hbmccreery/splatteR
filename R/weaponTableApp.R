#' App for weapon table
weaponTableApp <- function() {
  ui <- fluidPage(weaponTableUI("weaponTable"))
  server <- function(input, output, session) {
    weaponTableServer("weaponTable")
  }
  shinyApp(ui, server)
}
