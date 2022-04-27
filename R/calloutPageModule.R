#' Module for the page containing callout images
#'
#' @export
#'
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter select pull
#'
calloutPageUI <- function(id) {
  ns <- NS(id)

  mode_names <- unique(callout_df$mode_full)
  map_names <- unique(callout_df$map_name)

  tagList(
    titlePanel("Callout Images"),
    fixedRow(
      column(
        width = 2,
        selectizeInput(
          inputId = ns("selectedMode"),
          label = "Game Mode",
          choices = mode_names,
          selected = "Tower Control"
        )
      )
    ),
    fixedRow(
      column(
        width = 6,
        selectizeInput(
          inputId = ns("mapA"),
          label = "Map",
          choices = map_names,
          selected = "MakoMart"
        ),
        uiOutput(ns("mapALink"))
      ),
      column(
        width = 6,
        selectizeInput(
          inputId = ns("mapB"),
          label = "Map",
          choices = map_names,
          selected = "Camp Triggerfish"
        ),
        uiOutput(ns("mapBLink"))
      ),
    )
  )
}

calloutPageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$mapALink <- renderUI({
        tags$iframe(
          imageOutput("imageMapA"),
          src = callout_df %>%
            filter(
              .data$map_name == input$mapA,
              .data$mode_full == input$selectedMode
            ) %>%
            mutate(embed_link = glue::glue('https://drive.google.com/file/d/{id_str}/preview')) %>%
            pull(embed_link) %>%
            .[[1]],
          width = 720,
          height = 540
        )
      })

      output$mapBLink <- renderUI({
        tags$iframe(
          imageOutput("imageMapB"),
          src = callout_df %>%
            filter(
              .data$map_name == input$mapB,
              .data$mode_full == input$selectedMode
            ) %>%
            mutate(embed_link = glue::glue('https://drive.google.com/file/d/{id_str}/preview')) %>%
            pull(embed_link) %>%
            .[[1]],
          width = 720,
          height = 540
        )
      })
    }
  )
}
