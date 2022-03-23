#' Module for the team composition comparison page
#'
#' @export
#'
#' @import shiny
#' @importFrom tibble tibble
#' @importFrom dplyr pull filter arrange case_when
#'
teamComparisonUI <- function(id) {
  ns <- NS(id)

  tagList(
    titlePanel("Splatoon 2 Team Composition Analyzer"),
    fixedRow(
      column(
        width = 2,
        h5("Team 1"),
        uiOutput(ns("teamAlphaInput"))
      ),
      column(
        width = 4,
        gt_output(ns("teamAlphaTable"))
      ),
      column(
        width = 4,
        gt_output(ns("teamBetaTable"))
      ),
      column(
        width = 2,
        h5("Team 2"),
        uiOutput(ns("teamBetaInput"))
      )
    )
  )
}

teamComparisonServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      default_comp_alpha <- tibble(
        team_id = "Alpha",
        player_id = 1:4,
        weapon_id = c("nzap85", "sshooter_becchu", "hydra_custom", "bottlegeyser_foil")
      )

      default_comp_beta <- tibble(
        team_id = "Beta",
        player_id = 1:4,
        weapon_id = c("wakaba", "sharp_neo", "liter4k_scope", "campingshelter_camo")
      )

      weapon_types <- agg_df %>%
        filter(mode_name == "Splat Zones") %>%
        arrange(weapon) %>%
        pull(weapon)

      names(weapon_types) <- agg_df %>%
        filter(mode_name == "Splat Zones") %>%
        arrange(weapon) %>%
        pull(weapon_name)

      output$teamAlphaInput <- renderUI({
        generateTeamRows(default_comp_alpha, weapon_types, session)
      })
      output$teamBetaInput <- renderUI({
        generateTeamRows(default_comp_beta, weapon_types, session)
      })

      output$teamAlphaTable <- render_gt({
        generateTeamComparisonWeaponTable(
          "Splat Zones",
          lapply(paste0("Alpha", 1:4, "_weapon"), function(x){input[[unlist(x)]]})
        )
      })

      output$teamBetaTable <- render_gt({
        generateTeamComparisonWeaponTable(
          "Splat Zones",
          lapply(paste0("Beta", 1:4, "_weapon"), function(x){input[[unlist(x)]]})
        )
      })
    }
  )
}

