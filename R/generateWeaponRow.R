#' Function to generate shiny UI for one weapon for team comp analyzer
#'
#' @importFrom shiny selectizeInput
#'
generateWeaponRow <- function(weapon_types, session, item_team_id, item_player_id, item_weapon) {
  selectizeInput(
    inputId = session$ns(paste0(item_team_id, item_player_id, '_weapon')),
    label = '',
    choices = weapon_types,
    selected = item_weapon
  )
}
