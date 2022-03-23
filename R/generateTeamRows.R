#' Create all select inputs for comp analyzer
#'
#' @importFrom purrr pmap
generateTeamRows <- function(team_data, weapon_types, session) {
  pmap(team_data, ~generateWeaponRow(weapon_types, session, ..1, ..2, ..3))
}
