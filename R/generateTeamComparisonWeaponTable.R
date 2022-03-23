#' Generate Team Comparison Table
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom gt gt text_transform cells_body vars fmt_number data_color cols_label cols_align summary_rows web_image
#' @importFrom dplyr ungroup filter mutate arrange select
#' @importFrom paletteer paletteer_c
#'
generateTeamComparisonWeaponTable <- function(
  selected_mode_name,
  selected_weapons
) {
  IMG_URL_START <- "http://www.stat.ink/assets/20220112-159/gxzctyqhz7zbajyi/"
  IMG_URL_END <- ".png?v=1642007108"

  ## TODO: MOVE THIS TO SOME KIND OF CONSTANTS SET-UP RATHER THAN THIS
  ## TODO: MAYBE PRE-COMPUTE IT AND STORE IT W/ THE REST OF OUR DATA?
  spg_lim <- pmax(abs(pmin(agg_df$spg)), pmax(agg_df$spg))
  kpg_lim <- pmax(abs(pmin(agg_df$kpg)), pmax(agg_df$kpg))
  apg_lim <- pmax(abs(pmin(agg_df$apg)), pmax(agg_df$apg))
  dpg_lim <- pmax(abs(pmin(agg_df$dpg)), pmax(agg_df$dpg))
  scale_lim <- max(spg_lim, kpg_lim, apg_lim, dpg_lim)

  ink_domain <- c(min(agg_df$iapg), max(agg_df$iapg))

  range_min <- min(agg_df$weapon_Range)
  range_max <- max(agg_df$weapon_Range)
  range_domain <- c(min(agg_df$weapon_Range), max(agg_df$weapon_Range))
  range_pal <- paletteer::paletteer_c("viridis::viridis", 100) %>% as.character()

  if(length(selected_weapons) <= 0) {
    tibble(err = 'If you see this message, go tell Hugh how you got here.') %>%
      gt()
  }

  agg_df %>%
    ungroup() %>%
    filter(
      .data$weapon %in% selected_weapons,
      .data$mode_name == selected_mode_name
    ) %>%
    mutate(weapon_fct = factor(.data$weapon, levels = unique(selected_weapons))) %>%
    arrange(.data$weapon_fct) %>%
    select(-.data$weapon_fct) %>%
    select(
      .data$weapon,
      .data$weapon_subweapon,
      .data$weapon_special,
      .data$weapon_Range,
      .data$kpg,
      .data$apg,
      .data$iapg,
      .data$dpg
    ) %>%
    gt() %>%
    text_transform(
      locations = cells_body(columns = vars(weapon)),
      fn = function(x) {
        web_image(
          url = paste0(IMG_URL_START, x, IMG_URL_END),
          height = 45
        )
      }
    ) %>%
    text_transform(
      locations = cells_body(columns = vars(weapon_special)),
      fn = function(x) {
        web_image(
          url = paste0(IMG_URL_START, 'sp/', x, IMG_URL_END),
          height = 30
        )
      }
    ) %>%
    text_transform(
      locations = cells_body(columns = vars(weapon_subweapon)),
      fn = function(x) {
        web_image(
          url = paste0(IMG_URL_START, 'sub/', x, IMG_URL_END),
          height = 30
        )
      }
    ) %>%
    fmt_number(
      columns = vars(kpg, apg, dpg),
      decimals = 1
    ) %>%
    fmt_number(
      columns = vars(iapg),
      decimals = 0
    ) %>%
    data_color(
      columns = vars(kpg, apg),
      colors = scales::col_numeric(
        palette = c('red', 'white', 'blue'),
        domain = c(-1 * scale_lim, scale_lim)
      )
    ) %>%
    data_color(
      columns = vars(iapg),
      colors = scales::col_numeric(
        palette = c('red', 'white', 'blue'),
        domain = ink_domain
      )
    ) %>%
    data_color(
      columns = vars(dpg),
      colors = scales::col_numeric(
        palette = c('blue', 'white', 'red'),
        domain = c(-1 * scale_lim, scale_lim)
      )
    ) %>%
    cols_label(
      weapon = "",
      weapon_subweapon = "",
      weapon_special = "",
      weapon_Range = 'Range',
      kpg = 'Splat',
      apg = 'Assist',
      iapg = 'Ink',
      dpg = 'Death'
    ) %>%
    cols_align(
      columns = vars(kpg, apg, iapg, dpg),
      align = 'center'
    ) %>%
    data_color(
      columns = vars(weapon_Range),
      colors = scales::col_numeric(
        palette = range_pal,
        domain = range_domain
      )
    ) %>%
    summary_rows(
      columns = vars(kpg, apg, dpg, iapg, weapon_Range),
      fns = list(Tot = ~mean(.)),
      formatter = fmt_number,
      decimals = 1
    )
}
