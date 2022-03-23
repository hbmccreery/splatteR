#' Table to display weapon values
#'
#' @import gt
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select relocate contains vars
#'
generateWeaponValueTable <- function(df, mode_type) {
  IMG_URL_START <- "http://www.stat.ink/assets/20220112-159/gxzctyqhz7zbajyi/"
  IMG_URL_END <- ".png?v=1642007108"

  language_dict <- jsonlite::fromJSON("https://raw.githubusercontent.com/Leanny/leanny.github.io/master/data/Languages/lang_dict_USen.json")

  spg_lim <- pmax(abs(pmin(df$spg)), pmax(df$spg))
  kpg_lim <- pmax(abs(pmin(df$kpg)), pmax(df$kpg))
  apg_lim <- pmax(abs(pmin(df$apg)), pmax(df$apg))
  dpg_lim <- pmax(abs(pmin(df$dpg)), pmax(df$dpg))
  scale_lim <- max(spg_lim, kpg_lim, apg_lim, dpg_lim)

  iapg_lim <- pmax(abs(pmin(df$iapg)), pmax(df$iapg))

  df %>%
    mutate(
      par0_plot = map2(ParamValue0, Param0, ~bar_chart(.x, .y, language_dict, color="#7fc97f")),
      par1_plot = map2(ParamValue1, Param1, ~bar_chart(.x, .y, language_dict, color="#beaed4")),
      par2_plot = map2(ParamValue2, Param2, ~bar_chart(.x, .y, language_dict, color="#fdc086")),
    ) %>%
    relocate(
      contains("pg"),
      .after = games
    ) %>%
    relocate(
      contains("_plot"),
      .after = games
    ) %>%
    select(-c(min_played, contains("Param"))) %>%
    gt(rowname_col = "weapon_name") %>%
    cols_align(
      align = "left",
      columns = vars(par0_plot, par1_plot, par2_plot)
    ) %>%
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
      columns = vars(kpg, apg, spg, dpg),
      decimals = 1
    ) %>%
    fmt_number(
      columns = vars(iapg),
      decimals = 0
    ) %>%
    # fmt_number(columns = vars(min_played), decimals = 0) %>%
    fmt_percent(
      columns = vars(win_pct),
      decimals = 1
    ) %>%
    data_color(
      columns = vars(spg, kpg, apg),
      colors = scales::col_numeric(
        palette = c('red', 'white', 'blue'),
        domain = c(-1 * scale_lim, scale_lim)
      )
    ) %>%
    data_color(
      columns = vars(dpg),
      colors = scales::col_numeric(
        palette = c('blue', 'white', 'red'),
        domain = c(-1 * scale_lim, scale_lim)
      )
    ) %>%
    data_color(
      columns = vars(iapg),
      colors = scales::col_numeric(
        palette = c('red', 'white', 'blue'),
        domain = c(-1 * iapg_lim, iapg_lim)
      )
    ) %>%
    data_color(
      columns = vars(win_pct),
      # colors = paletteer::paletteer_c("viridis::magma", 100) %>% as.character()
      colors = scales::col_numeric(
        palette = c('red', 'white', 'blue'),
        domain = c(0.45, 0.55)
      )
    ) %>%
    data_color(
      columns = vars(weapon_Range),
      colors = paletteer::paletteer_c("viridis::viridis", 100) %>% as.character()
    ) %>%
    cols_label(
      weapon_name = "",
      weapon_Range = "Range",
      weapon_special = "",
      weapon_subweapon = "",
      weapon_SpecialCost = "Special",
      weapon = "",
      games = "Games",
      # min_played = "Time Played\n(min)",
      kpg = "Splats",
      apg = "Assists",
      spg = "Specials",
      dpg = "Deaths",
      iapg = "Ink",
      win_pct = "Win%",
      par0_plot = '',
      par1_plot = '',
      par2_plot = '',
    ) %>%
    cols_align(
      columns = vars(kpg, apg, iapg, spg, dpg, win_pct),
      align = 'center'
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "right",
          color = "gray",
          weight = px(2)
        )
      ),
      locations = list(
        cells_body(
          columns = vars(weapon_SpecialCost, weapon_Range, games, iapg, par2_plot)
        )
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(style = "italic")
      ),
      locations = list(
        cells_body(
          columns = vars(weapon_SpecialCost)
        )
      )
    ) %>%
    tab_spanner(
      label = "Stat Over Average Per Game",
      columns = vars(kpg, apg, spg, dpg, iapg)) %>%
    tab_spanner(
      label = "Weapon Kit",
      columns = contains("weapon")
    ) %>%
    tab_spanner(
      label = "In-Game",
      columns = contains("par")
    ) %>%
    tab_header(title = glue("Weapon Usage in {mode_type} X Rank")) %>%
    tab_options(table.width = pct(100))
}

bar_chart <- function(value, bar_text, language_dict, color = "red"){
  glue::glue("
    <table>
      <tr><td stype=\"font-size: 0.8em\">{language_dict[bar_text]}</td></tr>
      <tr><td><span
        style=\"display: inline-block; direction: ltr; border-radius: 4px; padding-right: 2px; font-size: 0.9em; background-color: {color}; width: {30 + (value * 0.7)}%\"
      > &nbsp;{value}
      </span></td></tr>
    </table>") %>%
    as.character() %>%
    gt::html()
}
