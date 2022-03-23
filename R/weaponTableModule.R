#' App for the weapon tables
#'
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr select ungroup group_by summarise mutate left_join case_when
#' @importFrom tidyr pivot_longer pivot_wider drop_na
#' @importFrom tibble tibble
#' @importFrom purrr pmap map map2
#' @importFrom gt gt_output render_gt
#'
weaponTableUI <- function(id) {
  ns <- NS(id)

  sort_type <- c("Games", "Range", "Wins", "Splats", "Assists", "Ink", "Specials", "Deaths")
  sort_display_map <- list(
    "games" = "Games",
    "weapon_Range" = "Range",
    "win_pct" = "Wins",
    "kpg" = "Splats",
    "apg" = "Assists",
    "iapg" = "Ink",
    "spg" = "Specials",
    "dpg" = "Deaths"
  )

  game_types <- unique(agg_df$mode_name)
  weapon_types <- unique(agg_df$weapon_cat)
  weapon_subs <- unique(agg_df$weapon_sub_name)
  weapon_specials <- unique(agg_df$weapon_special_name)
  weapon_count <- length(unique(agg_df$weapon))

  tagList(
    titlePanel("Splatoon 2 Weapons"),
    p("Data collected from stat.ink and leanny.github.io. Thanks to them for the valuable resource!"),

    # Sidebar with a slider input for number of bins
    fluidRow(
      column(
        2,
        selectInput(
          ns("modeName"),
          "Mode Select",
          game_types
        ),
        selectInput(
          ns("tableSort"),
          "Sort By",
          sort_type
        ),
        sliderInput(
          ns("nDisplay"),
          "# Weapons",
          min = 1,
          max = weapon_count,
          value = 10,
          sep = 1
        )
      ),
      column(
        3,
        selectizeInput(
          inputId = ns("weaponCat"),
          label = "Weapon Category",
          choices = weapon_types,
          selected = weapon_types,
          multiple = T
        )
      ),
      column(
        3,
        selectizeInput(
          inputId = ns("weaponSub"),
          label = "Weapon Subweapon",
          choices = weapon_subs,
          selected = weapon_subs,
          multiple = T
        )
      ),
      column(
        4,
        selectizeInput(
          inputId = ns("weaponSpecial"),
          label = "Weapon Special",
          choices = weapon_specials,
          selected = weapon_specials,
          multiple = T
        )
      ),
    ),

    hr(),

    h3("Weapon Stat Value Filters"),

    fluidRow(uiOutput(ns("sliderRow1"))),
    fluidRow(uiOutput(ns("sliderRow2"))),

    hr(),

    gt_output(outputId = ns("table"))
  )
}

weaponTableServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ## TODO: probably a more clever way to do this than dumping it in server
      sort_display_map <- list(
        "games" = "Games",
        "weapon_Range" = "Range",
        "win_pct" = "Win %",
        "kpg" = "Splats",
        "apg" = "Assists",
        "iapg" = "Ink",
        "spg" = "Specials",
        "dpg" = "Deaths"
      )

      val_bounds <- agg_df %>%
        select(.data$games, .data$weapon_Range, .data$win_pct, contains("pg")) %>%
        pivot_longer(.data$games:.data$iapg) %>%
        ungroup() %>%
        group_by(.data$name) %>%
        summarise(
          min_val = floor(min(.data$value) * 10) / 10,
          max_val = ceiling(max(.data$value) * 10) / 10
        ) %>%
        mutate(
          name_display = sort_display_map[.data$name]
        )

      output$sliderRow1 <- renderUI({
        val_bounds %>%
          filter(!grepl("pg", name)) %>%
          select(name, name_display, min_val, max_val) %>%
          pmap(
            ~ generateWeaponValueRowSlider(..1, ..2, ..3, ..4, session)
          )
      })

      output$sliderRow2 <- renderUI({
        val_bounds %>%
          filter(grepl("pg", name)) %>%
          select(name, name_display, min_val, max_val) %>%
          pmap(
            ~ generateWeaponValueRowSlider(..1, ..2, ..3, ..4, session)
          )
      })

      output$table <- render_gt({
        sort_type <- input$tableSort

        input_bound_df <- lapply(
          val_bounds$name,
          function(y) {
            tibble(min_stat_val = input[[y]][1], max_stat_val = input[[y]][2])
          }
        ) %>%
          do.call(rbind, .)

        if (dim(input_bound_df)[1] == 0) {
          agg_df_filtered <- agg_df %>%
            filter(
              mode_name == input$modeName,
              weapon_cat %in% input$weaponCat,
              weapon_sub_name %in% input$weaponSub,
              weapon_special_name %in% input$weaponSpecial
            )
        } else {
          input_bound_df$name <- val_bounds$name
          agg_df_filtered <- agg_df %>%
            filter(
              mode_name == input$modeName,
              weapon_cat %in% input$weaponCat,
              weapon_sub_name %in% input$weaponSub,
              weapon_special_name %in% input$weaponSpecial
            ) %>%
            pivot_longer(val_bounds$name) %>%
            left_join(input_bound_df) %>%
            filter(value >= min_stat_val, value <= max_stat_val) %>%
            select(-contains("stat_val")) %>%
            pivot_wider(names_from = name, values_from = value) %>%
            drop_na()
        }

        agg_df_filtered %>%
          ungroup() %>%
          mutate(
            sort_feat = case_when(
              sort_type == "Games" ~ as.double(.data$games),
              sort_type == "Range" ~ as.double(.data$weapon_Range),
              sort_type == "Wins" ~ .data$win_pct,
              sort_type == "Splats" ~ .data$kpg,
              sort_type == "Assists" ~ .data$apg,
              sort_type == "Ink" ~ .data$iapg,
              sort_type == "Specials" ~ .data$spg,
              sort_type == "Deaths" ~ -.data$dpg,
              T ~ as.double(.data$games)
            )
          ) %>%
          arrange(-.data$sort_feat) %>%
          head(input$nDisplay) %>%
          group_by(.data$weapon_cat) %>%
          select(
            -c(
              .data$mode_name,
              .data$weapon_sub_name,
              .data$weapon_special_name,
              .data$sort_feat
            )
          ) %>%
          generateWeaponValueTable(input$modeName)
      })
    }
  )
}
