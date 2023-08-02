#' player_statistics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_player_statistics_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("uiPlayerStatistics"))
  )

}

#' player_statistics Server Functions
#'
#' @noRd
#' @importFrom dplyr mutate select across filter slice rename_with
#' @importFrom tibble tibble
#' @importFrom tidyselect everything ends_with
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr map_chr map2
#' @importFrom DT datatable
#' @importFrom stringr str_remove
#' @import gt
#' @import rlang
#' @importFrom gtExtras img_header
#' @importFrom shinyWidgets materialSwitch
mod_player_statistics_server <- function(id, credentials, sel_list, df_list, table_option){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$uiPlayerStatistics <- renderUI({
      req(credentials()$user_auth)

      tagList(
        fluidRow(
          column(width = 3,
                 tags$h3("Data format"),
                 tags$div(
                   materialSwitch(inputId = ns("swP90"), label = "Per 90", inline = TRUE),
                   tags$span("Total")
                 )
          ),
          column(width = 3,
                 shinyjs::hidden(
                   shiny::div(
                     id = ns("ui_too_many_players"),
                     shiny::tags$p(
                       "Too many players selected",
                       style = "text-align:left; color: red; font-weight: bold; padding-top: 5px;"
                     ),
                     shiny::tags$p(
                       "Removing second-last pick.",
                       style = "text-align:left; color: red; font-weight: bold; padding-top: 5px;"
                     )
                   )
                 )),
        ),
        fluidRow(
          gt_output(ns("tblSelectedPlayers"))
        )
      )
    })

    output$tblSelectedPlayers <- render_gt({
      req(credentials()$user_auth)

      tab <- table_option()

      sel <- sel_list[[tab]]
      df <- df_list[[tab]]

      print_debug("player statistics render gt entered")
      sel_id <- sel()

      if (!(length(sel_id) > 0)) {
        return(tibble("Select player(s)" = "To display player statistics") %>% gt)
      }

      p90_cols <- !input$swP90

      # n_max = 4



      # if (length(sel_id) > n_max) {
      #   shinyjs::toggle(id = "ui_too_many_players", anim = TRUE, time = 0.2,
      #                   animType = "fade")
      #   shinyjs::delay(
      #     2500,
      #     shinyjs::toggle(id = "ui_too_many_players", anim = TRUE, time = 1,
      #                     animType = "fade"))
      # }

      filtered_df <- df %>%
        # slice(df$ord()) %>%
        mutate(img = url_player_image(code)) %>%
        select(web_name, img,
               minutes, "points" = total_points, "PPG" = points_per_game,
               "goals" = goals_scored, assists, bonus,
               "xG" = expected_goals, "xA" = expected_assists, "xGI" = expected_goal_involvements) %>%
        mutate(across(c(everything(), -web_name, -img), as.numeric)) %>%
        mutate(across(c(points, goals, assists, bonus, xG, xA, xGI), ~signif(. * 90 / minutes, 2), .names = "{col}_p90")) %>%
        mutate(across(everything(), as.character)) %>%
        slice(sel_id)

      imgs <- filtered_df$img

      if (p90_cols) {
        filtered_df <- filtered_df %>%
          select(web_name, minutes, ends_with("p90")) %>%
          rename_with(~str_remove(., "_p90"), ends_with("p90"))
      } else {
        filtered_df <- filtered_df %>%
          select(web_name, minutes, everything(), -img, -ends_with("p90"))
      }

      wide_df <- filtered_df %>%
        pivot_longer(c(everything(), -web_name), names_to = "stat") %>%
        pivot_wider(names_from = web_name, values_from = value) %>%
        mutate(across(c(everything(), -stat), as.numeric))

      pal <- if (length(sel_id) == 1) {
        "Greys"
      } else if ((length(sel_id) == 2)) {
        c("#FDAE61", "#A6D96A")
      } else {
        "RdYlGn"
      }

      wide_df %>%
        gt(rowname_col = "stat") %>%
        cols_label(
          .list =
            map2(names(wide_df)[-1], imgs, function(nm, im) {
              img_header(
                nm,
                im,
                height = 150,
                font_size = 20
              )
            }) %>% set_names(names(wide_df)[-1])

        ) %>%
        cols_align(
          align = "center",
          columns = c(everything())
        ) %>%
        data_color(
          rows = everything(),
          # contains("goals"),
          direction = "row",
          palette = pal
        ) %>%
        tab_options(table.align='left')
    })
  })
}
