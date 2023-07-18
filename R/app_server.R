#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  observeEvent(once = TRUE, eventExpr = session$clientData, {
    showModal(
      ui =   mod_splash_ui("splash")
    )
  })

  mod_splash_server("splash")


  base_table <-  mod_load_player_dataset_server("load_data")

  save_btn <- mod_title_bar_server("header")
  watchlist_outputs <- mod_main_table_server("watchlist", base_table)
  mod_player_statistics_server("statistics", sel = watchlist_outputs$selected_players)

  mod_save_watchlist_server("save_watchlist", save_btn = save_btn, df = watchlist_outputs$table)

}
