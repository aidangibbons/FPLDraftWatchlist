#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom tibble tibble
#' @importFrom sodium password_store
#' @noRd
app_server <- function(input, output, session) {{

  login_outputs <- mod_login_server("login")

  base_table <-  mod_load_player_dataset_server("load_data", con = login_outputs$con, credentials = login_outputs$credentials)


  watchlist_outputs <- mod_main_table_server("watchlist", login_outputs$credentials, df_raw = base_table$tbl, ord = base_table$ord)
  mod_player_statistics_server("statistics", login_outputs$credentials, sel = watchlist_outputs$selected_players,
                               df = base_table)

  mod_title_bar_server("header", con = login_outputs$con, credentials = login_outputs$credentials, df_watchlist = watchlist_outputs$order)

}}
