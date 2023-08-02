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

  output$uiMainTableTabs <- renderUI({
    req(login_outputs$credentials()$user_auth)
    tagList(
      div(style = "margin-top: -10px;", p("Note - Overall and individual position watchlists are not linked.")),
      div(style = "margin-top: -10px;", p("Updating one won't update the other.")),
      tabsetPanel(
        id = "tbstMainTable",
        tabPanel("All players", value = "all", mod_main_table_ui("watchlist")),
        tabPanel("GKP", value = "gkp", mod_main_table_ui("wl_gkp")),
        tabPanel("DEF", value = "def", mod_main_table_ui("wl_def")),
        tabPanel("MID", value = "mid", mod_main_table_ui("wl_mid")),
        tabPanel("FWD", value = "fwd", mod_main_table_ui("wl_fwd"))
      )
    )
  })

  base_table <- mod_load_player_dataset_server("load_data", con = login_outputs$con, credentials = login_outputs$credentials, pos_filt = "all", saved_wl = login_outputs$saved_watchlist)
  base_table_gkp <- mod_load_player_dataset_server("load_data", con = login_outputs$con, credentials = login_outputs$credentials, pos_filt = "gkp", saved_wl = login_outputs$saved_watchlist)
  base_table_def <- mod_load_player_dataset_server("load_data", con = login_outputs$con, credentials = login_outputs$credentials, pos_filt = "def", saved_wl = login_outputs$saved_watchlist)
  base_table_mid <- mod_load_player_dataset_server("load_data", con = login_outputs$con, credentials = login_outputs$credentials, pos_filt = "mid", saved_wl = login_outputs$saved_watchlist)
  base_table_fwd <- mod_load_player_dataset_server("load_data", con = login_outputs$con, credentials = login_outputs$credentials, pos_filt = "fwd", saved_wl = login_outputs$saved_watchlist)

  watchlist_outputs <- mod_main_table_server("watchlist", login_outputs$credentials, df_raw = base_table$tbl, ord = base_table$ord)
  wl_gkp_outputs <-mod_main_table_server("wl_gkp", login_outputs$credentials, df_raw = base_table_gkp$tbl, ord = base_table_gkp$ord)
  wl_def_outputs <-mod_main_table_server("wl_def", login_outputs$credentials, df_raw = base_table_def$tbl, ord = base_table_def$ord)
  wl_mid_outputs <-mod_main_table_server("wl_mid", login_outputs$credentials, df_raw = base_table_mid$tbl, ord = base_table_mid$ord)
  wl_fwd_outputs <-mod_main_table_server("wl_fwd", login_outputs$credentials, df_raw = base_table_fwd$tbl, ord = base_table_fwd$ord)


  base_tables_list <- list(
    "all" = base_table$tbl,
    "gkp" = base_table_gkp$tbl,
    "def" = base_table_def$tbl,
    "mid" = base_table_mid$tbl,
    "fwd" = base_table_fwd$tbl
  )

  selected_players_list <- list(
    "all" = watchlist_outputs$selected_players,
    "gkp" = wl_gkp_outputs$selected_players,
    "def" = wl_def_outputs$selected_players,
    "mid" = wl_mid_outputs$selected_players,
    "fwd" = wl_fwd_outputs$selected_players
  )

  orders_list <- list(
    "all" = watchlist_outputs$order,
    "gkp" = wl_gkp_outputs$order,
    "def" = wl_def_outputs$order,
    "mid" = wl_mid_outputs$order,
    "fwd" = wl_fwd_outputs$order
  )

  selected_tab <- reactive({
    input$tbstMainTable
  })

  mod_player_statistics_server("statistics", login_outputs$credentials, sel_list = selected_players_list,
                               df_list = base_tables_list, table_option = selected_tab)

  title_bar_outputs <- mod_title_bar_server("header", con = login_outputs$con, credentials = login_outputs$credentials, df_watchlist_list = orders_list)

  # TODO this isn't working
  # session$onSessionEnded(function() {
  #   save_on_close(con = login_outputs$con, credentials = login_outputs$credentials,
  #                 df_watchlist = watchlist_outputs$order, SaveOnClose = title_bar_outputs$SaveOnClose)
  # })
}}

save_on_close <- function (con, credentials, df_watchlist, SaveOnClose) {
  print("Close initiated")
  browser()
  creds <- isolate(credentials())
  save_chk <- isolate(SaveOnClose())
  wl <- isolate(df_watchlist())

  if (save_chk & creds$user_auth) {
    print("Trying to save watchlist on close")

    print(glue::glue("Watchlist {creds$info$id} ({creds$info$username}) save requested... "))

    watchlist_id = creds$info$id

    tryCatch({

      res <- save_watchlist_close(con, watchlist_id, wl)

      if (res == 1) {
        shinyalert(
          title = glue("Watchlist {creds$info$username} saved"),
          text = "", type = "success", immediate = T,
          timer = 2000
        )

        print(glue("Watchlist {watchlist_id} saved successfully!"))
      }
    }, error = function (cond) {

      shinyalert(
        title = glue("Watchlist {creds$info$username} save failed.
                       Try again or contact app owner."),
        text = "", type = "error", immediate = T,
        timer = 3000
      )
      print(glue("Watchlist {watchlist_id} save failed. \n--- Error condition: {cond}"))
    })


  } else {
    print("Close completed without save.")
  }
}


save_watchlist_close <- function (con, user_id, wl) {

  values_out <- wl %>%
    mutate(user_id = user_id,
           player = id,
           ranking = 1:n()) %>%
    filter(row_number() != draft_rank) %>%
    select(user_id, player, ranking) %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate(out_text = glue::glue("({user_id}, {player}, {ranking})")) %>%
    pull(out_text)

  if (length(values_out) == 0) {
    shinyalert(
      title = glue("Watchlist: {credentials()$info$username} save failed
                       due to watchlist matching draft rank."),
      text = "", type = "error", immediate = T,
      timer = 4000
    )

    print(glue::glue("Watchlist {user_id} save failed due to no difference to draft rank."))
    return(0)
  }

  # # TODO get saving working
  # browser()

  DBI::dbSendQuery(
    con,
    glue::glue_sql(
      .con = con,
      "DELETE FROM watchlists WHERE user_id = {user_id};"
    )
  )

  res <- DBI::dbSendQuery(
    con,
    glue::glue_sql(
      .con = con,
      "INSERT INTO watchlists (user_id, player, ranking)
      SELECT user_id, player, ranking FROM watchlists
      UNION
      VALUES
      {paste(values_out, collapse = ',')}
      except
      SELECT user_id, player, ranking FROM watchlists;"
    ) %>%
      gsub("'", "", .)
  )
  print(res)
  return(1)

}
