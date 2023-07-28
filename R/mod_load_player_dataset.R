#' load_player_dataset UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_player_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' load_player_dataset Server Functions
#'
#' @noRd
#' @importFrom tibble tibble rownames_to_column
#' @importFrom dplyr mutate select
mod_load_player_dataset_server <- function(id, con, credentials){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    base_table <- get_player_details(url_bootstrap_static) %>%
      mutate(test = '≡') %>%
      mutate(rank = draft_rank) %>%
      select(id, test, rank, everything()) %>%
      arrange(rank)# %>%
      # slice(1:8)


    final_tbl <- reactive({
      if (credentials()$user_auth) {
        set_config_variables()
        user = credentials()$info$id



        print(glue::glue("Attempting watchlist load for watchlist: {user}"))

        tryCatch({
          saved_watchlist <- DBI::dbGetQuery(con,
                                             glue::glue_sql(
                                               .con = con,
                                               "SELECT * FROM watchlists WHERE user_id = {user}"
                                             )
          )
        }, error = function(e) {
          shinyalert(
            title = glue::glue("Error"),
            text = glue::glue("Error loading watchlist from database."), type = "error", immediate = T,
            timer = 3000
          )
          print(glue::glue("Error loading watchlist from database."))
          return(base_table)
        })

        print("Watchlist load successful!")

        if (nrow(saved_watchlist) == 0) {

          print(glue::glue("No watchlist rows found: {user}. Returning base table"))
          return(base_table)
        } else {

          out_tbl <- base_table %>%
            left_join(saved_watchlist %>%
                        select(player, "saved_rank" = ranking), by = c("id" = "player")) %>%
            # take the saved rank if exists. fill in all other players by draft rank
            #  this allows for new players to be added to the game and not be pushed to the bottom
            mutate(rank = ifelse(is.na(saved_rank),
                                 draft_rank,
                                 saved_rank)) %>%
            arrange(rank) %>%
            mutate(rank = 1:n()) %>%
            select(-saved_rank)

          return(out_tbl)
        }
      }
      else {
        return(base_table)
      }
    })


    table_order <- reactive({
      req(credentials()$user_auth)
      rank(final_tbl()$draft_rank)
    })

    return(
      list(
        tbl = base_table,
        ord = table_order
      )
    )
  })

}

## To be copied in the UI
# mod_load_player_dataset_ui("load_player_dataset_1")

## To be copied in the server
# mod_load_player_dataset_server("load_player_dataset_1")
