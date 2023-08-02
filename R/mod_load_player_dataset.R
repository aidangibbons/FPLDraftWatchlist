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
#' @importFrom purrr map map_df
mod_load_player_dataset_server <- function(id, con, credentials, pos_filt = NA_character_, saved_wl){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    set_config_variables()

    bs <<- get_url_data(url_bootstrap_static)
    bd <<- get_url_data(url_bootstrap_dynamic)

    current_gw <<- get_current_gw()

    team_lookup <- bs$teams %>%
      select(id, short_name)

    position_lookup <- bs$element_types %>%
      select("element_type" = id, "position" = "singular_name_short")

    next_5_fixtures <- current_gw:(current_gw + 4) %>%
      map_df(get_gw_fixtures, team_lookup = team_lookup) %>%
      mutate(opp = ifelse(location == "H", toupper(opp), tolower(opp))) %>%
      rename("team_name_short" = team) %>%
      group_by(team_id, team_name_short) %>%
      summarise(opps = list(opp), .groups = "drop")

    base_table <- bs$elements %>%
      tibble %>%
      mutate(test = '≡') %>%
      mutate(rank = draft_rank) %>%
      select(id, test, rank, everything()) %>%
      arrange(rank) %>%
      left_join(next_5_fixtures, by = c("team" = "team_id")) %>%
      left_join(position_lookup, by = "element_type")

    if (!is.na(pos_filt) & pos_filt != "all") {
      # browser()
      base_table <- base_table %>%
        filter(position == toupper(pos_filt)) %>%
        mutate(draft_rank = 1:n()) %>%
        mutate(rank = draft_rank)
    }

    final_tbl <- reactive({
      if (credentials()$user_auth) {
        user = credentials()$info$id

        saved_watchlist = saved_wl() %>%
          filter(watchlist_type == pos_filt)

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
