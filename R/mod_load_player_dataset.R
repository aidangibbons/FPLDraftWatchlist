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
mod_load_player_dataset_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    base_table <- tibble(web_name = c("Salah", "Kane", "Haaland"), total_points = c(220, 268, 277)) %>%
      rownames_to_column("Draft Rank") %>%
      mutate("Community Ranking" = 1:n()) %>%
      mutate(test = '≡') %>%
      select(test, everything())

    return(base_table)

  })
}

## To be copied in the UI
# mod_load_player_dataset_ui("load_player_dataset_1")

## To be copied in the server
# mod_load_player_dataset_server("load_player_dataset_1")
