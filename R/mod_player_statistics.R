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
    wellPanel(
      tableOutput(ns("tblSelectedPlayers"))
    )
  )
}

#' player_statistics Server Functions
#'
#' @noRd
mod_player_statistics_server <- function(id, sel){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tblSelectedPlayers <- renderTable({
      sel()
    })
  })
}

## To be copied in the UI
# mod_player_statistics_ui("player_statistics_1")

## To be copied in the server
# mod_player_statistics_server("player_statistics_1")
