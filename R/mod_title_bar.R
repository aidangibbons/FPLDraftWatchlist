#' title_bar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_title_bar_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(
          width = 4,
          h1("FPL Draft Watchlist")
        ),
        column(
          width = 8,
          textInput(ns("txtFilename"), "Name:"),
          actionButton(ns("btnSaveWatchlist"), "Save Watchlist",
                       class = "btn btn-primary",
                       icon = icon("save")
                       )
        )
      )
    )
  )
}

#' title_bar Server Functions
#'
#' @noRd
mod_title_bar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    save_btn <- reactive(input$btnSaveWatchlist)

    return(save_btn)

  })
}

## To be copied in the UI
# mod_title_bar_ui("title_bar_1")

## To be copied in the server
# mod_title_bar_server("title_bar_1")
