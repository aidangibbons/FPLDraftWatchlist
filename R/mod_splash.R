#' splash UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_splash_ui <- function(id){
  ns <- NS(id)
  tagList(
    modalDialog(
      title = NULL,
      h1(align = "center", 'Create or load watchlist'),
      modal_row(
        textInput(ns("txtUser"), "Username"),
      ),
      modal_row(
        passwordInput(ns("pwdPass"), "Password")
      ),
      fluidRow(
        column(10, offset = 1, align = "center",
               actionButton(ns("btnCreateWatchlist"), "Create Watchlist", icon = icon("plus")),
               actionButton(ns("btnLoadWatchlist"), "Load Watchlist", icon = icon("folder-open"))
        )
      ),
      fade = T,
      footer = NULL
    )
  )
}

#' splash Server Functions
#'
#' @noRd
#' @importFrom shinyalert shinyalert
#' @importFrom glue glue
mod_splash_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    user_list <- c("aidan", "kieran", "oscar")

    observeEvent(input$btnCreateWatchlist, {

      user = input$txtUser
      pass = input$pwdPass

      shinyalert(title = "New Watchlist Generated!",
                 text = glue("User: {user}, Pass: {pass}"),
                 type = "success", timer = 1000)
      removeModal()
    })

    observeEvent(input$btnLoadWatchlist, {
      shinyalert(title = "Watchlist Successfully Loaded!",
                 text = glue("User: {user}, Pass: {pass}"),
                             type = "success", timer = 1000)
      removeModal()
    })

  })
}

modal_row <- function (...) {
  fluidRow(
    column(10, offset = 1, align = "center", ...)
  )
}

