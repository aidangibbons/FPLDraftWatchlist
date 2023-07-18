#' save_watchlist UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_save_watchlist_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' save_watchlist Server Functions
#'
#' @noRd
#' @importFrom shinyalert shinyalert
mod_save_watchlist_server <- function(id, save_btn, df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    eventReactive({save_btn()
      session$onSessionEnded}, {

        print(glue("Watchlist {watchlist_id()} save requested"))

        is_local <- Sys.getenv('SHINY_PORT') == ""
        browser()

        if (is_local) {
          print("woooo save data")
        } else {
          save_watchlist(copy_of_watchlist(), id = watchlist_id())
        }



        shinyalert(title = glue("Watchlist {watchlist_id()} saved"), text = "", type = "success")
        print(glue("Watchlist {watchlist_id()} saved"))

      })

  })
}

## To be copied in the UI
# mod_save_watchlist_ui("save_watchlist_1")

## To be copied in the server
# mod_save_watchlist_server("save_watchlist_1")
