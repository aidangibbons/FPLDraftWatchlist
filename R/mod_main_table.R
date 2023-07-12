#' main_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_main_table_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' main_table Server Functions
#'
#' @noRd 
mod_main_table_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_main_table_ui("main_table_1")
    
## To be copied in the server
# mod_main_table_server("main_table_1")
