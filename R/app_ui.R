#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    fluidPage(
      titlePanel(
        mod_title_bar_ui("header")
      ),
      mod_login_ui("login"),
      fluidRow(
        column(
          width = 4,
          mod_main_table_ui("watchlist")
        ),
        column(
          width = 8,
          mod_player_statistics_ui("statistics")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "FPLDraftWatchlist"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
