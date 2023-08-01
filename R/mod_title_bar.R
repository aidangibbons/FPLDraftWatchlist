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
    uiOutput(ns("title_bar_ui"))
  )
}

#' title_bar Server Functions
#'
#' @noRd
mod_title_bar_server <- function(id, con, credentials, df_watchlist){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$title_bar_ui <- renderUI({
      req(credentials()$user_auth)
      wellPanel(
        fluidRow(
          column(
            width = 4,
            h1("FPL Draft Watchlist")
          ),
          column(width = 1, offset = 6,
                 shinyjs::hidden(
                   div(
                     checkboxInput(ns("chkSaveOnClose"), value = T, label = HTML("Save watchlist when<br/>closing app?")),
                     style="font-size:50%;"
                   )
                 )
          ),
          column(
            width = 1,
            actionButton(
              inputId =  ns("btnSaveWatchlist"),
              label = "Save Watchlist",
              class = "btn btn-primary",
              icon = icon("save")
            )
          )
        )
      )


    })

    observeEvent(input$btnSaveWatchlist, {

      req(credentials()$user_auth)
      print(glue::glue("Watchlist {credentials()$info$id} ({credentials()$info$username}) save requested... "))

      watchlist_id = credentials()$info$id

      # shinyalert(
      #   title = glue("Error: Watchlist saving not yet enabled"),
      #   text = "", type = "error", immediate = T,
      #   timer = 2000
      # )

      # return(NULL)

      tryCatch({

        res <- save_watchlist(con, watchlist_id, df_watchlist)

        if (res == 1) {
          shinyalert(
            title = glue("Watchlist {credentials()$info$username} saved"),
            text = "", type = "success", immediate = T,
            timer = 2000
          )

          print(glue("Watchlist {watchlist_id} saved successfully!"))
        }
      }, error = function (cond) {

        shinyalert(
          title = glue("Watchlist {credentials()$info$username} save failed.
                       Try again or contact app owner."),
          text = "", type = "error", immediate = T,
          timer = 3000
        )
        print(glue("Watchlist {watchlist_id} save failed. \n--- Error condition: {cond}"))
      })






    })

    SaveOnClose <- reactive({input$chkSaveOnClose})

    return(
      list(
        SaveOnClose = SaveOnClose
      )
    )
  })
}

save_watchlist <- function (con, user_id, wl) {

  values_out <- wl() %>%
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

