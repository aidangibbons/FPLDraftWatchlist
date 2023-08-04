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
    uiOutput(ns("title_bar_ui")),
    uiOutput(ns("uiLiveTitleBar"))
  )
}

#' title_bar Server Functions
#'
#' @noRd
#' @importFrom purrr map2_df
#' @importFrom lubridate as_datetime int_diff hours
#' @importFrom tibble tibble
#' @importFrom stringr str_sub
#' @importFrom tidyr drop_na
mod_title_bar_server <- function(id, con, credentials, df_watchlist_list){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    league_id <- reactiveVal(NULL)

    # set timer-related reactive values
    timer <- reactiveVal(3600)
    timer_active <- reactiveVal(F)
    livedraft_trigger <- reactiveVal(F)
    pickList <- reactiveVal(list(
      status = "",
      drafted_ids = NULL
    ))

    output$title_bar_ui <- renderUI({
      req(credentials()$user_auth)
      req(is.null(league_id()))
      wellPanel(
        fluidRow(
          column(
            width = 4,
            h2("FPL Draft Watchlist")
          ),
          column(width = 1, offset = 6,
                 actionButton(
                   inputId =  ns("btnLiveLeague"),
                   label = "Show Live Draft",
                   class = "btn btn-primary",
                   icon = icon("play")
                 )
          ),
          column(
            width = 1,
            actionButton(
              inputId =  ns("btnSaveWatchlist"),
              label = "Save Watchlist",
              class = "btn btn-primary",
              icon = icon("save")
            ),
            shinyjs::hidden(
              div(
                checkboxInput(ns("chkSaveOnClose"), value = T, label = HTML("Save watchlist when<br/>closing app?")),
                style="font-size:50%;"
              )
            )
          )
        )
      )


    })

    output$uiLiveTitleBar <- renderUI({
      req(league_id())
      wellPanel(
        fluidRow(
          column(
            width = 4,
            h2("FPL Draft Watchlist")
          ),
          column(
            width = 3, offset = 5, align = "right",
            uiOutput(ns("txtLiveStatus"))
          )
        )
      )
    })

    observeEvent(input$btnSaveWatchlist, {

      req(credentials()$user_auth)
      print(glue::glue("Watchlist {credentials()$info$id} ({credentials()$info$username}) save requested... "))

      watchlist_id = credentials()$info$id

      df_watchlist <- df_watchlist_list %>%
        map2_df(names(df_watchlist_list), function(df, nm) {
          df() %>%
            mutate(watchlist_type = nm) %>%
            mutate(user_id = watchlist_id,
                   player = id,
                   ranking = 1:n()) %>%
            filter(row_number() != draft_rank) %>%
            select(watchlist_type, user_id, player, ranking, web_name) %>%
            mutate(across(c(everything(), -watchlist_type, -web_name), as.numeric))
        })

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

    leagueIDModal <- function (failed = F) {
      modalDialog(
        h1("Enter league ID (if known), or team ID"),
        p("To find team ID, got to the 'Status' tab on the draft website."),
        p("On the right-hand sidebar, there is the 'Transactions' section. Click 'View Transactions'."),
        p("Your team ID should be visible in your URL in the format 'https://draft.premierleague.com/entry/TEAM_ID/transactions'"),
        textInput(ns("txtLeagueID"), "League ID:"),
        p("OR"),
        textInput(ns("txtTeamID"), "Team ID:"),
        if (failed)
          p("One of League ID or Team ID must be provided.",
            style = "color: red; font-weight: bold; padding-top: 5px;",
            class = "text-center"),
        footer = actionButton(ns("btnConfirm"), "Confirm")
      )
    }

    observeEvent(input$btnLiveLeague, {
      showModal(leagueIDModal())
    })

    observeEvent(input$btnConfirm, {

      lg <- input$txtLeagueID %>% clean_numeric_input
      tm <- input$txtTeamID %>% clean_numeric_input

      if (!is.null(lg) & !is.na(lg)) {
        league_id(lg)
        removeModal()
      } else if (!is.null(tm) & !is.na(tm)) {
        league_id(
          team_id_to_league_id(tm)
        )
        removeModal()
      } else {
        # TODO make error text appear
        updateTextInput(session, ns("txtLeagueID"), value = "")
        updateTextInput(session, ns("txtTeamID"), value = "")
        showModal(leagueIDModal(failed=T))
      }
    })

    # initiate countdown once a league ID is entered
    observeEvent(league_id(), {
      req(!is.null(league_id()))
      start_time <- league_id() %>%
        url_league_details %>%
        get_url_data() %>%
        {.$league$draft_dt} %>%
        as_datetime()

      timer(as.numeric(int_diff(c(Sys.time(), start_time))))
      timer_active(T)
    })

    # observer that invalidates every second. If timer is active, decrease by one.
    # trigger the switch from countdown -> video -> countdown -> live draft
    observe({
      req(timer_active())
      invalidateLater(1000, session)
      isolate({
        timer(round(timer())-1)
        if (timer() <= 0 ) {
          print_debug("Timer complete.")
          timer_active(F)
          livedraft_trigger(T)
        }
      })
    })

    output$txtLiveStatus <- renderUI({
      req(league_id())
      # browser()
      req(isolate(pickList()$status) != "complete")
      if (timer_active()) {
        out_txt <- format_time(timer())
      } else if (pickList()$status != "complete") {
        invalidateLater(1000 * 5)
        draft <- table_live_draft_picks(league_id())
        pickList(draft)
        out_txt <- draft$latest
      } else {
        out_txt <- "Draft complete!"
      }
      p(out_txt, style = "font-size:80px")
    })

    SaveOnClose <- reactive({input$chkSaveOnClose})

    return(
      list(
        SaveOnClose = SaveOnClose,
        livedraft_trigger = livedraft_trigger,
        pickList = pickList
      )
    )
  })
}

save_watchlist <- function (con, user_id, wl) {

  values_out <- wl %>%
    mutate(out_text = glue::glue("('{watchlist_type}', {user_id}, {player}, {ranking})")) %>%
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

  DBI::dbSendQuery(
    con,
    glue::glue_sql(
      .con = con,
      "DELETE FROM watchlists WHERE user_id = {user_id};"
    )
  )

  res <- DBI::dbSendQuery(
    con,
    glue::glue(
      .con = con,
      "INSERT INTO watchlists (watchlist_type, user_id, player, ranking)
      SELECT watchlist_type, user_id, player, ranking FROM watchlists
      UNION
      VALUES
      {paste(values_out, collapse = ',')}
      EXCEPT
      SELECT watchlist_type, user_id, player, ranking FROM watchlists;"
    )
  )
  print(res)
  return(1)

}

clean_numeric_input <- function(t) {
  tryCatch({
    as.numeric(t)
  }, error = function(e) {
    return(NULL)
  })
}

team_id_to_league_id <- function (team_id) {
  url_team_details(team_id) %>%
    get_url_data %>%
    {.$entry$league_set}
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param secs PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname format_time
#' @export
#' @importFrom glue glue
format_time <- function(secs) {
  hours = secs %/% 3600
  mins = secs %% 3600 %/% 60
  seconds = secs %% 60

  hours_text = if (hours < 10) {paste0("0", hours)} else hours
  mins_text = if (mins < 10) {paste0("0", mins)} else mins
  seconds_text = if (seconds < 10) {paste0("0", seconds)} else seconds
  if (hours > 0) {
    glue("{hours_text}:{mins_text}:{seconds_text}")
  } else if (mins > 0) {
    glue("{mins_text}:{seconds_text}")
  } else {
    glue("{seconds}")
  }
}
