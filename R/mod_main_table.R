#' main_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT DTOutput
mod_main_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("tblWatchlist"))
  )
}

#' main_table Server Functions
#'
#' @noRd
#' @importFrom DT renderDT datatable JS dataTableProxy formatStyle
#' @importFrom yaml read_yaml
#' @import dplyr
mod_main_table_server <- function(id, df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tblWatchlist <- renderDT({
      df %>%
        datatable_watchlist()

    })

    table_order <- reactiveVal(value = NULL)
    watchlist_id = reactiveVal(value = NULL)


    proxy <- dataTableProxy("tblWatchlist")

    observeEvent(input$rowreorder, {
      old <- unlist(input$rowreorder$old)
      new <- unlist(input$rowreorder$new)
      dat[new, ] <- dat[old, ]
      replaceData(proxy, dat, resetPaging = FALSE)
    })

    selected_players <- reactive({
      input$tblWatchlist_rows_selected
    })

    base_table = reactive({
      df
    })

    return(
      list(
        table = base_table,
        selected_players = selected_players
      )
    )




    ## TODO implement alllll this

    # ## watchlist tab ####

    #

    #

    #
    # observeEvent(input$btnNewWatchlist, {
    #
    #   print(glue("New watchlist requested"))
    #
    #   table_order(NULL)
    #
    #   max_watchlist_id = drop_dir(glue("{drop_dir}/watchlists"), dtoken = token) %>%
    #     pull(name) %>%
    #     tools::file_path_sans_ext() %>%
    #     as.numeric() %>%
    #     max
    #
    #   watchlist_id(max_watchlist_id + 1)
    #
    #   updateTextInput(inputId = "txtWatchlistID", value = watchlist_id())
    #
    #   print(glue("Watchlist {watchlist_id()} created"))
    #
    #   df <- load_ranks()[[game_type()]] %>%
    #     arrange(ranking_position) %>%
    #     mutate(Change = draft_rank - ranking_position) %>%
    #     select("Player" = id, "Name" = web_name, "Team" = team_name_short, "Pos" = position,
    #            "Draft Rank" = draft_rank, "Community Ranking" = ranking_position, id = id)
    #
    #   n_imgs_to_show = min(nrow(df), 120)
    #
    #   df$Player[1:n_imgs_to_show] <- glue("imgs/{cur_season}/players_small/{df$Player[1:n_imgs_to_show]}.png") %>%
    #     as.character()
    #
    #   df <- df %>%
    #     mutate(Player = ifelse(str_detect(Player, "imgs"),
    #                            Player,
    #                            ""))
    #
    #
    #   df$Player[1:n_imgs_to_show] <- df$Player[1:n_imgs_to_show] %>%
    #     map_chr(img_uri, height = 55)
    #
    #   save_watchlist(df, id = watchlist_id())
    #
    #   table_order(1:nrow(df))
    #
    #   shinyalert(title = "New watchlist created", text = glue("Watchlist ID: {watchlist_id()} \n\nRemember this ID and enter it next time to load the saved watchlist."), type = "success")
    #
    #   print(glue("Watchlist {watchlist_id()} saved"))
    # })
    #
    # rankings_df <- reactive({
    #   ## TODO if there is a table already present, save it before loading the new one
    #
    #   if (input$btnLoadWatchlist > 0 | input$btnNewWatchlist > 0) {
    #     table_order(NULL)
    #     watchlist_id(input$txtWatchlistID)
    #
    #     print(glue("Load watchlist request: {input$txtWatchlistID}"))
    #     df <- load_watchlist(watchlist_id())
    #     print(glue("Watchlist {watchlist_id()} loaded"))
    #
    #   } else {
    #     print(glue("No watchlist selected, output empty table"))
    #
    #     tbl_colnames = c("id", "web_name", "team_name_short", "position", "draft_rank", "ranking_position")
    #     df <- as_tibble(matrix(nrow = 0, ncol = length(tbl_colnames)), .name_repair = ~ tbl_colnames)
    #
    #
    #     df <- df %>%
    #       mutate(Change = draft_rank - ranking_position) %>%
    #       select("Player" = id, "Name" = web_name, "Team" = team_name_short, "Pos" = position,
    #              "Draft Rank" = draft_rank, "Community Ranking" = ranking_position, id = id)
    #
    #     n_imgs_to_show = min(nrow(df), 120)
    #
    #     df$Player[1:n_imgs_to_show] <- glue("imgs/{cur_season}/players_small/{df$Player[1:n_imgs_to_show]}.png") %>%
    #       as.character()
    #
    #     df <- df %>%
    #       mutate(Player = ifelse(str_detect(Player, "imgs"),
    #                              Player,
    #                              ""))
    #
    #
    #     df$Player[1:n_imgs_to_show] <- df$Player[1:n_imgs_to_show] %>%
    #       map_chr(img_uri, height = 55)
    #
    #     df
    #   }
    #
    #   table_order(1:nrow(df))
    #
    #   df
    # })
    #
    # watchlist_df <- reactive({
    #
    #   if (input$btnLoadWatchlist > 0) {
    #
    #     print(glue("Loading up to date community rankings"))
    #
    #     community_rankings <- load_ranks()[[game_type()]]
    #
    #     df <- rankings_df() %>%
    #       select(-"Community Ranking") %>%
    #       left_join(community_rankings %>%
    #                   select(id, "Community Ranking" = ranking_position),
    #                 by = "id") %>%
    #       select("Player", "Name", "Team", "Pos",
    #              "Draft Rank", "Community Ranking", id)
    #
    #   } else {
    #
    #     df = rankings_df()
    #   }
    #
    #   df
    # })
    #
    # output$tblWatchlist <- renderDT(server = F, {
    #
    #   watchlist_df() %>%
    #     datatable(
    #       escape = F,
    #       filter = "top",
    #       colnames = c("Personal Ranking" = 1),  # add the name
    #       extensions = c('RowReorder'),
    #       selection = 'none',
    #       options = list(order = list(list(0, 'asc')),
    #                      rowReorder = TRUE,
    #                      pageLength = nrow(watchlist_df()),
    #                      dom = "frtip",
    #                      columnDefs = list(list(orderable = T,
    #                                             className = "reorder",
    #                                             targets = 0),
    #                                        list(visible=FALSE,
    #                                             targets = "id"),
    #                                        list(orderable = F,
    #                                             targets = "_all"),
    #                                        list(className = 'dt-center',
    #                                             targets = "_all"))),
    #       callback=JS(
    #         "// pass on data to R
    # table.on('row-reorder', function(e, details, changes) {
    #     Shiny.onInputChange('table_row_reorder', JSON.stringify(details));
    # });")
    #     ) %>%
    #     formatStyle(columns = c("Community Ranking"), fontWeight = 'bold', `text-align` = 'center', `font-size` = "1.3em")
    #
    # })
    #
    # # observe row reordering event - sent from javascript function
    # observeEvent(input$table_row_reorder, {
    #
    #   info <- input$table_row_reorder
    #
    #   # print(info)
    #   # print(table_order())
    #
    #   # error checking
    #   if(is.null(info) | class(info) != 'character') { return() }
    #
    #   info <- read_yaml(text=info)
    #   # info will be empty if a reorder event fired but no row orders changed
    #   if(length(info) == 0) { return() }
    #
    #   # load our order vectors
    #   .order <- table_order()
    #   .new_order <- .order
    #
    #   # for each updated row in the info object, update the order vector
    #   for(i in 1:length(info)) {
    #     j <- info[[i]]
    #     .new_order[(j$newPosition + 1)] <- .order[(j$oldPosition + 1)]
    #   }
    #
    #   # update our order vector's reactive value
    #   table_order(.new_order)
    # })
    #
    #
    # copy_of_watchlist <- reactive({
    #   rankings_df() %>%
    #     filter(id %in% rankings_df()$id) %>%
    #     slice(table_order())
    # })
    #
    # observe({
    #   print(glue("Current tab: {input$tabs}"))
    # })
    #
    # # on close events ####
    # # upload comparisons when session closed
    # session$onSessionEnded(function() {
    #
    #   print(glue("End of session"))
    #
    #   wl <- isolate(copy_of_watchlist())
    #
    #   if (nrow(wl) > 0 & isolate(input$chkSaveOnClose)) {
    #     print(glue("Saving Watchlist {isolate(watchlist_id())}"))
    #     save_watchlist(wl, id = isolate(watchlist_id()))
    #   }
    #
    #   print(glue("Load comparisons"))
    #   comp_df <- load_comparisons(drop = F)
    #   comp_drop_df <- load_comparisons(drop = T)
    #
    #   comps <- list(
    #     "Classic" = bind_rows(comp_df$Classic,
    #                           comp_drop_df$Classic) %>%
    #       distinct,
    #     "H2H" = bind_rows(comp_df$H2H,
    #                       comp_drop_df$H2H) %>%
    #       distinct
    #
    #   )
    #
    #   print(glue("Save appended comparisons"))
    #   comps %>%
    #     save_comparisons(drop = T)
    #
    #
    #   print(glue("Update ranks - Classic"))
    #   comps$Classic %>%
    #     calculate_ranks(type = "Classic")
    #
    #   print(glue("Update ranks - H2H"))
    #
    #   comps$H2H %>%
    #     calculate_ranks(type = "H2H")
    #
    #
    #   print(glue("End of session save complete"))
    #   ## TODO   update coummnity rankings within watchlist before saving (or when loading?)
    #
    # })


  })
}

## functions ####

datatable_watchlist <- function (df) {

  callback <- c(
    "table.on('row-reorder', function(e, details, edit){",
    "  var oldRows = [], newRows = [];",
    "  for(let i=0; i < details.length; ++i){",
    "    oldRows.push(details[i].oldData);",
    "    newRows.push(details[i].newData);",
    "  }",
    "  Shiny.setInputValue(ns('rowreorder'), {old: oldRows, new: newRows});",
    "});"
  )


  df %>%
    datatable(
      escape = F,
      filter = "top",
      colnames = c("", "Personal Ranking", names(df[-c(1, 2)])),
      extensions = c('RowReorder'),
      selection = 'multiple',
      options = list(order = list(list(0, 'asc')),
                     rowReorder = TRUE,
                     pageLength = nrow(df),
                     dom = "frtip",
                     columnDefs = list(list(orderable = F,
                                            className = "reorder",
                                            targets = 1),
                                       list(visible=FALSE,
                                            targets = 0),
                                       list(orderable = F,
                                            targets = "_all"),
                                       list(className = 'dt-center',
                                            targets = "_all"))),
      callback=JS(callback)
    ) %>%
    formatStyle(columns = c("Community Ranking"), fontWeight = 'bold', `text-align` = 'center', `font-size` = "1.3em")

}

## To be copied in the UI
# mod_main_table_ui("main_table_1")

## To be copied in the server
# mod_main_table_server("main_table_1")
