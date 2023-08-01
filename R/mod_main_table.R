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
    uiOutput(ns("uiMainTable"))
  )
}

#' main_table Server Functions
#'
#' @noRd
#' @importFrom DT renderDT datatable JS dataTableProxy formatStyle
#' @importFrom yaml read_yaml
#' @import dplyr
mod_main_table_server <- function(id, credentials, df_raw, ord){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$uiMainTable <- renderUI({
      req(credentials()$user_auth)
      print_debug("render main table UI")
      tagList(
        fluidRow(
          column(width = 9,
                 div(style = "margin-top: -12px;", p("Click and drag handle to reorder rows.")),
                 div(style = "margin-top: -12px;", p("Click 'Ranking' cells to edit manually.")),
                 div(style = "margin-top: -12px;", p("Click on player names to view data comparison."))
          ),
          column(width = 3,
                 div(style = "display:inline-block; float:right", actionButton(ns('btnClearRows'), 'Clear Rows'))
          )
        ),
        DTOutput(ns("tblWatchlist")),
        hr()
      )
    })

    df_tbl <- df_raw %>%
      select(test, rank, draft_rank, web_name, position, team_name_short)

    df <- reactiveVal(df_tbl)
    slct <- reactiveVal(NULL)
    df_update <- reactiveVal(df_tbl)
    table_order <- reactiveVal(1:nrow(df_tbl))
    max_rows <- 3
    selectable_col = 4
    editable_col = 2

    observeEvent(ord(), {
      req(credentials()$user_auth)
      print_debug("update table with new ordering")

      out <- list(
        df = df_tbl %>%
          slice(ord()),
        sel = NULL,
        dont_update_proxy = T
      )

      df_update(out)

    })

    # observeEvent(ord(), {
    #   req(credentials()$user_auth)
    #   print_debug("update table with new ordering")
    #
    #   out <- list(
    #     df = df_raw %>%
    #       slice(ord()),
    #     sel = NULL
    #   )
    #   df_update(out)
    #
    # })

    proxy <- dataTableProxy("tblWatchlist")

    callback <- c(
      "table.on('row-reorder', function(e, details, edit){",
      "  var oldRows = [], newRows = [];",
      "  for(let i=0; i < details.length; ++i){",
      "    oldRows.push(details[i].oldData);",
      "    newRows.push(details[i].newData);",
      "  }",
      sprintf("  Shiny.setInputValue('%s', {old: oldRows, new: newRows});", session$ns("rowreorder")),
      "});",
      "table.on('click', 'td', function() {
        if ($(this).get(0)._DT_CellIndex.column == 2) {
          $(this).dblclick();
        }
  });"
    )


    output$tblWatchlist <- renderDT(server = T, {

      req(credentials()$user_auth)
      print_debug("create main table DT")
      df_tbl %>%
        slice(ord()) %>%
        mutate(rank = 1:n()) %>%
        datatable(
          escape = F,
          filter = "none",
          colnames = c(NULL, "", "Ranking", "Draft Rank", "Player", "Position", "Team"),
          selection = list(mode = "multiple", target = "cell",
                           selectable = cbind(1:nrow(df_tbl), rep(selectable_col, nrow(df_tbl)))),
          editable = list(target = 'cell', disable = list(columns = (0:(ncol(df_tbl)))[-(editable_col + 1)])),
          extensions = c('RowReorder'),
          options = list(order = list(list(0, 'asc')),
                         rowReorder = TRUE,
                         pageLength = 30,
                         dom = "frtip",
                         ordering = F,
                         columnDefs = list(list(visible=FALSE,
                                                targets = c(0)),
                                           list(className = 'dt-center',
                                                targets = "_all"))),
          callback=JS(callback)
        )
    })

    # update proxy with edited cells
    observeEvent(input$tblWatchlist_cell_edit, {
      print_debug("enter cell edit observe")
      row_selection <- isolate(slct())
      inp_edit <- input$tblWatchlist_cell_edit

      if (is.null(inp_edit)) {return()}

      out <- calculate_edit(inp_edit, df, editable_col)
      df_update(out)
      print_debug("complete cell edit observe")
    })

    # update proxy and selection reactive, when rows are selected
    observe({
      req(credentials()$user_auth)
      print_debug("enter cell selection observe")
      sel_raw <- input$tblWatchlist_cells_selected

      if (length(sel_raw) == 0) {return()}
      rows <- sel_raw[, 1]

      if (length(rows) > max_rows) {
        # drop the second-last clicked row
        rows <- rows[c(1:(max_rows - 1), length(rows))]
        cells <- cbind(rows, rep(selectable_col, length(rows)))
        DT::selectCells(proxy, cells)
      }
      slct(rows)
      print_debug("complete cell selection observe")
    })


    observeEvent(input$rowreorder, {
      print_debug("enter rowreorder observe")

      row_selection <- isolate(slct())
      inp_reorder <- input$rowreorder

      if (is.null(inp_reorder) | length(inp_reorder) == 0) {return()}

      out <- calculate_reorder(inp_reorder, df, row_selection)
      df_update(out)
      print_debug("complete rowreorder observe")
    })

    observeEvent(df_update(), ignoreInit = T, {
      print_debug("enter df_update observe")
      out <- df_update()

      if (is.null(out) | length(out) == 0) {return()}
      if (!identical(out$df$rank, 1:length(out$df$rank))) {
        out$df$rank <- 1:length(out$df$rank)
      }

      df(out$df)
      table_order(rank(out$df$draft_rank))

      if (!("dont_update_table" %in% names(out))) {
        print_debug("updating proxy with replaceData")
        DT::replaceData(proxy, out$df, resetPaging = F, clearSelection = "none")
      }

      if (!identical(slct(), out$sel)) {
        slct(out$sel)
        rows <- slct()
        cells <- cbind(rows, rep(selectable_col, length(rows)))
        print_debug("updating proxy with selectCells")
        DT::selectCells(proxy, cells)
      }

      print_debug("complete df_update observe")
    })

    observeEvent(input$btnClearRows, {
      print_debug("enter btn clear rows observe")
      slct(NULL)
      DT::selectRows(proxy, data.frame())
      print_debug("complete btn clear rows observe")
    })


    df_out <- reactive({
      print_debug("enter df_out reactive")
      df_raw %>%
        slice(table_order())
    })

    sel_out <- reactive({
      print_debug("enter sel_out reactive")
      df_tbl$draft_rank[table_order()][slct()]
    })

    return(
      list(
        order = df_out,
        selected_players = sel_out
      )
    )
  })
}

## functions ####
calculate_reorder <- function (r, df, sel) {
  print_debug("reorder entered")
  old <- as.numeric(unlist(r$old))
  new <- as.numeric(unlist(r$new))

  if (is.null(old) | is.null(new)) {return()}
  if (length(old) + length(new) == 0) {return()}

  df_orig <- isolate(df())
  df_reordered <- df_orig
  df_reordered[new, ] <- df_reordered[old, ]

  if (length(sel) > 0) {
    row_selection_orig <- df_orig$rank[sel]
    row_selection_new <- unique(which(df_reordered$rank %in% c(row_selection_orig)))
  } else {
    row_selection_new <- NULL
  }

  print_debug("reorder finished")
  list(df = df_reordered, sel = row_selection_new)
}

calculate_edit <- function (ed, df, editable_col = 2) {
  print_debug("edit entered")
  if (ed$col != editable_col) {return()}
  if (ed$row == ed$value) {return()}

  ed$value <- if (ed$value > ed$row) {
    ed$value + 0.1
  } else {
    ed$value - 0.1
  }

  df_orig <- isolate(df())
  df_replaced <- df_orig
  df_replaced$rank[ed$row] <- ed$value
  prev_rank <- df_orig$rank[ed$row]
  df_replaced$rank[df_orig$rank >= ed$value & df_orig$rank < prev_rank] <- df_replaced$rank[df_orig$rank >= ed$value & df_orig$rank < prev_rank] + 1
  df_ranked <- arrange(df_replaced, rank)
  df_ranked <- mutate(df_ranked, rank = round(rank))

  ed$value <- round(ed$value)

  print_debug("edit finished")
  list(df = df_ranked, sel = ceiling(ed$value))
}
