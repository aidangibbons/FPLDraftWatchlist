library(shiny)
library(DT)
library(dplyr)
library(glue)

ui <- function () {
  fluidPage(
    DTOutput("tblWatchlist"),
    textOutput("selected"),
    DTOutput("tbl2"),
    DTOutput("tbl3")
  )
}

server <- function (input, output, session) {

  df_raw = data.frame(rank = 1:5, web_name = c("Haaland", "Salah", "Kane", "De Bruyne", "Joelinton"), draft_rank = 1:5)
  df <- reactiveVal(df_raw)
  slct <- reactiveVal(NULL)
  df_update <- reactiveVal(df_raw)
  table_order <- reactiveVal(1:nrow(df_raw))

  max_rows = 3
  selectable_col = 2
  editable_col = 1
  proxy <- dataTableProxy("tblWatchlist")

  callback <- c(
    "table.on('row-reorder', function(e, details, edit){",
    "  var oldRows = [], newRows = [];",
    "  for(let i=0; i < details.length; ++i){",
    "    oldRows.push(details[i].oldData);",
    "    newRows.push(details[i].newData);",
    "  }",
    "  Shiny.setInputValue('rowreorder', {old: oldRows, new: newRows});",
    "});",
    "table.on('click', 'td', function() {
      if ($(this).get(0)._DT_CellIndex.column == 1) {
        $(this).dblclick();
      }
  });"
  )

  output$tblWatchlist <- renderDT(server  = T, {
    datatable(df_raw, extensions = c("RowReorder"),
              selection = list(mode = "multiple", target = "cell",
                               selectable = cbind(1:nrow(df_raw), rep(selectable_col, nrow(df_raw)))),
              editable = list(target = 'cell', disable = list(columns = (0:(ncol(df_raw)))[-(editable_col + 1)])),
              options = list(
                rowReorder = T,
                columnDefs = list(
                )
              ),
              callback = JS(callback)
    )
  })

  # TODO implement code for table editing


  observeEvent(input$tblWatchlist_cell_edit, {
    print(input$tblWatchlist_cell_edit)
    print("Edit observe entered")
    row_selection <- isolate(slct())
    inp_edit <- input$tblWatchlist_cell_edit

    if (is.null(inp_edit)) {return()}

    out <- calculate_edit(inp_edit, df)
    df_update(out)
  })

  # update proxy and selection reactive, when rows are selected
  observe({
    sel_raw <- input$tblWatchlist_cells_selected

    if (length(sel_raw) == 0) {return()}

    print(sel_raw)
    rows <- sel_raw[, 1]

    print(rows)
    print("row selection observe entered")

    if (length(rows) > max_rows) {
      # drop the second-last clicked row
      rows <- rows[c(1:(max_rows - 1), length(rows))]
      cells <- cbind(rows, rep(selectable_col, length(rows)))
      DT::selectCells(proxy, cells)
    }

    slct(rows)
  })


  observeEvent(input$rowreorder, {
    print("Re-order observe entered")
    row_selection <- isolate(slct())
    inp_reorder <- input$rowreorder

    print(inp_reorder)
    if (is.null(inp_reorder) | length(inp_reorder) == 0) {return()}

    out <- calculate_reorder(inp_reorder, df, row_selection)
    print(out)
    df_update(out)

  })


  observeEvent(df_update(), ignoreInit = T, {
    print("DF update entered")
    out <- df_update()
    print(out)

    if (is.null(out) | length(out) == 0) {return()}

    if (!identical(out$df$rank, 1:length(out$df$rank))) {
      out$df$rank <- 1:length(out$df$rank)
    }

    df(out$df)
    table_order(order(out$df$draft_rank))

    print(out$df)

    DT::replaceData(proxy, df(), resetPaging = F, clearSelection = "none")

    if (!identical(slct(), out$sel)) {
      slct(out$sel)
      rows <- slct()
      cells <- cbind(rows, rep(selectable_col, length(rows)))
      DT::selectCells(proxy, cells)
    }
  })

  output$tbl2 <- renderDT({
    df_selected <- slice(df(), slct())
    datatable(df_selected)
  })

  output$tbl3 <- renderDT({
    datatable(df())
  })

}


calculate_reorder <- function (r, df, sel) {
  print("reorder function entered")
  old <- as.numeric(unlist(r$old))
  new <- as.numeric(unlist(r$new))


  if (is.null(old) | is.null(new)) {return()}
  if (length(old) + length(new) == 0) {return()}

  print(paste(old, new, sep = " -> ", collapse = ", "))

  df_orig <- isolate(df())
  df_reordered <- df_orig
  df_reordered[new, ] <- df_reordered[old, ]

  if (length(sel) > 0) {
    row_selection_orig <- df_orig$rank[sel]
    row_selection_new <- which(df_reordered$rank %in% row_selection_orig)
  } else {
    row_selection_new <- NULL
  }

  list(df = df_reordered, sel = row_selection_new)

}

calculate_edit <- function (ed, df) {
  print("edit function entered")
  if (ed$col != 1) {return()}

  df_orig <- isolate(df())
  df_replaced <- df_orig
  df_replaced$rank[ed$row] <- ed$value
  prev_rank <- df_orig$rank[ed$row]
  df_replaced$rank[df_orig$rank >= ed$value & df_orig$rank < prev_rank] <- df_replaced$rank[df_orig$rank >= ed$value & df_orig$rank < prev_rank] + 1
  df_ranked <- arrange(df_replaced, rank)

  list(df = df_ranked, sel = ceiling(ed$value))
}

runApp(list(ui = ui, server = server), port = 3606)
