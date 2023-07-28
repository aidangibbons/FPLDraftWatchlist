#' login UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyjs
#' @import shinyauthr
mod_login_ui <- function(id){
  ns <- NS(id)
  tagList(
    loginUI_custom(
      title = "Create or load watchlist",
      id = ns("login")
    )
  )
}


#' login Server Functions
#'
#' @noRd
mod_login_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    is_local <- T
    print(glue::glue("Is local session: {is_local}"))

    con <- get_DB_connection(is_local = is_local)

    set_config_variables()

    user_base <- get_user_base(con)

    credentials <- loginServer_custom(
      con,
      id = "login",
      data = user_base,
      user_col = "username",
      pwd_col = "pass",
      sodium_hashed = TRUE
    )

    logout_init <- shinyauthr::logoutServer(
      id = "logout",
      active = reactive(credentials()$user_auth)
    )

    return(
      list(
        con = con,
        credentials = credentials
      )
    )
  })
}


check_username <- function (u) {
  # TODO add username text validation and update
  #  ui error message accordingly
}

check_password <- function (p) {
  # TODO add password text validation and update
  #  ui error message accordingly
}


add_user <- function (con, user, pass, user_base) {

  user <- tolower(user)

  if (user %in% user_base$username) {

    print(glue::glue("Error trying to add user: {user}. User already exists"))

    return(NULL)
  }

  tryCatch({
    print(glue::glue("Trying to create new user: {user}..."))
    res <- DBI::dbSendQuery(
      con,
      glue::glue_sql(
        "INSERT INTO users (username, pass) VALUES ({user}, {pass});",
        .con = con
      )
    )

    print(glue::glue("Query successful!"))

    res_row <- get_DB_table(con, "users",
                            glue::glue("SELECT * FROM users WHERE username = '{user}';"))

    return(res_row)
  },
  error=function(cond) {
    print(glue::glue("Database query failed, add_user: {user}."))

    return(NULL)
  })

}

loginUI_custom <- function (id, title = "Please log in", user_title = "User Name",
                            pass_title = "Password",
                            additional_ui = NULL,
                            cookie_expiry = 7) {
  ns <- shiny::NS(id)
  shinyjs::hidden(
    shiny::div(
      id = ns("panel"),
      style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      shiny::wellPanel(
        shinyjs::useShinyjs(),
        shinyauthr:::jscookie_script(),
        shinyjs::extendShinyjs(
          text = shinyauthr:::js_cookie_to_r_code(
            ns("jscookie"),
            expire_days = cookie_expiry),
          functions = c("getcookie",
                        "setcookie", "rmcookie")
        ),
        shinyjs::extendShinyjs(
          text = shinyauthr:::js_return_click(
            ns("password"),
            ns("button")
          ),
          functions = c()
        ),
        shiny::tags$h2(
          title,
          class = "text-center",
          style = "padding-top: 0;"
        ),
        shiny::textInput(
          ns("user_name"),
          shiny::tagList(
            shiny::icon("user"),
            user_title)
        ),
        shiny::passwordInput(
          ns("password"),
          shiny::tagList(
            shiny::icon("unlock-alt"),
            pass_title
          )
        ),
        shiny::fluidRow(
          shiny::div(
            style = "text-align: center;",
            shiny::actionButton(
              ns("button"),
              "Load or Create Watchlist",
              class = "btn-primary",
              icon = icon("folder-open"))
          ),
        ),
        additional_ui,
        shinyjs::hidden(
          shiny::div(
            id = ns("error"),
            shiny::tags$p(
              "Invalid username or password!",
              style = "color: red; font-weight: bold; padding-top: 5px;",
              class = "text-center"
            )
          )
        ),
        shinyjs::hidden(
          shiny::div(
            id = ns("username_error"),
            shiny::tags$p(
              "Invalid username, creating new watchlist",
              style = "color: blue; font-weight: bold; padding-top: 5px;",
              class = "text-center"
            )
          )
        )
      )
    )
  )
}

loginServer_custom <- function (con, id, data, user_col, pwd_col, sodium_hashed = FALSE,
                                log_out = shiny::reactiveVal(), reload_on_logout = FALSE,
                                cookie_logins = FALSE, sessionid_col, cookie_getter, cookie_setter) {
  try_class_uc <- try(class(user_col), silent = TRUE)
  if (try_class_uc == "character") {
    user_col <- rlang::sym(user_col)
  }
  try_class_pc <- try(class(pwd_col), silent = TRUE)
  if (try_class_pc == "character") {
    pwd_col <- rlang::sym(pwd_col)
  }
  if (cookie_logins && (missing(cookie_getter) | missing(cookie_setter) |
                        missing(sessionid_col))) {
    stop("if cookie_logins = TRUE, cookie_getter, cookie_setter and sessionid_col must be provided")
  }
  else {
    try_class_sc <- try(class(sessionid_col), silent = TRUE)
    if (try_class_sc == "character") {
      sessionid_col <- rlang::sym(sessionid_col)
    }
  }
  data <- dplyr::mutate_if(data, is.factor, as.character)
  shiny::moduleServer(id, function(input, output, session) {
    credentials <- shiny::reactiveValues(user_auth = FALSE,
                                         info = NULL, cookie_already_checked = FALSE)
    shiny::observeEvent(log_out(), {
      if (cookie_logins) {
        shinyjs::js$rmcookie()
      }
      if (reload_on_logout) {
        session$reload()
      }
      else {
        shiny::updateTextInput(session, "password", value = "")
        credentials$user_auth <- FALSE
        credentials$info <- NULL
      }
    })
    shiny::observe({
      if (cookie_logins) {
        if (credentials$user_auth) {
          shinyjs::hide(id = "panel")
        }
        else if (credentials$cookie_already_checked) {
          shinyjs::show(id = "panel")
        }
      }
      else {
        shinyjs::toggle(id = "panel", condition = !credentials$user_auth)
      }
    })
    if (cookie_logins) {
      shiny::observeEvent(shiny::isTruthy(shinyjs::js$getcookie()),
                          {
                            shinyjs::js$getcookie()
                          })
      shiny::observeEvent(input$jscookie, {
        credentials$cookie_already_checked <- TRUE
        shiny::req(credentials$user_auth == FALSE, is.null(input$jscookie) ==
                     FALSE, nchar(input$jscookie) > 0)
        cookie_data <- dplyr::filter(cookie_getter(),
                                     {
                                       {
                                         sessionid_col
                                       }
                                     } == input$jscookie)
        if (nrow(cookie_data) != 1) {
          shinyjs::js$rmcookie()
        }
        else {
          .userid <- dplyr::pull(cookie_data, {
            {
              user_col
            }
          })
          .sessionid <- randomString()
          shinyjs::js$setcookie(.sessionid)
          cookie_setter(.userid, .sessionid)
          cookie_data <- utils::head(dplyr::filter(cookie_getter(),
                                                   {
                                                     {
                                                       sessionid_col
                                                     }
                                                   } == .sessionid, {
                                                     {
                                                       user_col
                                                     }
                                                   } == .userid))
          credentials$user_auth <- TRUE
          credentials$info <- dplyr::bind_cols(dplyr::filter(data,
                                                             {
                                                               {
                                                                 user_col
                                                               }
                                                             } == .userid), dplyr::select(cookie_data,
                                                                                          -{
                                                                                            {
                                                                                              user_col
                                                                                            }
                                                                                          }))
        }
      })
    }
    shiny::observeEvent(input$button, {
      row_username <- which(dplyr::pull(data, {
        {
          user_col
        }
      }) == tolower(input$user_name))
      if (length(row_username)) {
        row_password <- dplyr::filter(data, dplyr::row_number() ==
                                        row_username)
        row_password <- dplyr::pull(row_password, {
          {
            pwd_col
          }
        })
        if (sodium_hashed) {
          password_match <- sodium::password_verify(row_password,
                                                    input$password)
        }
        else {
          password_match <- identical(row_password, input$password)
        }
      }
      else {
        password_match <- FALSE

      }
      if (length(row_username) == 1 && password_match) {
        print(glue::glue("Password match for user: {input$user_name}, login successful!"))

        credentials$user_auth <- TRUE
        credentials$info <- dplyr::filter(data, {
          {
            user_col
          }
        } == tolower(input$user_name))
        if (cookie_logins) {
          .sessionid <- randomString()
          shinyjs::js$setcookie(.sessionid)
          cookie_setter(tolower(input$user_name), .sessionid)
          cookie_data <- dplyr::filter(dplyr::select(cookie_getter(),
                                                     -{
                                                       {
                                                         user_col
                                                       }
                                                     }), {
                                                       {
                                                         sessionid_col
                                                       }
                                                     } == .sessionid)
          if (nrow(cookie_data) == 1) {
            credentials$info <- dplyr::bind_cols(credentials$info,
                                                 cookie_data)
          }
        }

        shinyalert(title = "Watchlist Successfully Loaded!",
                   text = glue("User: {input$user_name}"),
                   type = "success", immediate = T, timer = 1500)

      }
      else {
        if (length(row_username)) {
          print(glue::glue("Incorrect password match for user: {input$user_name}"))

          shinyjs::toggle(id = "error", anim = TRUE, time = 1,
                          animType = "fade")
          shinyjs::delay(
            5000,
            shinyjs::toggle(id = "error", anim = TRUE, time = 1,
                            animType = "fade"))
        } else {

          credentials$user_auth <- TRUE

          res_row <- add_user(con,
                              user = input$user_name,
                              pass = sodium::password_store(input$password),
                              user_base = data)

          print("New user added to database:")
          print(res_row$id)
          print(res_row$username)

          credentials$info <- res_row

          # TODO after this we need to update
          #  credentials$info with the table info
          #  we can do this by outputting a read of the
          #  users table, from add_user
          #  then assign here

          shinyalert(title = "New Watchlist Generated!",
                     text = glue("User: {input$user_name}"),
                     type = "success", immediate = T, timer = 1200)
        }


      }
    })
    shiny::reactive({
      shiny::reactiveValuesToList(credentials)
    })
  })
}
