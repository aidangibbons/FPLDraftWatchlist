#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

get_DB_connection <- function (db = "fpldraftwatchlist", is_local = T) {

  if (is_local) {
    cat(glue::glue("Connecting to database locally: {db}... "))

    # fly proxy 15432:5432 -a fpldraftwatchlist-db

    db <- "fpldraftwatchlist"  #provide the name of your db
    host_db <- "localhost" #i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
    db_port <- '15432'  # or any other port specified by the DBA
    db_user <- "postgres"
    db_password <- "ZMSN7bOArmN8NtG"

  } else {
    cat(glue::glue("Connecting to non-local database: {db}... "))

    db <- "fpldraftwatchlist"  #provide the name of your db
    host_db <- "fpldraftwatchlist-db.flycast" #i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
    db_port <- '5432'  # or any other port specified by the DBA
    db_user <- "fpldraftwatchlist"
    db_password <- "1fQOWnuXx8zRa0b"

  }

  tryCatch({
    conn <- DBI::dbConnect(RPostgres::Postgres(),
                           dbname = db,
                           host=host_db,
                           port=db_port,
                           user=db_user,
                           password=db_password)
    print(glue::glue("Database Connected!"))

    return(conn)
  },
  error=function(cond) {
    print(glue::glue("Unable to connect to Database. \n--- Error code: {cond}"))

    shinyalert(
      title = glue("Error: Database connection failed. \nPlease refresh to try again or contact app owner."),
      text = "", type = "error", immediate = T,
      timer = 10000, showConfirmButton = F
    )

    stop("Unable to connect to database")

    return(NULL)
  })

}

get_user_base <- function (con) {
  get_DB_table(con, "users")
}

get_DB_table <- function (con, tbl, qry = NULL) {

  if (is.null(qry)) {
    qry <- glue::glue("SELECT * FROM {tbl}")
  }

  tryCatch({
    cat(glue::glue("Trying DB query: {qry}... "))
    res <- DBI::dbGetQuery(con, qry)
    print(glue::glue("Query successful!"))

    return(res)
  },
  error=function(cond) {
    print(glue::glue("Database query failed: {qry}."))

    return(NULL)
  })

}

## FPL functions ####
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param url PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_url_data
#' @export
#' @importFrom curl curl
#' @importFrom jsonlite fromJSON
get_url_data <- function (url) {
  url %>%
    curl %>%
    fromJSON(simplifyVector = T)
}

get_player_details <- function (bs_static) {

  # select only constant columns if "general" is chosen
  player_details <- bs_static$elements %>%
    tibble

  # bind on team information
  player_details_team <- player_details %>%
    left_join(bs_static$teams %>%
                select(id, code, name, short_name) %>%
                rename("team_name" = "name",
                       "team_name_short" = "short_name",
                       "team_code" = "code"),
              by = c("team" = "id"))

  # bind on position information
  player_details_position <- player_details_team %>%
    left_join(bs_static$element_types %>%
                select(id, singular_name_short) %>%
                rename("position" = "singular_name_short"),
              by = c("element_type" = "id"))

  player_details_position %>%
    select(id, code, web_name, position, team_name_short, everything())
}


set_config_variables <- function () {
  cur_season <<-"2023-24"

  url_bootstrap_dynamic <<- "https://draft.premierleague.com/api/game"
  url_bootstrap_static <<- "https://draft.premierleague.com/api/bootstrap-static"

}


url_league_details <- function (league) {paste0("https://draft.premierleague.com/api/league/", league, "/details")}
url_league_player_status <- function (league) {paste0("https://draft.premierleague.com/api/league/", league_id, "/element-status")}
url_league_trades <- function (league) {paste0("https://draft.premierleague.com/api/draft/league/",
                                               league_id, "/trades")}
url_league_transactions <- function (league) {paste0("https://draft.premierleague.com/api/draft/league/", league_id, "/transactions")}
url_league_draft_picks <- function (league) {paste0("https://draft.premierleague.com/api/draft/", league_id, "/choices")}

url_team_details <- function (team_id) {paste0("https://draft.premierleague.com/api/entry/", team_id, "/public")}
url_team_gw <- function (team_id, gw_id) {paste0("https://draft.premierleague.com/api/entry/", team_id, "/event/", gw_id)}
url_team_transactions <- function (team_id) {paste0("https://draft.premierleague.com/api/draft/entry/", team_id,"/transactions")}
url_team_historic_standings <- function (team_id) {paste0("https://draft.premierleague.com/api/entry/", team_id,"/history")}

url_gw_players <- function (gw_id) {paste0("https://draft.premierleague.com/api/event/", gw_id, "/live")}
url_gw_fixtures <- function (gw_id) {paste0("https://draft.premierleague.com/api/event/", gw_id, "/fixtures")}

url_player_history <- function (player_id) {paste0("https://draft.premierleague.com/api/element-summary/", player_id)}
url_player_image <- function (player_code) {paste0("https://resources.premierleague.com/premierleague/photos/players/110x140/p", player_code, ".png")}

url_team_image <- function (team_id) {paste0("https://draft.premierleague.com/img/badges/badge_", team_id,"_40.png")}
url_team_image_new <- function (team_id) {paste0("https://resources.premierleague.com/premierleague/badges/70/t", team_id, ".png")}


get_gw_fixtures <- function (gw, team_lookup) {

  raw_fixtures <- url_gw_fixtures(gw) %>%
    get_url_data() %>%
    tibble

  if (nrow(raw_fixtures) == 0) {
    return(tibble())
  }

  raw_fixtures <- raw_fixtures %>%
    mutate(gw_id = gw) %>%
    rename("match_id" = "id") %>%
    select(gw_id, match_id, started, team_a, team_h, team_a_score, team_h_score, kickoff_time, finished)

  bind_rows(
    raw_fixtures %>%
      rename("team_id" = "team_h",
             "team_goals" = "team_h_score",
             "opp_id" = "team_a",
             "opp_goals" = "team_a_score") %>%
      mutate(location = "H"),
    raw_fixtures %>%
      rename("team_id" = "team_a",
             "team_goals" = "team_a_score",
             "opp_id" = "team_h",
             "opp_goals" = "team_h_score") %>%
      mutate(location = "A")
  ) %>%
    left_join(
      team_lookup %>%
        rename("team" = short_name),
      by = c("team_id" = "id")
    ) %>%
    left_join(
      team_lookup %>%
        rename("opp" = short_name),
      by = c("opp_id" = "id")
    )

}

get_current_gw <- function () {
  gw <- bd$current_event

  if (is.null(gw)) {
    1
  } else {
    gw
  }
}

# General functions ####
img_uri <- function (x, height = 20, local = F) {
  if (local) {
    sprintf('<img src="%s" height="%i"/>', knitr::image_uri(x), height)
  } else {
    sprintf('<img src="%s" style=\"height:%i;\"/>', x, height)

  }
}

print_debug <- function (t, loc = is_local) {
  if (loc) {
    print(t)
  }
}
