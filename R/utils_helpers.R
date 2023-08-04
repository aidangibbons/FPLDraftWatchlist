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
url_league_player_status <- function (league) {paste0("https://draft.premierleague.com/api/league/", league, "/element-status")}
url_league_trades <- function (league) {paste0("https://draft.premierleague.com/api/draft/league/",
                                               league_id, "/trades")}
url_league_transactions <- function (league) {paste0("https://draft.premierleague.com/api/draft/league/", league, "/transactions")}
url_league_draft_picks <- function (league) {paste0("https://draft.premierleague.com/api/draft/", league, "/choices")}

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
table_live_draft_picks <- function (league, plot_table = F) {
  bs_static <- url_bootstrap_static %>%
    get_url_data()

  current_picks <- league %>%
    url_league_draft_picks %>%
    get_url_data() %>%
    {.$choices} %>%
    tibble

  player_details <- get_player_details(bs_static) %>%
    select(web_name, id, code, first_name, second_name, position)

  picks_with_players <- current_picks %>%
    mutate(player_name = paste(player_first_name, str_sub(player_last_name, 1, 1))) %>%
    left_join(player_details %>%
                select(web_name, id, code, position), by = c("element" = "id"))

  chosen_players <- picks_with_players %>%
    arrange(index) %>%
    group_by(entry) %>%
    mutate(pick_order = first(index)) %>%
    ungroup %>%
    select(player_name, web_name, round) %>%
    rename("R" = round) %>%
    pivot_wider(names_from = player_name,
                values_from = web_name)

  # table of which positions have been taken
  bootstrap_squad <- bs_static$settings$squad

  req_pos_df <- tibble(position =
                         c(rep("GKP", bootstrap_squad$select_GKP),
                           rep("DEF", bootstrap_squad$select_DEF),
                           rep("MID", bootstrap_squad$select_MID),
                           rep("FWD", bootstrap_squad$select_FWD))
  ) %>%
    group_by(position) %>%
    mutate(selection = 1:n()) %>%
    ungroup

  players_position_picks <- picks_with_players %>%
    group_by(player_name, position) %>%
    mutate(selection = 1:n()) %>%
    ungroup %>%
    select(player_name, position, selection, web_name) %>%
    pivot_wider(names_from = player_name,
                values_from = web_name)

  positions_df <- req_pos_df %>%
    left_join(players_position_picks,
              by = c("position", "selection")) %>%
    rename("Pos" = position) %>%
    select(-selection)

  last_pick <- picks_with_players %>%
    filter(!is.na(element)) %>%
    slice(n()) %>%
    left_join(player_details %>%
                select(first_name, second_name, id), by = c("element" = "id"))

  last_change <- picks_with_players %>%
    filter(!is.na(choice_time)) %>%
    slice(n()) %>%
    pull(choice_time) %>%
    as_datetime()


  calc_time_diff <- abs(as.numeric(int_diff(c(Sys.time(), last_change + hours(1)))))

  last_pick_text <- glue("{toupper(last_pick$player_first_name)} chose {toupper(last_pick$web_name)}")


  if (plot_table) {

    return(grid.arrange(chosen_players %>% tableGrob(),
                        last_pick_text %>% tableGrob(),
                        calc_time_diff %>% tableGrob()))
  }

  # get the image file for the latest address
  latest_player_id <- picks_with_players %>%
    filter(!is.na(choice_time)) %>%
    slice(n()) %>%
    pull(element)

  next_player <- picks_with_players %>% filter(is.na(choice_time)) %>% slice(1) %>%
    pull(player_name)

  next_pick <- if (length(next_player) != 0) {
    paste0("Next Pick:<br/>", next_player)
  } else {
    "Draft is complete!"
  }

  status <- if (length(next_player) != 0) {
    "ongoing"
  } else {
    "complete"
  }

  latest_player_img_file <- last_pick$code %>% url_player_image

  return(list("picks" = chosen_players,
              "latest" = last_pick_text,
              "positions" = positions_df,
              "last_change" = calc_time_diff,
              "img" = latest_player_img_file,
              "next_pick" = next_pick,
              "status" = status,
              "drafted_ids" = current_picks %>% select(element) %>% drop_na %>% pull(element)))
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


