# Open a DB pool ----------------------------------------------------------
#' create a DB pool
#'
#' @description This function just returns a db pool object by passing a config file data to it. You may optionally pass all the parameters as list too.
#'
#' @param param_list A list of values for creating a pool connection. Would actually prefer passing a config file data directly.
#' @param driver A custom driver to connect to database if the need arises
#' @param min A minimum number of connection for pool object to hold at a point in time
#' @param max A maximum number of connection for pool object to hold at a point in time
#' @param idle The number of seconds that an idle object will be kept in the pool before it is destroyed (only applies if the number of objects is over the minSize). Use Inf if you want created objects never to be destroyed (there isn't a great reason for this usually).
#'
#' @return pool object
#'
#' @export
create_pool <- function(
  param_list,
  driver = NULL,
  min = 2,
  max = 20,
  idle = 10
){

  if ( is.null(driver) ) {

    pool <- dbPool(
      drv = param_list$driver,
      host = param_list$server,
      user = param_list$uid,
      password = param_list$pwd,
      port = param_list$port,
      dbname = param_list$database,
      minSize = min,
      idleTimeout = idle,
      maxSize = max
    )

  } else {

    pool <- dbPool(
      drv = driver,
      host = param_list$server,
      user = param_list$uid,
      password = param_list$pwd,
      port = param_list$port,
      dbname = param_list$database,
      minSize = min,
      idleTimeout = idle,
      maxSize = max
    )

  }

  return(pool)

}

# open a DB conn ----------------------------------------------------------
#' create a DB Conn
#'
#' @description This function just returns a db conn object by passing a config file data to it. You may optionally pass all the parameters as list too.
#'
#' @param param_list A list of values for creating a pool connection. Would actually prefer passing a config file data directly.
#' @param driver A custom driver to connect to database if the need arises
#'
#' @return db connection object
#'
#' @export
create_conn <- function(
  param_list,
  driver = NULL
  ){

  if ( is.null(driver) ) {

    conn <- dbConnect(
      drv = param_list$driver,
      host = param_list$server,
      user = param_list$uid,
      password = param_list$pwd,
      port = param_list$port,
      dbname = param_list$database
    )

  } else {

    conn <- dbConnect(
      drv = driver,
      host = param_list$server,
      user = param_list$uid,
      password = param_list$pwd,
      port = param_list$port,
      dbname = param_list$database
    )

  }

  return(conn)

}

# read SQL files ----------------------------------------------------------
#' read a SQL file
#'
#' @description This function just returns a character string from a .SQL file.
#'
#' @param filepath path to an SQL file which has a query
#'
#' @return character string
#'
read_sql <- function(
  filepath
){

  con = file(filepath, "r")
  sql_string <- ""

  while (TRUE){
    line <- readLines(con, n = 1)

    if ( length(line) == 0 ){
      break
    }

    line <- gsub(
      pattern = "\\t",
      replacement =  " ",
      x = line)

    if( grepl("--",line) == TRUE ){

      line <- paste(
        sub(
          pattern = "--",
          replacement = "/*",
          x = line
        ),
        "*/"
      )
    }

    sql_string <- paste(sql_string, line)
  }

  close(con)

  return(sql_string)

}
# get_sql_query_from_files_interpolated -------------------------------------
#' get SQL query object
#'
#' @description This function just returns a db query object from a SQL file path.
#'
#' @param sql_conn a connection object be it a pool or a normal connection to the DB
#' @param sql_file_path path to an SQL file which has a query
#' @param query_params A list of values for interpolation in the SQL file
#'
#' @return query object
#'
#' @export
get_sql_query <- function(
  sql_conn,
  sql_file_path,
  query_params = list()
) {

  # set Variables ------------------------------------------------------------

  sql_query_converted <-

    DBI::sqlInterpolate(
      conn = sql_conn,
      sql = read_sql(sql_file_path),
      .dots = query_params
    )

  return(
    sql_query_converted
  )

}

# send query to DB interpolated -------------------------------------------
#' execute a SQL file
#'
#' @description This function runs a .SQL file against a db connection
#'
#' @param sql_conn a connection object be it a pool or a normal connection to the DB
#' @param sql_file_path path to an SQL file which has a query
#' @param query_params A list of values for interpolation in the SQL file
#' @param method only 2 options 'get' or 'post' to either get the data from SQL or just execute a query on the DB server.
#'
#' @return query object
#'
#' @export
execute_sql_file <- function(
  sql_conn,
  sql_file_path,
  query_params,
  method = "get"
){

  query <- get_sql_query(
    sql_conn = sql_conn,
    sql_file_path = sql_file_path,
    query_params = query_params
  )

  if ( method == "get" ) {

    value <- DBI::dbGetQuery(
      conn = conn,
      statement = query
    )

    return(value)

  } else {

    value <- dbExecute(
      conn = sql_conn,
      statement = query
    )

    return(value)

  }

}
