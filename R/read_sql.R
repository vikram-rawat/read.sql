# read SQL files ----------------------------------------------------------
#' read a SQL file
#'
#' @description This function creates an object of type sql_query by reading the sql file.
#'
#' @param filepath path to an SQL file which has a query
#' @param method Only 2 methods are allowed get and post short for getQuery and sendstatement
#'
#' @return character string
#'
#'@export

rs_read_query <- function(
  filepath,
  method = "get"
){

  method <- tolower(method)

  if(! tolower(method) %in% c("get","post")){
    stop("method can only be get or post")
  }

  sql_query <- readChar(
    con = filepath,
    nchars = file.info(filepath)$size,
    useBytes = TRUE
  )

  sql_query <- structure(
    .Data = list(
      sql_query = SQL(sql_query),
      method = tolower(method)
    ),
    class = "sql_query"
  )

  return(sql_query)

}

# print sql query ----------------------------------------------------------
#' Prints a sql.query class
#'
#' @description This method just prints a sql_query class nicely
#'
#' @param sql_query an Object of type sql_query
#'
#' @return character string
#'
print.sql_query <- function(
  sql_query
){

  sprintf(
    fmt = " %s \n---------------------
%s--------------------- ",
sql_query$method,
sql_query$sql_query
  ) |>
    cat()

}


# get_sql_query_from_files_interpolated -------------------------------------
#' get SQL query object
#'
#' @description This function just returns a new sql_query object after interpolation
#'
#' @param sql_query a sql_query object that will be used for sqlinterpolation
#' @param sql_conn a connection object be it a pool or a normal connection to the DB
#' @param query_params A list of values for interpolation in the SQL file
#'
#' @return query object
#'
#' @import DBI
#'
#' @export
rs_interpolate <- function(
  sql_query,
  sql_conn,
  query_params = list()
) {

  # set Variables ------------------------------------------------------------

  sql_query$sql_query <- DBI::sqlInterpolate(
      conn = sql_conn,
      sql = sql_query$sql_query,
      .dots = query_params
    ) |>
    SQL()

  return(sql_query)

}

# send query to DB interpolated -------------------------------------------
#' execute a SQL file
#'
#' @description This function runs a .SQL file against a db connection
#'
#' @param sql_query a sql_query object that will be used for sqlinterpolation
#' @param sql_conn a connection object be it a pool or a normal connection to the DB
#'
#' @return query object
#'
#' @import DBI
#'
#' @export
rs_execute <- function(
  sql_query,
  sql_conn
){

  if ( sql_query$method == "get" ) {

    value <- DBI::dbGetQuery(
      conn = sql_conn,
      statement = sql_query$sql_query
    )

    return(value)

  } else   if ( method == "post" ) {

    value <- DBI::dbExecute(
      conn = sql_conn,
      statement = sql_query$sql_query
    )

    return(value)

  } else {

    stop("Please choose between 'get' and 'post' methods only")

  }

}
