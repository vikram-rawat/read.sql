# Open a DB pool ----------------------------------------------------------
# written by    : Vikram Singh Rawat
# written on    : 15th Jan 2021
# purpose       : Database Connection
# desc          : establish a pool connection with the database

create_pool <- function(
  param_list,
  min = 2,
  max = 20,
  idle = 10
){

  pool <- dbPool(
    drv = RPostgres::Postgres(),
    host = param_list$server,
    user = param_list$uid,
    password = param_list$pwd,
    port = param_list$port,
    dbname = param_list$database,
    minSize = min,
    idleTimeout = idle,
    maxSize = max
  )

  return(pool)

}

# open a DB conn ----------------------------------------------------------
# written by    : Vikram Singh Rawat
# written on    : 15th Jan 2021
# purpose       : Database Connection
# desc          : establish a normal connection with the database

create_conn <- function(
  param_list
  ){

  conn <- dbConnect(
    drv = RPostgres::Postgres(),
    host = param_list$server,
    user = param_list$uid,
    password = param_list$pwd,
    port = param_list$port,
    dbname = param_list$database
  )

  return(conn)

}
# read SQL files ----------------------------------------------------------
# written by    : Vikram Singh Rawat
# purpose       : Read SQL files
# desc          : readSQLFiles

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
        sub(pattern = "--",
            replacement = "/*",
            x = line),
        "*/"
      )
    }

    sql_string <- paste(sql_string, line)
  }

  close(con)

  return(sql_string)

}
# get_sql_query_from_files_interpolated -------------------------------------
# written by    : Vikram Singh Rawat
# purpose       : read SQL files
# desc          : get sql query interpolated

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
# written by    : Vikram Singh Rawat
# purpose       : read SQL files
# desc          : get sql query interpolated

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
