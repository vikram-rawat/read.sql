# read SQL files ----------------------------------------------------------
#' read a SQL file
#'
#' @description This function creates an object of type sql_query by reading the sql file.
#'
#' @param filepath path to an SQL file which has a query
#' @param sql_query_str a proper sql statement string if you don't want to read from a file
#' @param method Only 2 methods are allowed get and post short for getQuery and sendstatement
#' @return character string
#'
#' @export
rs_read_query <- function(
    filepath,
    sql_query_str = "",
    method = "get") {
  method <- tolower(method)

  if (!tolower(method) %in% c("get", "post")) {
    stop("method can only be get or post")
  }

  if (nchar(sql_query_str) > 1) {
    sql_query <- sql_query_str
  } else {
    sql_query <- readChar(
      con = filepath,
      nchars = file.info(filepath)$size,
      useBytes = TRUE
    )
  }

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
#' @method print sql_query
#'
#' @return character string
#'
#' @export
print.sql_query <- function(
    sql_query) {
  cat(
    sprintf(
      fmt = "\n %s ==> \n--------------------- \n%s--------------------- \n",
      sql_query$method,
      sql_query$sql_query
    )
  )
}

# generate_sql_statement -------------------------------------
#' generate sql statement by adding multiple where clause
#'
#' @description This function just returns a new sql_query object after adding multiple 
#' where clause in the SQL statement provided
#'
#' @param sql_query a sql_query object that will be used as a base for sql. 
#' This sql statement shouldn't have a where clause. 
#' That where clause will be added by the function.
#' @param meta_query_params A list of values for adding where clause and 
#' each param should consists of a list of 3 values. col_name, operator and value. 
#' Read examples below to understand it more.
#' it can also deal with parameter where the we need to build an IN value in sql
#'
#' @return query object
#'
generate_sql_statement <- function(sql_query, param_ls) {
  sql_query <- sprintf(
    fmt = "%s \n WHERE 1 = 1 \n", # Base query,
    sql_query
  )

  lapply(
    param_ls,
    function(x) {
      if (isTruthy(x$value)) {
        if (x$wrap) {
          in_values <- sprintf(
            fmt = "('%s')",
            paste(x$value, collapse = "','")
          )
        } else {
          in_values <- x$value
        }

        sql_query <<- sprintf(
          fmt = "%s AND \n    %s %s %s \n",
          sql_query,
          x$col_name,
          x$operator,
          in_values
        )
      }
    }
  )

  print("----------------")
  cat(sql_query)
  print("----------------")

  return(sql_query)
}

# meta_sql_interpolate -------------------------------------
#' interpolate meta data in sql query
#'
#' @description This function just returns a new sql_query object after an interpolation of meta data
#'
#' @param sql_query a sql_query object that will be used for sqlinterpolation
#' @param meta_query_params A list of values for interpolation in the SQL file
#' it can also deal with parameter where the we need to build an IN value in sql
#'
#' @return query object
#'
#' @import DBI
#'
meta_sql_interpolate <- function(sql_query, meta_query_params) {
  # Loop over each item in the query_params list
  for (param in names(meta_query_params)) {
    # Check if the parameter value is a vector
    if (length(meta_query_params[[param]]) > 1) {
      # Create a string of values, with numeric values not in
      # parentheses and non-numeric values in parentheses
      values <- sapply(meta_query_params[[param]], function(x) {
        if (is.numeric(x)) {
          return(as.character(x))
        } else {
          return(paste0("'", as.character(x), "'"))
        }
      })
      # Join the values with commas
      replacement <- paste(values, collapse = ", ")
    } else {
      # If not a vector, use the parameter value as is
      replacement <- meta_query_params[[param]]
    }
    # Replace the placeholder in the query with the replacement string
    sql_query <- stringi::stri_replace_all_fixed(
      sql_query,
      stringi::stri_sprintf(
        format = "{%s}",
        param
      ),
      replacement,
      vectorize_all = FALSE
    )
  }
  # Return the interpolated query
  return(sql_query)
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
    query_params = list(),
    meta_query_params = list(),
    query_builder_params = list()
  ) {
  # build query: ----------------------------------
  if (length(query_builder_params) >= 1) {
    sql_query$sql_query <- generate_sql_statement(
      sql_query = sql_query$sql_query,
      meta_query_params = query_builder_params
    )
  }
  # if meta_sql_interopolate is available: ----------------------------------
  if (length(meta_query_params) >= 1) {
    sql_query$sql_query <- meta_sql_interpolate(
      sql_query = sql_query$sql_query,
      meta_query_params = meta_query_params
    )
  }

  # set Variables ------------------------------------------------------------
  if (length(query_params) >= 1) {
    sql_query$sql_query <- DBI::sqlInterpolate(
      conn = sql_conn,
      sql = sql_query$sql_query,
      .dots = query_params
    )
  }

  # convert to SQL class: ----------------------------------
  sql_query$sql_query <- SQL(sql_query$sql_query)

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
    sql_conn) {
  if (sql_query$method == "get") {
    value <- DBI::dbGetQuery(
      conn = sql_conn,
      statement = sql_query$sql_query
    )

    return(value)
  } else if (sql_query$method == "post") {
    value <- DBI::dbExecute(
      conn = sql_conn,
      statement = sql_query$sql_query
    )

    return(value)
  } else {
    stop("Please choose between 'get' and 'post' methods only")
  }
}
