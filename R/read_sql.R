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
#' @param x an Object of type sql_query
#'
#' @method print sql_query
#'
#' @return character string
#'
#' @export
print.sql_query <- function(x, ...) {
  cat(
    sprintf(
      fmt = "\n %s ==> \n--------------------- \n%s--------------------- \n",
      x$method,
      x$sql_query
    )
  )
}

# generate_sql_statement -------------------------------------
#' generate_sql_statement
#'
#' @description This function returns a new SQL query object after adding multiple 
#' WHERE clauses in the provided SQL statement.
#'
#' @param sql_query A SQL query object that will be used as a base for SQL. 
#' This SQL statement shouldn't have a WHERE clause; that WHERE clause will be added by the function.
#' @param param_ls A list of values for adding WHERE clauses. Each param should consist of a list 
#' of 4 values: col_name, operator, value, and wrap. The wrap parameter is a boolean indicating 
#' whether to wrap the value in parentheses (useful for IN clauses).
#'
#' @return query object
#'
#' @examples
#' sql_query <- "SELECT * FROM my_table"
#' params <- list(
#'   list(col_name = "name", operator = "=", value = "John", wrap = FALSE),
#'   list(col_name = "age", operator = ">", value = 30, wrap = FALSE),
#'   list(col_name = "status", operator = "IN", value = c("active", "pending"), wrap = TRUE)
#' )
#' generate_sql_statement(sql_query, params)
#'
#' @export 
generate_sql_statement <- function(sql_query, param_ls) {
  # Start with a base query
  sql_query <- sprintf(
    fmt = "%s \n WHERE 1 = 1 \n",
    sql_query
  )

  # Function to escape special characters to prevent SQL injection
  escape_sql <- function(value) {
    if (is.character(value)) {
      return(gsub("'", "''", value))
    }
    return(value)
  }

  lapply(
    param_ls,
    function(x) {
      if (!is.null(x$value) && length(x$value) > 0) {
        if (length(x$value) > 1 && isTRUE(x$wrap)) {
          in_values <- sprintf(
            fmt = "('%s')",
            paste(sapply(x$value, escape_sql), collapse = "','")
          )
          sql_query <<- sprintf(
            fmt = "%s AND \n    %s %s %s \n",
            sql_query,
            x$col_name,
            x$operator,
            in_values
          )
        } else {
          value <- escape_sql(x$value)
          if (is.character(value) && !isTRUE(x$wrap)) {
            value <- sprintf("'%s'", value)
          }
          sql_query <<- sprintf(
            fmt = "%s AND \n    %s %s %s \n",
            sql_query,
            x$col_name,
            x$operator,
            value
          )
        }
      }
    }
  )

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
#' @import DBI stringi
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
#' @param query_params A list of values for interpolation in the SQL file with SQL specifications like ?min_value etc.
#' @param meta_query_params A list of values for adding values in the SQL file like normal string syntax like \{min_value\} etc
#' @param query_builder_params A list of list of values for create and adding a where clause in in the SQL file 
#' example: 
#' 
#' params <- list(
#'   list(col_name = "name", operator = "=", value = "John", wrap = FALSE),
#'   list(col_name = "age", operator = ">", value = 30, wrap = FALSE),
#'   list(col_name = "status", operator = "IN", value = c("active", "pending"), wrap = TRUE)
#' )
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
      param_ls = query_builder_params
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
