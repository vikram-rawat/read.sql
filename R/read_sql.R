# read SQL files ----------------------------------------------------------
#' read a SQL file
#'
#' @description Creates an object of class \code{sql_query} by reading an SQL file or accepting a raw string.
#'              This object stores the SQL text and the execution method (e.g., "dbGetQuery").
#'
#' @param filepath Path to an SQL file containing a query.
#' @param sql_query_str A proper SQL statement string. If provided, this is used instead of reading a file.
#' @param method The name of the function to execute the query (e.g., "DBI::dbGetQuery", "read_adbc").
#'               Defaults to "dbGetQuery".
#'
#' @return An object of class \code{sql_query} containing the SQL text and execution method.
#'
#' @export
rs_read_query <- function(
  filepath,
  sql_query_str = "",
  method = "dbGetQuery"
) {
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
      method = method
    ),
    class = "sql_query"
  )

  return(sql_query)
}

# print sql query ----------------------------------------------------------
#' Prints a sql_query object
#'
#' @description This method provides a clean, formatted display of the SQL query and its intended execution method.
#'
#' @param x An object of class \code{sql_query}.
#' @param ... Additional arguments (not used; required for generic method consistency).
#'
#' @method print sql_query
#'
#' @return Invisible \code{NULL}. Prints output to the console.
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

# return just SQL text: ----------------------------------
#' return just SQL text
#' @description Extracts and returns the raw SQL text string from a \code{sql_query} object.
#' @param sql_query An object of class \code{sql_query}.
#'
#' @return A character string containing the SQL query.
#' @export
rs_get_sql_query <- function(sql_query) {
  return(as.character(sql_query$sql_query))
}

# generate_sql_statement -------------------------------------
#' generate_sql_statement
#'
#' @description Constructs a new SQL query string by programmatically
#'   adding a \code{WHERE} clause
#'   and applying basic SQL escaping to prevent injection.
#'
#' @param sql_query A base SQL query string (e.g., "SELECT * FROM my_table").
#'  It should typically not contain an existing \code{WHERE} clause.
#' @param param_ls A list of lists, where each inner list defines a condition:
#' \itemize{
#'   \item \strong{\code{col_name}}: The database column name.
#'   \item \strong{\code{operator}}: The SQL operator (e.g., "=", ">", "IN").
#'   \item \strong{\code{value}}: The value(s) for the comparison. Vectors are supported for "IN" clauses.
#'   \item \strong{\code{wrap}}: A boolean indicating whether to wrap the entire value string in parentheses
#'   (e.g., for use with \code{IN} operator).
#' }
#'
#' @return A character string containing the modified SQL statement with the added \code{WHERE} clause.
#'
#' @examples
#' base_sql <- "SELECT name, age FROM users"
#' params <- list(
#'   list(col_name = "name", operator = "=", value = "John", wrap = FALSE),
#'   list(col_name = "age", operator = ">", value = 30, wrap = FALSE),
#'   list(col_name = "status", operator = "IN", value = c("active", "pending"), wrap = TRUE)
#' )
#' \dontrun{
#' generate_sql_statement(base_sql, params)
#' }
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
#' @description Performs simple string replacement within the SQL query, replacing placeholders
#'  of the format \code{\{placeholder\}} with corresponding values from a list.
#'  This is useful for non-data parameters like table names, schema names, or structural SQL elements.
#'
#' @param sql_query A base SQL query string (or an object that can be coerced to one).
#' @param meta_query_params A named list where names correspond to the \code{\{placeholder\}}
#'  and values are the replacement strings. Supports vector values
#'  (e.g., for creating an "IN" list).
#'
#' @return A character string with all placeholders replaced.
#'
#' @import stringi
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
#' @description The main function for transforming a base \code{sql_query} object through a pipeline
#'  of query building, meta-interpolation, and secure SQL interpolation.
#'
#' @param sql_query An object of class \code{sql_query} used as the base for all transformations.
#' @param sql_conn A database connection object (e.g., from \code{DBI::dbConnect} or a connection pool).
#'  This is required for \code{DBI::sqlInterpolate} to correctly escape parameters.
#' @param query_params A named list of values for secure parameter substitution (e.g., replacing \code{?} or named placeholders).
#'  These values are passed to \code{DBI::sqlInterpolate}.
#' @param meta_query_params A named list of values for simple string replacement of meta-data placeholders (e.g., \code{\{table_name\}}).
#' @param query_builder_params A list of lists defining \code{WHERE} clauses to be programmatically added to the query
#'  (see \code{\link{generate_sql_statement}} for structure).
#' example:
#'
#' params <- list(
#'   list(col_name = "name", operator = "=", value = "John", wrap = FALSE),
#'   list(col_name = "age", operator = ">", value = 30, wrap = FALSE),
#'   list(col_name = "status", operator = "IN", value = c("active", "pending"), wrap = TRUE)
#' )
#'
#' @return A new \code{sql_query} object with the final, transformed SQL statement.
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
#' execute a SQL query (Final Scalable Design)
#'
#' @description The core execution engine. It uses the method stored in the \code{sql_query} object
#'              to run the query against a database. It automatically adjusts argument names for
#'              DBI and ADBC methods. For custom methods, the user must supply the connection
#'              object and necessary arguments via \code{...}.
#'
#' @param sql_query An object of class \code{sql_query} containing the method and final SQL string.
#' @param sql_conn The primary connection object or database wrapper object.
#' @param stmt_arg_name The name of the statement/query argument for custom functions
#'                      (e.g., if a custom function is \code{my_exec(conn, query)}, this defaults to "statement").
#' @param ... Additional arguments to be passed directly to the executing function.
#'            \strong{Note}: For custom methods, the connection object itself must be passed via \code{...}.
#'
#' @return The result of the executed function (e.g., a data frame, integer count, or connection handle).
#'
#' @export
rs_execute <- function(
  sql_query,
  sql_conn,
  stmt_arg_name = "statement", # Only statement arg is manually exposed
  ...
) {
  exec_method_str <- sql_query$method
  exec_sql <- rs_get_sql_query(sql_query)
  method_base <- tolower(sub(".*::", "", exec_method_str))

  # Initialize the argument list with arguments passed through ...
  args_list <- list(...)

  # --- 1. Pattern Detection & Argument Construction ---
  if (method_base %in% c("dbgetquery", "dbexecute")) {
    # DBI Standard: Arguments are (conn, statement, ...)
    conn_name <- "conn"
    stmt_name <- "statement"

    # Add the connection and statement explicitly
    args_list[[conn_name]] <- sql_conn
    args_list[[stmt_name]] <- exec_sql
  } else if (method_base %in% c("read_adbc", "execute_adbc")) {
    # ADBC Read Standard: Arguments are (db_or_con, query, ...)
    conn_name <- "db_or_con"
    stmt_name <- "query"

    # Add the connection and statement explicitly
    args_list[[conn_name]] <- sql_conn
    args_list[[stmt_name]] <- exec_sql
  } else {
    # Statement name is provided by the user (or defaults to "statement")
    stmt_name <- stmt_arg_name

    # Add the statement explicitly
    args_list[[stmt_name]] <- exec_sql
  }

  # --- 3. Execute ---
  value <- do.call(
    what = exec_method_str,
    args = args_list
  )

  return(value)
}
