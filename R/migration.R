# DB migration ----------------------------------------------------------
# DB migration ----------------------------------------------------------
#' Run Database Migration Scripts
#'
#' @description This function runs all SQL migration files found in the specified directory
#'  (defaulting to \code{sql/migrate/up} or \code{sql/migrate/down}).
#'  It uses the \code{rs_read_query} and \code{rs_execute} pipeline and will
#'  \strong{stop immediately} upon encountering the first error.
#'
#' @param sql_conn A database connection object or a custom database wrapper object
#'  (compatible with \code{rs_execute}).
#' @param up A boolean value. If \code{TRUE} (default), files are run from the \code{up} directory.
#'  If \code{FALSE}, files are run from the \code{down} directory.
#' @param file_names An optional character vector of full file paths to process.
#'  If \code{NULL} (default), files are auto-discovered from the folder path.
#' @param default_method The full function string used to execute the SQL.
#'  This must be a function
#'  that accepts the connection and SQL statement
#' (e.g., \code{"DBI::dbExecute"} or \code{"adbc::execute_adbc"}).
#'  Defaults to \code{"DBI::dbExecute"}.
#'
#' @details The base migration folder can be changed by
#'  setting the environment variable
#' \code{rs_migrate_folder}. The function sorts discovered files alphabetically
#'  to enforce a consistent migration order.
#'
#' @return A data frame with two columns: \code{file_names}
#' (the executed script names)
#'  and \code{execute} (the result of the execution,
#'  typically the number of rows affected).
#'
#' @export
rs_migrate <- function(
  sql_conn,
  up = TRUE,
  file_names = NULL,
  default_method = "DBI::dbExecute"
) {
  # Determine files if file_names is empty (Logic remains the same)
  if (length(file_names) == 0) {
    folder_path <- Sys.getenv("rs_migrate_folder")

    if (folder_path == "") {
      folder_path <- "sql/migrate/"
    }

    folder_dir <- if (up) {
      "up"
    } else {
      "down"
    }
    folder_path <- file.path(folder_path, folder_dir)

    if (!dir.exists(folder_path)) {
      stop("Migration folder not found: ", folder_path)
    }

    file_names <- sort(
      list.files(
        path = folder_path,
        pattern = "\\.sql$",
        full.names = TRUE
      )
    )
  }

  if (length(file_names) == 0) {
    message("Migration file list is empty: ", folder_path)
    return(
      data.frame(
        file_names = character(0),
        execute = character(0)
      )
    )
  }

  # Initialize return values
  value <- character(length(file_names))

  # 1. WRAP THE ENTIRE MIGRATION IN A TRANSACTION
  final_result <- DBI::dbWithTransaction(
    sql_conn,
    {
      message("Starting migration of ", length(file_names), " file(s) inside a transaction...")

      # Loop and Execute (Stopping on Error)
      for (i in seq_along(file_names)) {
        file_name <- file_names[[i]]

        sql_query <- rs_read_query(
          filepath = file_name,
          method = default_method
        )

        message("Executing: ", file_name)

        # 2. SIMPLIFIED ERROR HANDLING: dbWithTransaction handles the rollback
        value[[i]] <- tryCatch(
          expr = {
            rs_execute(
              sql_query = sql_query,
              sql_conn = sql_conn
            )
          },
          error = function(e) {
            # If an error occurs, this 'stop()' triggers the automatic rollback
            # by dbWithTransaction, which is the desired behavior.
            stop(
              sprintf(
                "Migration failed in %s. Transaction rolled back. Error: %s",
                file_name,
                e$message
              ),
              call. = FALSE
            )
          }
        )
      }

      # 3. Automatic Commit: dbWithTransaction automatically commits if no error occurs.
      message("Transaction successfully committed.")

      # Return the results data frame from the block
      data.frame(
        file_names = file_names,
        execute = value
      )
    }
  )

  # Return the result data frame
  return(final_result)
}
