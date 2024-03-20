# DB migration ----------------------------------------------------------
#' This function runs migration scripts
#'
#' @description This function runs all the files in the folder sql/migrate/up or sql/migrate/down.
#' If you want to change the folder set the environment variable **rs_migrate_folder** to a different folder name.
#'
#' @param sql_conn a sql connection on which all the queries will be executed
#' @param up a boolean value to determine if you need to run files from the folder `up` or `down`
#' remember these are the only 2 options available.
#' @param file_names a vector of all the files that need to be processed in a single go
#' @param default_method a choice between getQuery and sendstatement only 2 options available `get` and `post`. This option will be used for all the files in the list.
#'
#' @return a data table with the list of filenames and the rows changed after appending the value
#'
#' @export

rs_migrate <- function(
    sql_conn,
    up = TRUE,
    file_names = NULL,
    default_method = "post") {
  if (length(file_names) == 0) {
    folder_path <- Sys.getenv("rs_migrate_folder")

    if (folder_path == "") {
      folder_path <- "sql/migrate/"
    }

    if (up) {
      folder_path <- paste0(folder_path, "up")
    } else {
      folder_path <- paste0(folder_path, "down")
    }

    file_names <- sort(
      list.files(
        path = folder_path,
        full.names = TRUE
      )
    )
  }


  if (length(file_names) == 0) {
    print(folder_path, "--  is an empty folder")
  }

  value <- character(length(file_names))

  for (i in seq_along(file_names)) {
    file_name <- file_names[[i]]

    sql_query <- rs_read_query(
      filepath = file_name,
      method = tolower(default_method)
    )

    value[[i]] <- tryCatch(
      expr = {
        rs_execute(
          sql_query = sql_query,
          sql_conn = sql_conn
        )
      },
      error = function(e) {
        cat(
          sprintf(
            fmt = "There occurred an error in %s \n Error is ----- \n %s",
            file_name,
            e
          )
        )

        return(as.character(e))
      }
    )
  }

  return_value <- data.frame(
    file_names = file_names,
    execute = value
  )

  return(return_value)
}
