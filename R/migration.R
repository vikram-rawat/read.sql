# DB migration ----------------------------------------------------------
#' This function runs migration scripts
#'
#' @description This function runs all the files in the folder sql/migrate/up or sql/migrate/down.
#' If you want to change the folder set the environment variable **rs_migrate_folder** to a different folder name.
#'
#' @param sql_conn a sql connection on which all the queries will be executed
#' @param up a boolean value to determine if you need to run files from the folder `up` or `down`
#' remember these are the only 2 options available.
#'
#' @return a data table with the list of filenames and the rows changed after appending the value
#'
#' @export

rs_migrate <- function(
  sql_conn,
  up = TRUE
){

  folder_path <- Sys.getenv("rs_migrate_folder")

  if( folder_path == "" ){

    folder_path <- "sql/migrate/"

  }

  if(up){

    folder_path <- paste0(folder_path, "up")

  } else {

    folder_path <- paste0(folder_path, "down")

  }

  file_names <- list.files(
    path = folder_path,
    full.names = TRUE
  )

  if(length(file_names) == 0 ){
    print(folder_path, "--  is an empty folder")
  }

  value <- character(length(file_names))

  for( i in seq_along(file_names)){

    file_name <- file_names[[i]]

    sql_query <- rs_read_query(
      filepath = file_name,
      method = "post"
    )

    value[[i]] <- tryCatch(
      expr = {
        rs_execute(
          sql_query = sql_query,
          sql_conn = sql_conn
        )
      },
      error = function(e){
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

  return_value <- data.table::data.table(
    file_names = file_names,
    execute = value
  )

  return(return_value)

}