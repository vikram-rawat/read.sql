# open a DB conn ----------------------------------------------------------
#' create a DB Conn
#'
#' @description This function just returns a db conn object by passing a config file data to it. You may optionally pass all the parameters as list too.
#'
#' @param driver A custom driver to connect to database if the need arises
#' @param param_list A list of values for creating a pool connection. Would actually prefer passing a config file data directly.
#' @param pool A boolean value to decide if you need a normal `DBI::dbConnect` connection or a pool connection
#' through `pool::dbPool`
#'
#' @return db connection object
#'
#' @import DBI
#'
#' @export
rs_create_conn <- function(
    driver = NULL,
    param_list,
    pool = FALSE) {
  if (is.null(param_list$drv)) {
    if (missing(driver)) {
      stop("Please provide a valid Driver")
    } else {
      param_list <- append(param_list, c(drv = driver))
    }
  } else if (is.character(param_list$drv)) {
    if (missing(driver)) {
      stop("Please provide a valid Driver")
    } else {
      param_list$drv <- NULL

      param_list <- append(param_list, c(drv = driver))
    }
  }


  if (pool) {
    conn <- do.call(pool::dbPool, param_list)
  } else {
    conn <- do.call(DBI::dbConnect, param_list)
  }


  return(conn)
}
