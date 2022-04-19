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
#' @param ... Additional arguments to be passed to pool::dbPool function
#' @return pool object
#'
#' @import pool
#'
#' @export
rs_create_pool <- function(
  driver = NULL,
  param_list
){

  if(is.null(param_list$drv)){

    if(missing(driver)){

      stop("Please provide a valid Driver")

    } else {

      param_list <- append(param_list, c(drv = driver))

    }

  } else if( is.character(param_list$drv)) {

    if(missing(driver)){

      stop("Please provide a valid Driver")

    } else {

      param_list$drv <- NULL

      param_list <- append(param_list, c(drv = driver))

    }

  }

  pool <- do.call(pool::dbPool, param_list)

  return(pool)

}

# open a DB conn ----------------------------------------------------------
#' create a DB Conn
#'
#' @description This function just returns a db conn object by passing a config file data to it. You may optionally pass all the parameters as list too.
#'
#' @param param_list A list of values for creating a pool connection. Would actually prefer passing a config file data directly.
#' @param driver A custom driver to connect to database if the need arises
#' @param ... Additional arguments to be passed to DBI::dbConnect function
#'
#' @return db connection object
#'
#' @import DBI
#'
#' @export
rs_create_conn <- function(
  driver = NULL,
  param_list
  ){

  if(is.null(param_list$drv)){

    if(missing(driver)){

      stop("Please provide a valid Driver")

    } else {

      param_list <- append(param_list, c(drv = driver))

    }

  } else if( is.character(param_list$drv)) {

    if(missing(driver)){

      stop("Please provide a valid Driver")

    } else {

      param_list$drv <- NULL

      param_list <- append(param_list, c(drv = driver))

    }

  }

  conn <- do.call(DBI::dbConnect, param_list)

  return(conn)

}

