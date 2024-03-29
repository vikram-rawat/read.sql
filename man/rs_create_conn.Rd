% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_connection.R
\name{rs_create_conn}
\alias{rs_create_conn}
\title{create a DB Conn}
\usage{
rs_create_conn(driver = NULL, param_list, pool = FALSE)
}
\arguments{
\item{driver}{A custom driver to connect to database if the need arises}

\item{param_list}{A list of values for creating a pool connection. Would actually prefer passing a config file data directly.}

\item{pool}{A boolean value to decide if you need a normal \code{DBI::dbConnect} connection or a pool connection
through \code{pool::dbPool}}
}
\value{
db connection object
}
\description{
This function just returns a db conn object by passing a config file data to it. You may optionally pass all the parameters as list too.
}
