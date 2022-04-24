
<!-- README.md is generated from README.Rmd. Please edit that file -->

# read.sql

<!-- <img src='man/figures/' align="right" height="131.5" /></a> -->
<!-- badges: start -->
<!-- badges: end -->

I believe SQL should be written in a seperate file that can be modified
later easily instead of a string variable that we all are used to. It
also helps you add a proper DB admin into the team who can modify
queries without ever learning R. This package consists of few very
simple functions that I have been using in my projects for very long and
I see myself rewriting them again and again. This package doesn’t have
any dependencies that is not useful while talking to a DB. I would
recommend to use it whenever you have SQL files in your project.

## Installation

It’s not on cran yet. The development version can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("vikram-rawat/read_sql_files")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(read.sql)
## basic example code
```

Always start with creating a config file for keeping all your basic
environment variables in a file, specially db configurations. It looks
something like this

    default:

      datawarehouse:
        server: localhost
        uid: postgres
        pwd: postgres
        port: 5432
        database: chatbot

Then you can read it with config package. make sure you don’t add a drv
name in the YML file and also avoid using the password directly into Yml
file. Use environment variable instead. read.sql have 1 function to
create either a connection or a pool directly from this list.

    dw <- config::get("datawarehouse")

    conn <- read.sql::rs_create_conn(
      driver = RPostgres::Postgres(),
      param_list = dw
    )

    pool <- read.sql::rs_create_conn(
      driver = RPostgres::Postgres(),
      param_list = dw,
      pool = TRUE
    )

These functions come in handy when you want to re-establish a
connection. So I preferred to use them in the file.

Then there are only 3 functions that remain

``` r
conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

DBI::dbWriteTable(conn,"iris", iris)
```

Always write your SQL code in a separate file where you can edit the
code for later use or you can give it somebody who can optimize the
code. An external file helps you write and maintain large SQL code. Now
imagine you have a code like this.

### get_sql_query

``` sql
select
  * 
from 
  iris 
limit 
  5
```

| Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species |
|-------------:|------------:|-------------:|------------:|:--------|
|          5.1 |         3.5 |          1.4 |         0.2 | setosa  |
|          4.9 |         3.0 |          1.4 |         0.2 | setosa  |
|          4.7 |         3.2 |          1.3 |         0.2 | setosa  |
|          4.6 |         3.1 |          1.5 |         0.2 | setosa  |
|          5.0 |         3.6 |          1.4 |         0.2 | setosa  |

5 records

In this case you can use the function like this

    query <- read.sql::rs_read_query(filepath = "path/to/sql/file")

Now suppose you have a query like this.

``` sql
select 
  * 
from 
  iris 
where 
  `Sepal.Length` > 5   
  and 
  `Petal.Length` < 1.7
```

| Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species |
|-------------:|------------:|-------------:|------------:|:--------|
|          5.1 |         3.5 |          1.4 |         0.2 | setosa  |
|          5.4 |         3.7 |          1.5 |         0.2 | setosa  |
|          5.8 |         4.0 |          1.2 |         0.2 | setosa  |
|          5.7 |         4.4 |          1.5 |         0.4 | setosa  |
|          5.4 |         3.9 |          1.3 |         0.4 | setosa  |
|          5.1 |         3.5 |          1.4 |         0.3 | setosa  |
|          5.1 |         3.8 |          1.5 |         0.3 | setosa  |
|          5.1 |         3.7 |          1.5 |         0.4 | setosa  |
|          5.2 |         3.5 |          1.5 |         0.2 | setosa  |
|          5.2 |         3.4 |          1.4 |         0.2 | setosa  |

Displaying records 1 - 10

what if you want to make this query reuse able and use multiple
parameters. You could use SQL interpolations like this.

    select 
      * 
    from 
      iris 
    where 
      `Sepal.Length` > ?minsepal   
      and 
      `Petal.Length` < ?minpetal
      

and then you could use it in the function as this.

    query <- read.sql::rs_interpolate(
      sql_query = sql_query_object, # object created from rs_read_query function
      sql_conn = conn,
      query_params = list(
        minsepal = 5,
        minpetal = 5
      )
    )

query_params is an optional argument. You would only need it when you
want to interpolate a value in the SQL file you need.

### execute_sql_file

most common function you will use most of the time is this. This will
execute the query that is read from the file or modified by function
rs_interpolate.

    read.sql::rs_execute(
      sql_conn = conn,
      query_params = list(
        minsepal = 5,
        minpetal = 5
      )
    )

The only thing different about it is that it has a method argument where
if you need results from the DB you should use `get` all lower case. If
you want to execute a delete, or update statement you should use `post`
all lower case.

### migration

This package also has a migration function. That runs files saved in the
folder `sql/migrate/up` or `sql/migrate/down` depending on the boolean
value of up arguement.

    conn |> 
      rs_migrate()

This will help you set up a DB again and again anytime you need it.

### warning

Package doesn’t assume anything and it does no checking at all. It is
meant to be used with existing architecture where you will write all the
logic on top of it. So if there is anything wrong it will simply crash.

Please read the documentation of each function to understand different
arguments used in them.
