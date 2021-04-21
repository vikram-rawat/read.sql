
<!-- README.md is generated from README.Rmd. Please edit that file -->

# read.sql

<!-- badges: start -->
<!-- badges: end -->

I believe SQL should be written in a seperate file that can be modified
later easily instead of a string variable that we all are used to This
package consists of 4 very simple functions that I have been using in my
projects for very long and I see myself rewriting them again and again.
This package doesn’t have any dependencies. It just have 4 functions
that you can copy and modify if you like. I would recommend to use it
whenever you have SQL files in your project.

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
        driver: Postgres
        server: localhost
        uid: postgres
        pwd: postgres
        port: 5432
        database: chatbot
      

Then you can read it with config package. and I have 2 functions to
create either a connection or a pool directly from these functions.

    dw <- config::get("datawarehouse")

    conn <- read.sql::create_conn(
      driver = RPostgres::Postgres(),
      param_list = dw
    )

    pool <- read.sql::create_pool(
      driver = RPostgres::Postgres(),
      param_list = dw
    )

These functions come in handy when you want to restablish a connection.
So I preferred to use them in the file.

Then there are only 2 functions that remain

``` r
conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

DBI::dbWriteTable(conn,"iris", iris)
```

Always write you SQL in a seperate file where you can edit the code for
later use or you can give it somebody who can optimize the code. An
external file helps you write and maintain large SQL code

### get\_sql\_query

``` sql
select
  * 
from 
  iris 
limit 
  5
```

<div class="knitsql-table">

| Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species |
|-------------:|------------:|-------------:|------------:|:--------|
|          5.1 |         3.5 |          1.4 |         0.2 | setosa  |
|          4.9 |         3.0 |          1.4 |         0.2 | setosa  |
|          4.7 |         3.2 |          1.3 |         0.2 | setosa  |
|          4.6 |         3.1 |          1.5 |         0.2 | setosa  |
|          5.0 |         3.6 |          1.4 |         0.2 | setosa  |

5 records

</div>

In this case you can use the function like this

    query <- read.sql::get_sql_query(
      sql_conn = conn,
      sql_file_path = "path/to/sql/file")

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

<div class="knitsql-table">

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

</div>

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

    query <- read.sql::get_sql_query(
      sql_conn = conn,
      sql_file_path = "path/to/sql/file",
      query_params = list(
        minsepal = 5,
        minpetal = 5
      )
    )

query\_params is an optional aruement. You would only need it when you
want to interpolate a value in the SQL file you need.

### execute\_sql\_file

most common function you will use most of the time is this. It is just a
wrapper on get\_sql\_query but it also executes the query and brings
back the result.

    read.sql::execute_sql_file(
      sql_conn = conn,
      sql_file_path = "path/to/sql/file",
      query_params = list(
        minsepal = 5,
        minpetal = 5
      ),
      method = "get"
    )

The only thing different about it is that it has a method arguement
where if you need results from the DB you should use `get` all lower
case. If you want to execute a delete, or update statement you should
use `post` all lower case.

### warning

Package doesn’t assume anything and it does no checking at all. It is
meant to be used with existing architecture where you will write all the
logic on top of it. So if there is anything wrong it will simply crash.
