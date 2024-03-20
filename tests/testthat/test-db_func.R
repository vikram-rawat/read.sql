# load library ------------------------------------------------------------

library(DBI)
library(read.sql)

# establish a connection --------------------------------------------------

conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# insert data -------------------------------------------------------------

DBI::dbWriteTable(conn, "iris", iris, overwrite = TRUE)

range(iris$Sepal.Length)
range(iris$Petal.Length)

# check functions ---------------------------------------------------------

sql_query_object <- rs_read_query(
  "tests/sql/simplq_sql_interpolation.sql"
)

query_obj <- read.sql::rs_interpolate(
  sql_query = sql_query_object, # object created from rs_read_query function
  sql_conn = conn,
  meta_query_params = list(
    main_table = "iris",
    column1 = "`Sepal.Length`",
    column2 = "`Petal.Length`",
    column3 = "`Species`",
    species_value = c("Setosa", "versicolor"),
    width_value = seq(1.0, 1.4, 0.1)
  ),
  query_params = list(
    mincol1 = 4,
    mincol2 = 4
  )
)

query_obj |>
  read.sql::rs_execute(
    sql_conn = conn
  )
