library(DBI)
skip_if_not_installed("RSQLite")

conn     <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
sql_file <- testthat::test_path("../sql/simplq_sql_interpolation.sql")

DBI::dbWriteTable(conn, "iris", iris, overwrite = TRUE)

describe("rs_read_query", {
  it("reads a sql file and returns a sql_query object", {
    obj <- rs_read_query(sql_file)
    expect_s3_class(obj, "sql_query")
    expect_true(nchar(rs_get_sql_query(obj)) > 0)
  })

  it("accepts a raw string instead of a file path", {
    obj <- rs_read_query(sql_query_str = "SELECT 1")
    expect_s3_class(obj, "sql_query")
    expect_equal(rs_get_sql_query(obj), "SELECT 1")
  })
})

describe("rs_get_sql_query", {
  it("returns the sql text as a plain character string", {
    expect_equal(rs_get_sql_query(rs_read_query(sql_query_str = "SELECT 42")), "SELECT 42")
  })
})

describe("print.sql_query", {
  it("prints the sql text to console", {
    obj <- rs_read_query(sql_query_str = "SELECT 1")
    expect_output(print(obj), "SELECT 1")
  })
})

describe("meta_sql_interpolate", {
  it("replaces a scalar {placeholder}", {
    result <- read.sql:::meta_sql_interpolate("SELECT * FROM {tbl}", list(tbl = "iris"))
    expect_equal(result, "SELECT * FROM iris")
  })

  it("replaces a vector {placeholder} as quoted csv for IN clauses", {
    # character vectors are quoted; numeric vectors are unquoted
    result <- read.sql:::meta_sql_interpolate(
      "WHERE species IN ({sp})",
      list(sp = c("setosa", "versicolor"))
    )
    expect_match(result, "'setosa'")
    expect_match(result, "'versicolor'")
  })
})

describe("generate_sql_statement", {
  it("appends a WHERE clause with conditions", {
    sql <- generate_sql_statement(
      "SELECT * FROM iris",
      list(
        list(col_name = "Species", operator = "=",  value = "setosa",                    wrap = FALSE),
        list(col_name = "Species", operator = "IN", value = c("setosa", "versicolor"),   wrap = TRUE),
        list(col_name = "Age",     operator = "=",  value = NULL,                        wrap = FALSE)
      )
    )
    expect_match(sql, "WHERE")
    expect_match(sql, "IN")
    # NULL value conditions must be silently skipped
    expect_no_match(sql, "Age")
  })
})

describe("rs_interpolate", {
  it("resolves all meta and DBI placeholders", {
    result <- rs_interpolate(
      sql_query = rs_read_query(sql_file),
      sql_conn  = conn,
      meta_query_params = list(
        main_table    = "iris",
        column1       = "`Sepal.Length`",
        column2       = "`Petal.Length`",
        column3       = "`Species`",
        species_value = c("setosa", "versicolor"),
        width_value   = seq(1.0, 1.4, 0.1)
      ),
      query_params = list(mincol1 = 4, mincol2 = 4)
    )
    expect_s3_class(result, "sql_query")
    expect_no_match(rs_get_sql_query(result), "\\{")
    expect_no_match(rs_get_sql_query(result), "\\?")
  })
})

describe("rs_execute", {
  it("executes a SELECT and returns a data frame", {
    obj    <- rs_read_query(sql_query_str = "SELECT * FROM iris LIMIT 5", method = "DBI::dbGetQuery")
    result <- rs_execute(obj, sql_conn = conn)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 5)
  })

  it("replace_exec_method overrides the stored method", {
    obj <- rs_read_query(
      sql_query_str = "CREATE TABLE IF NOT EXISTS tmp_test (id INTEGER)",
      method        = "DBI::dbGetQuery"
    )
    expect_no_error(
      rs_execute(obj, sql_conn = conn, replace_exec_method = "DBI::dbExecute")
    )
    DBI::dbExecute(conn, "DROP TABLE IF EXISTS tmp_test")
  })
})

DBI::dbDisconnect(conn)
