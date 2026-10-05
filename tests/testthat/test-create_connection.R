library(DBI)
skip_if_not_installed("RSQLite")

describe("rs_create_conn", {
  it("creates a DBI connection from a param list", {
    test_conn <- rs_create_conn(
      driver     = RSQLite::SQLite(),
      param_list = list(dbname = ":memory:")
    )
    expect_true(DBI::dbIsValid(test_conn))
    DBI::dbDisconnect(test_conn)
  })

  it("errors when no driver is provided and param_list has no drv", {
    expect_error(
      rs_create_conn(param_list = list(dbname = ":memory:")),
      "Driver"
    )
  })

  it("errors when pool = TRUE but pool package is not available", {
    # only runs when pool is absent; skips otherwise
    skip_if(requireNamespace("pool", quietly = TRUE), "pool is installed")
    expect_error(
      rs_create_conn(
        driver     = RSQLite::SQLite(),
        param_list = list(dbname = ":memory:"),
        pool       = TRUE
      ),
      "pool"
    )
  })
})
