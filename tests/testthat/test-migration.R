library(DBI)
skip_if_not_installed("RSQLite")

conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

describe("rs_migrate", {
  it("errors when migration folder does not exist", {
    expect_error(rs_migrate(conn, up = TRUE), "Migration folder not found")
  })

  it("runs sql files in a folder and returns a data frame", {
    tmp_dir <- file.path(tempdir(), "sql", "migrate", "up")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines(
      "CREATE TABLE IF NOT EXISTS migrate_test (id INTEGER);",
      file.path(tmp_dir, "001_create.sql")
    )

    withr::with_envvar(
      list(rs_migrate_folder = file.path(tempdir(), "sql", "migrate")),
      {
        result <- rs_migrate(conn, up = TRUE, default_method = "DBI::dbExecute")
        expect_s3_class(result, "data.frame")
        expect_equal(nrow(result), 1)
      }
    )

    DBI::dbExecute(conn, "DROP TABLE IF EXISTS migrate_test")
    unlink(tmp_dir, recursive = TRUE)
  })
})

DBI::dbDisconnect(conn)
