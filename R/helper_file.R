generate_sql_statement <- function(sql_query, param_ls) {
  sql_query <- sprintf(
    fmt = "%s \n WHERE 1 = 1 \n", # Base query,
    sql_query
  )
 
  lapply(
    param_ls,
    function(x) {
      if (isTruthy(x$value)) {
        if (x$wrap) {
          in_values <- sprintf(
            fmt = "('%s')",
            paste(x$value, collapse = "','")
          )
        } else {
          in_values <- x$value
        }
 
        sql_query <<- sprintf(
          fmt = "%s AND \n    %s %s %s \n",
          sql_query,
          x$col_name,
          x$operator,
          in_values
        )
      }
    }
  )
 
  print("----------------")
  cat(sql_query)
  print("----------------")
 
  return(sql_query)
}
