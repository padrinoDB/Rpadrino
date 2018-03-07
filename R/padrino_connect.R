#' @importFrom DBI dbDriver
#' @importFrom RPostgreSQL dbConnect dbDisconnect
#' @noRd

padrino_connect <- function() {
  drv <- DBI::dbDriver("PostgreSQL")
  con <- RPostgreSQL::dbConnect(drv,
                                dbname = "Padrino",
                                host = "localhost",
                                port = 5432,
                                user = "postgres",
                                password = "postgres")

  return(con)
}

padrino_disconnect <- function(connection) {
  RPostgreSQL::dbDisconnect(conn = connection)
}
