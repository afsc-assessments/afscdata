

#' utility function to connect to server
#' @param db the database schema ("akfin" or "afsc")
#' @export connect
#' 
connect <- function(db = "akfin") {

    # check if database name is stored in keyring, if not request user/pwd
  if(!(db %in% keyring::key_list()[,1])) {
    user <- getPass::getPass(paste("Enter", db, "username: "))
    pwd <- getPass::getPass(paste("Enter", db, "password: "))
  } else {
    user <- keyring::key_list(db)$username
    pwd <-  keyring::key_get(db, keyring::key_list(db)$username)
  }
    # connect to server
    DBI::dbConnect ( odbc::odbc(), 
                     driver = db,
                     uid = user,
                     pwd = pwd )
}

#' utility function to disconnect from server
#' @param db the database schema (e.g., akfin or afsc)
#' @export disconnect
#' 
disconnect <- function(db) {
  DBI::dbDisconnect(db)
}

#' utility function to read sql file
#' @param x the sql code to read, pulled from the top directory
#' @export sql_read
#' @examples 
#' \dontrun{
#' .d = sql_read("fsh_catch.sql")
#' }
sql_read <- function(x) {
  if(file.exists(system.file("sql", x, package = "afscdata"))) {
    readLines(system.file("sql", x, package = "afscdata"))
  } else {
    stop("The sql file does not exist.")
  }
}

#' utility function to filter sql files
#'
#' @param sql_precode change input e.g., ("=")
#' @param x the variable to change (e.g., year)
#' @param sql_code the sql query code...
#' @param flag a flag in the sql code to place the precode and x in the appropriate location
#' @export sql_filter
#' @examples
#' \dontrun{
#' .d = sql_filter(sql_precode = "<=", 2011, sql_code = .d, flag = "-- insert year")
#' }
sql_filter <- function(sql_precode = "IN", x, sql_code, flag = "-- insert species") {
  
  i = suppressWarnings(grep(flag, sql_code))
  sql_code[i] <- paste0(
    sql_precode, " (",
    collapse_filters(x), ")"
  )
  sql_code
}

#' utility function to run sql query
#'
#' @param database which database to connect to 'akfin' or 'afsc'
#' @param query the sql query code
#'
#' @export sql_run
#'
#' @examples
#' \dontrun{
#' .d = sql_read("fsh_catch.sql")
#' .d = sql_filter(sql_precode = "<=", 2011, sql_code = .d, flag = "-- insert year")
#' .d = sql_filter(x = area, sql_code = .d, flag = "-- insert region")
#' .d = sql_filter(sql_precode = "IN", x = c("PEL7", "PELS"), 
#'                    sql_code = .d, flag = "-- insert species")
#' 
#' afsc = DBI::dbConnect(odbc::odbc(), "afsc", UID = "afsc_user", PWD = "afsc_pwd") 
#'  
#' sql_run(afsc, query) %>%
#'          vroom::vroom_write(here::here(year, 'data', 'raw', 'fsh_catch_data.csv'))
#' DBI::dbDisconnect(afsc)
#' }
sql_run <- function(database, query) {
  query = paste(query, collapse = "\n")
  DBI::dbGetQuery(database, query, as.is=TRUE, believeNRows=FALSE)
}

#' utility function for sql queries
#'
#' @param x variable to add quotes to
#' @description adds correct quotes for sql queries, nested within `sql_filter`
#' @export collapse_filters

collapse_filters <- function(x) {
  sprintf("'%s'", paste(x, collapse = "','"))
}

#' utility function for date of data query
#'
#' @param year assessment year
#' @param loc location to save file if different from default
#' 
#' @return a query date file saved as `year/data/raw/data_called.txt`
#'
q_date <- function(year, loc = NULL){
  txt = "Data were downloaded on:"
  dt = format(Sys.time(), "%Y-%m-%d")
  
  if(is.null(loc)) {
    utils::write.table(c(txt, dt), file = here::here(year, "data", "raw", "data_called.txt"),
                sep = "\t", col.names = F, row.names = F)
  } else {
    utils::write.table(c(txt, dt), file = paste0(loc, "/data_called.txt"),
                sep = "\t", col.names = F, row.names = F)
  }
}

