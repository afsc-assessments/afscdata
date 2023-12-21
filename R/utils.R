

#' utility function to connect to server
#' @param db the database schema ("akfin" or "afsc")
#' @export connect
#' @export catch_to_ss
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
                     db,
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

#' Setup folder structure
#'
#' Creates a common folder structure for assessment data
#'
#' @param year assessment year
#' @param dirs directories to write
#' @param tier assessment tier to change the folders used - not currently implemented
#' @return creates a designated/named folder structure 
#' @export setup_folders
#'
#' @examples
#' \dontrun{
#' setup(2022)
#'}
setup_folders <- function(year, dirs = c("raw", "user_input", "output", "sara", "sql"), tier = NULL){
  

    for(i in 1:length(dirs)){
      if(dir.exists(here::here(year, "data", dirs[i])) == FALSE){
        dir.create(here::here(year, "data", dirs[i]), recursive=TRUE)
      }
    }
    
}


#' utility function for date of data query
#'
#' @param year assessment year
#' @param loc location to save file if different from default
#' 
#' @return a query date file saved as `year/data/raw/data_called.txt`
#' @export q_date
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

#' copy the previous SSC accepted model as the new "base" model
#'
#' @description copy of all base model files, adds a "README.md" file that identifies the model used (or appends the current README), predicated on using the 'afscdata::setup_folders()' function, though has an option for other folder structures. Note, this function copies everything over (e.g., retrospectives) and may take a few minutes to complete
#' @param base_year year of the base model
#' @param base_model name of the base model (folder name)
#' @param year current year
#' @param folder if the base model is in a different location than the afscdata structure is looking for
#'
#' @return a 'base' model folder with all of the prior years inputs/outputs
#' @export accepted_model
#'
#' @examples
#' \dontrun{
#' accepted_model(base_year = 2020, base_model = "m18.2b", year = 2021)
#' }
accepted_model <- function(base_year, base_model, year, folder = NULL){
  
  
  x0 = dir.exists(here::here(base_year, base_model))
  
  if(!x0 & is.null(folder)){
    stop("incorrect base model location: maybe provide a file?")
  }
  
  x = dir.exists(here::here(year, "base"))
  y = NA
  
  if (!x) {
    dir.create(here::here(year, "base"))
  } else {
    y = readline("To overwrite the base folder enter 1, else 0: ")
  }
  
  if(y!=1 & !is.na(y)){
    stop("something has to give, maybe create a new folder?")
  }
  
  if(y==1 & is.null(folder) | is.na(y) & is.null(folder)){
    
    file.copy(list.files(here::here(base_year, base_model), full.names = TRUE),
              here::here(year, "base"),
              recursive = TRUE, overwrite = TRUE)
    
    utils::write.table(c(paste0("Using SSC accepted model ", base_model, " from ", base_year, " as the base model for ", year, ".")), 
                       file = here::here(year, "README.md"),
                       sep = "\t", col.names = F, row.names = F, append=TRUE)
    
    
  } else if(y==1 & !is.null(folder) | is.na(y) & !is.null(folder)){
    file.copy(list.files(folder, full.names = TRUE),
              here::here(year, "base"),
              recursive = TRUE, overwrite = TRUE)
    
    utils::write.table(c(paste0("Using SSC accepted model ", base_model, " from ", base_year, " as the base model for ", year, ".")), 
                       file = here::here(year, "README.md"),
                       sep = "\t", col.names = F, row.names = F, append=TRUE)
  } else {
    cat("folder not overwritten.\n")
  }
  
  
}


catch_to_ss <- function(year, se = 0.01, season = 7, fleet=1){
  vroom::vroom(here::here(year,'data','output','fsh_catch.csv')) %>%
    mutate(seas = season, fleet, catch, catch_se = se) %>%
    select(year, seas, fleet, catch, catch_se) %>%
    vroom::vroom_write(here::here(year, "data", "output", "fsh_catch_ss3.csv"), delim = ",")
}