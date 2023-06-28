# bottom trawl survey ----
#' query bottom trawl survey length data from the AFSC server
#' 
#'
#' @param year max year to retrieve data from 
#' @param species 5 digit species code (e.g., 10110) - can place multiple in a vector c(10110, 10130)
#' @param area bs, ai, goa, or hwc, wc, hg, hbs - can do multiples c("bs","ai")
#' @param db data server to connect to (afsc)
#' @param print_sql outputs the sql query instead of calling the data (default: false) - save must be false
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: true)
#' 
#' @return saves bts length data as data/raw/bts_length_data.csv or outputs to the global environment, also saves a copy of the SQL code used for the query and stores it in the data/sql folder. 
#' @export q_bts_length
#'
#' @examples
#' \dontrun{
#' db <- afscdata::connect("afsc")
#' q_bts_length(year=2022, species=10110, area = "goa", db = db)
#' }
q_bts_length <- function(year, species, area, db, print_sql=FALSE, save=TRUE){
  
  # globals
  yr = year
  area = toupper(area)
  
  # pull data sources
  dplyr::tbl(db, dplyr::sql("racebase.cruise")) %>% 
    dplyr::rename_with(tolower) -> aa
  
  dplyr::tbl(db, dplyr::sql("racebase.haul")) %>% 
    dplyr::rename_with(tolower) -> bb
  
  dplyr::tbl(db, dplyr::sql("racebase.length")) %>% 
    dplyr::rename_with(tolower) -> cc
  # join, filter and query
  dplyr::select(aa, cruisejoin, region, survey_name, start_date) %>% 
    dplyr::left_join(dplyr::select(bb, cruisejoin, hauljoin, end_latitude, 
                                   end_longitude, bottom_depth, abundance_haul, 
                                   stratum, gear_temperature, performance)) %>%
    dplyr::left_join(dplyr::select(cc, hauljoin, species_code, sex, length, 
                                   frequency)) %>% 
    dplyr::mutate(year = lubridate::year(start_date)) %>%
    dplyr::select(-start_date) %>% 
    dplyr::filter(abundance_haul == "Y", 
                  year <= yr, 
                  region %in% area, 
                  species_code %in% species) %>% 
    dplyr::arrange(year) -> table
  
  if(isTRUE(save)) {
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", "bts_length_data.csv"), 
                         delim = ",")
    capture.output(dplyr::show_query(table), 
                   file = here::here(year, "data", "sql", "bts_length_sql.txt"))
    
    message("bottom trawl survey length data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
  
}

#' query bottom trawl survey specimen data from the AFSC server
#' 
#'
#' @param year  max year to retrieve data from 
#' @param species 5 digit species code (e.g., 10110) - can place multiple in a vector c(10110, 10130)
#' @param area bs, ai, goa, or hwc, wc, hg, hbs - can do multiples c("bs","ai")
#' @param db data server to connect to (afsc)
#' @param print_sql outputs the sql query instead of calling the data (default: false)
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: true)
#' @return saves bts length data as data/raw/bts_length_data.csv or outputs to the global environment, also saves a copy of the SQL code used for the query and stores it in the data/sql folder. 
#' @export q_bts_specimen
#'
#' @examples
#' \dontrun{
#' db <- afscdata::connect("afsc")
#' q_bts_length(year=2022, species=21921, area = "goa", db = db)
#' } 
q_bts_specimen <- function(year, species, area, db, print_sql=FALSE, save=TRUE){
  
  # globals
  yr = year
  area = toupper(area)
  
  # pull data sources
  dplyr::tbl(db, dplyr::sql("racebase.cruise")) %>% 
    dplyr::rename_with(tolower) -> aa
  
  dplyr::tbl(db, dplyr::sql("racebase.haul")) %>% 
    dplyr::rename_with(tolower) -> bb
  
  dplyr::tbl(db, dplyr::sql("racebase.specimen")) %>% 
    dplyr::rename_with(tolower) -> cc
  
  # join, filter and query
  dplyr::select(aa, cruisejoin, region, survey_name, start_date) %>% 
    dplyr::left_join(dplyr::select(bb, cruisejoin, hauljoin, end_latitude, 
                                   end_longitude, bottom_depth, abundance_haul, 
                                   stratum, gear_temperature)) %>%
    dplyr::left_join(dplyr::select(cc, hauljoin, species_code, sex, 
                                   length, weight, age, maturity)) %>% 
    dplyr::mutate(year = lubridate::year(start_date)) %>%
    dplyr::select(-start_date) %>% 
    dplyr::filter(abundance_haul == "Y", 
                  year <= yr, 
                  region %in% area, 
                  species_code %in% species) %>% 
    dplyr::arrange(year) -> table
  
  if(isTRUE(save)) {
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", "bts_specimen_data.csv"), 
                         delim = ",")
    capture.output(dplyr::show_query(table), 
                   file = here::here(year, "data", "sql", "bts_specimen_sql.txt"))
    
    message("bottom trawl survey specimen data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
  
  
}

#' query bottom trawl survey biomass
#' 
#' probably need to beef up the documentation on the "by" switch
#' 
#' six areas are available to query from 
#' bs = bering sea + northwest 1987-present (includes nw stations) - recommended
#' bsslope = bering sea slope 
#' nbs = northern bering sea
#' ai = aleutian islands 
#' goa = gulf of alaska
#' old_bs = bering sea standard 1982-present (minus ~20 stations in nw) - not recommended 
#' 
#' for the goa and ai there is a "type" switch that queries by depth, stratum, area, total, or uses the inpfc table or inpfc by depth table only one of these can be used at a time.
#' 
#' @param year  max year to retrieve data from 
#' @param area options are bs, bsslope, nbs, ai, goa, old_bs - can only call a single area
#' @param species 5 digit afsc species code(s) e.g., 79210 or c(79210, 90210)
#' @param type "depth", "stratum", "area", "total", "inpfc", "inpfc_depth" - only available for goa/ai (default: "total") - can only use a single switch
#' @param db  the database to query (akfin)
#' @param print_sql outputs the sql query instead of calling the data (default: false)
#' @param save save the file in designated folder, if FALSE outputs to global environment
#' 
#' @return saves bts biomass data as data/raw/area_(type)_bts_biomass_data.csv or outputs to the global environment, also saves a copy of the SQL code used for the query and stores it in the data/sql folder. 
#' @export q_bts_biomass
#'
#' @examples
#' \dontrun{
#' db <- afscdata::connect("akfin")
#' q_bts_biomass(year=2022, species=21921, area = "goa", type = "depth", db = db)
#' } 
q_bts_biomass <- function(year, species, area, type='total', db, print_sql=FALSE, save=TRUE) {
  
  # adjust filters
  yr = year
  area = toupper(area)
  type = tolower(type)
  
  # message center
  if(type!="total") {
    if(type %in% c("depth", "stratum", "area", "total", "inpfc", "inpfc_depth")==FALSE){ 
      stop("appropriate args for type are: 'depth', 'stratum' 'area', or 'total'.\n 
           these args will be ignored if area != 'goa' or 'ai'")
    }
  }
  
  # decide which tables to use 
  
  if(area=="BS") {
    table = dplyr::tbl(db, dplyr::sql("afsc.race_biomass_ebsshelf_standard"))
  } else if(area=="BSNW") {
    table = dplyr::tbl(db, dplyr::sql("afsc.race_biomass_ebsshelf_plusnw"))
  } else if(area=="BSSLOPE") {
    table = dplyr::tbl(db, dplyr::sql("afsc.race_biomass_ebsslope"))
  } else if(area=="NBS") {
    table = dplyr::tbl(db, dplyr::sql("afsc.race_biomass_nbs"))
  } else if(area %in% c("GOA", "AI") & type=="depth"){
    table = dplyr::tbl(db, dplyr::sql("afsc.race_biomassdepthaigoa"))
  } else if(area %in% c("GOA", "AI") & type=="area"){
    table = dplyr::tbl(db, dplyr::sql("afsc.race_biomassareaaigoa"))    
  } else if(area %in% c("GOA", "AI") & type=="stratum"){
    table = dplyr::tbl(db, dplyr::sql("afsc.race_biomassstratumaigoa"))    
  } else if(area %in% c("GOA", "AI") & type=="inpfc"){
    table = dplyr::tbl(db, dplyr::sql("afsc.race_biomassinpfcaigoa")) 
  } else if(area %in% c("GOA", "AI") & type=="inpfc_depth"){
    table = dplyr::tbl(db, dplyr::sql("afsc.race_biomassinpfcdepthaigoa")) 
  } else {
    table = dplyr::tbl(db, dplyr::sql("afsc.race_biomasstotalaigoa"))     
  }
    
    
  table <- table %>% 
    dplyr::rename_with(tolower) %>% 
    dplyr::filter(year <= yr, species_code %in% species)
  
  if(area %in% c("AI", "GOA")){
    table <- dplyr::filter(table, survey == area)
  } 

  # prefix area and type (for goa/ai) to file name
  area = tolower(area)
  id = NULL
  if(area %in% c("ai", "goa")) id = paste0(type, "_")
  id = paste0(area, "_", id)
  
  if(isTRUE(save)){
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", paste0(id, "bts_biomass_data.csv")), 
                         delim = ",")
    capture.output(dplyr::show_query(table), 
                   file = here::here(year, "data", "sql", paste0(id, "bts_biomass_sql.txt")))
    
    message("bottom trawl survey biomass data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
  
}

#' query bottom trawl survey agecomps
#' 
#' 
#' currently only available for goa and ai
#' 
#' for the goa and ai there is a "type" switch that queries by stratum, or total - !!stratum should not be used at the moment !!. only one of these can be used at a time.
#' 
#' @param year  max year to retrieve data from 
#' @param area options are 'ai' or 'goa' - can only call a single area
#' @param species 5 digit afsc species code(s) e.g., 79210 or c(79210, 90210)
#' @param type "stratum" or "total" - only available for goa/ai (default: "total") - can only use a single switch
#' @param db  the database to query (afsc)
#' @param print_sql outputs the sql query instead of calling the data (default: false)
#' @param save save the file in designated folder, if FALSE outputs to global environment
#' 
#' @return saves bts agecomp data as data/raw/area_(type)_bts_agecomp_data.csv or outputs to the global environment, also saves a copy of the SQL code used for the query and stores it in the data/sql folder. 
#' @export q_bts_agecomp
#'
#' @examples
#' \dontrun{
#' db <- afscdata::connect("afsc")
#' q_bts_agecomp(year=2022, species=21921, area = "goa", type = "total", db = db)
#' } 
q_bts_agecomp <- function(year, species, area, type='total', db, print_sql=FALSE, save=TRUE){
  # adjust filters
  yr = year
  area = tolower(area)
  type = tolower(type)
  
  # message center
  if(type!="total") {
    if(type %in% c("stratum", "total")==FALSE){ 
      stop("appropriate args for type are: 'stratum' or 'total'.\n 
           these args will be ignored if area != 'goa' or 'ai'")
    }
  }
  
  if(area == 'goa' & type == 'total') {
    survey = 47
    id = 99903
    } else if(area == 'goa' & type == 'stratum'){
      survey = 47
      id = 10:550
    } else if(area == "ai" & type == 'total') {
      survey = 52    
      id = c(211:224,311:324,411:424,511:523,611:624,711:722)
    } else if(area == "ai" & type == 'stratum') {
      survey = 52  
      id = 99904
    } else {
      stop('incorrect area and type combination')
    }
  
  dplyr::tbl(db, dplyr::sql("gap_products.agecomp")) %>% 
    dplyr::rename_with(tolower) %>% 
    dplyr::filter(survey_definition_id == survey,
                  area_id %in% id, 
                  species_code %in% species) -> table

  # # prefix area and type to file name
  id = NULL
  if(area %in% c("ai", "goa")) id = paste0(type, "_")
  id = paste0(area, "_", id)

  if(isTRUE(save)){
    dplyr::collect(table) %>%
      vroom::vroom_write(here::here(year, "data", "raw", paste0(id, "bts_agecomp_data.csv")),
                         delim = ",")
    capture.output(dplyr::show_query(table),
                   file = here::here(year, "data", "sql", paste0(id, "bts_agecomp_sql.txt")))

    message("bottom trawl survey agecomp data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
  
}

#' query bottom trawl survey sizecomps
#' 
#' 
#' currently only available for goa and ai
#' 
#' for the goa and ai there is a "type" switch that queries by stratum, or total - !! stratum should not be used at the moment !!. only one of these can be used at a time.
#' 
#' @param year  max year to retrieve data from 
#' @param area options are 'ai' or 'goa' - can only call a single area
#' @param species 5 digit afsc species code(s) e.g., 79210 or c(79210, 90210)
#' @param type "stratum" or "total" - only available for goa/ai (default: "total") - can only use a single switch
#' @param db  the database to query (afsc)
#' @param print_sql outputs the sql query instead of calling the data (default: false)
#' @param save save the file in designated folder, if FALSE outputs to global environment
#' 
#' @return saves bts sizecomp data as data/raw/area_(type)_bts_sizecomp_data.csv or outputs to the global environment, also saves a copy of the SQL code used for the query and stores it in the data/sql folder. 
#' @export q_bts_sizecomp
#'
#' @examples
#' \dontrun{
#' db <- afscdata::connect("afsc")
#' q_bts_sizecomp(year=2022, species=21921, area = "goa", type = "total", db = db)
#' } 
q_bts_sizecomp <- function(year, species, area, type='total', db, print_sql=FALSE, save=TRUE){
  # adjust filters
  yr = year
  area = tolower(area)
  type = tolower(type)
  
  # message center
  if(type!="total") {
    if(type %in% c("stratum", "total")==FALSE){ 
      stop("appropriate args for type are: 'stratum' or 'total'.\n 
           these args will be ignored if area != 'goa' or 'ai'")
    }
  }
  
  if(area == 'goa' & type == 'total') {
    survey = 47
    id = 99903
  } else if(area == 'goa' & type == 'stratum'){
    survey = 47
    id = 10:550
  } else if(area == "ai" & type == 'total') {
    survey = 52    
    id = c(211:224,311:324,411:424,511:523,611:624,711:722)
  } else if(area == "ai" & type == 'stratum') {
    survey = 52  
    id = 99904
  } else {
    stop('incorrect area and type combination')
  }
  
  dplyr::tbl(db, dplyr::sql("gap_products.sizecomp")) %>% 
    dplyr::rename_with(tolower) %>% 
    dplyr::filter(survey_definition_id == survey,
                  area_id %in% id, 
                  species_code %in% species) -> table
  
  # # prefix area and type to file name
  id = NULL
  if(area %in% c("ai", "goa")) id = paste0(type, "_")
  id = paste0(area, "_", id)
  
  if(isTRUE(save)){
    dplyr::collect(table) %>%
      vroom::vroom_write(here::here(year, "data", "raw", paste0(id, "bts_sizecomp_data.csv")),
                         delim = ",")
    capture.output(dplyr::show_query(table),
                   file = here::here(year, "data", "sql", paste0(id, "bts_sizecomp_sql.txt")))
    
    message("bottom trawl survey sizecomp data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
  
}
# fishery ----
#' query fishery catch data from AKFIN server
#'
#' Note that this function produces results that may be confidential. 
#' They can be hidden from git/GitHub by adding fsh_catch_data.csv to your .gitignore file
#'
#' @param year max year to retrieve data from 
#' @param species species group code e.g., "DUSK" or numeric agency values e.g. c("131", "132") - must be either all 4 digit or 3 digit codes
#' @param area fmp_area (GOA, BSAI) or fmp_subarea (BS, AI, WG, CG, WY, EY, SE) - also available (SEI, PWSI), can use all fmp_areas or all fmp_subareas, but don't mix the two
#' @param db data server to connect to (akfin)
#' @param add_fields add other columns to the database (must currently exist on server). "*" will return all table columns available
#' @param print_sql outputs the sql query instead of calling the data - save must be false
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: TRUE)
#' 
#' @return saves catch data as data/raw/fsh_catch_data.csv or outputs to the global environment, save also saves a copy of the SQL code used for the query and stores it in the data/sql folder.
#' @export q_catch
#' @examples 
#' \dontrun{
#' db <- afscdata::connect()
#' q_catch(year=2022, species="NORK", area="goa", db=db)
#' }
#'  
q_catch <- function(year, species, area, db, add_fields=NULL, print_sql=FALSE, save=TRUE) {
  
  # globals 
  area = toupper(area)
  area = if(isTRUE(area == "GOA")){
    area = c("WG", "CG", "WY", "EY", "SE")
  } else if(isTRUE(area=="BSAI")){
    area = c("BS", "AI")
  } else if(sum(sapply(c("BSAI", "GOA"), grepl, area))==2){
    area = c("WG", "CG", "WY", "EY", "SE", "BS", "AI")
  } else {
    area
  }
  
  yr = year
  
  # select columns to import
  if(!is.null(add_fields)) {
    if(grepl("\\*", add_fields)){
      table <- dplyr::tbl(db, dplyr::sql("council.comprehensive_blend_ca")) %>% 
        dplyr::rename_with(tolower) %>% 
        dplyr::filter(year <= yr, fmp_subarea %in% area)
    }
  }  else {
    cols = c("year",                             
             "agency_species_code",              
             "species_group_name",               
             "species_name",          
             "species_group_code",               
             "retained_or_discarded",            
             "trip_target_code",                 
             "trip_target_name",               
             "cdq_flag",                  
             "fmp_gear",                 
             "agency_gear_code",                   
             "fmp_area",                           
             "fmp_subarea",                        
             "reporting_area_code",   
             "week_end_date",                      
             "weight_posted",              
             "vessel_id",                          
             "ves_akr_length",                     
             "sampling_strata",                    
             "sampling_strata_name",               
             "sampling_strata_deployment_category",
             "sampling_strata_selection_rate",     
             "deployment_trip_pk",                 
             "deployment_trip_start_date",         
             "deployment_trip_end_date",           
             "adfg_stat_area_code",                
             "akr_state_federal_waters_code",
             tolower(add_fields))
    
    
    table <- dplyr::tbl(db, dplyr::sql("council.comprehensive_blend_ca")) %>% 
      dplyr::rename_with(tolower) %>% 
      dplyr::select(!!!cols) %>% 
      dplyr::filter(year <= yr, fmp_subarea %in% area)
  }
  
  # filter species
  if(isTRUE(sum(stringi::stri_length(species) - 3) == 0)){
    dplyr::filter(table, agency_species_code %in% species) -> table
  } else {
    dplyr::filter(table, species_group_code %in% species) -> table
  }
  
  # output
  if(isTRUE(save)) {
    if(isFALSE(dir.exists(here::here(year, "data")))) {
      stop("you must run afscdata::setup_folders() before you can save to the default location")
      }
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", "fsh_catch_data.csv"), 
                         delim = ",")
    
    capture.output(dplyr::show_query(table), 
                   file = here::here(year, "data", "sql", "fsh_catch_sql.txt"))
    
    message("fishery catch data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
}
#' query foreign catch data from the AKFIN server
#' 
#' something about the species names and area codes, since they are so different from everything else?
#' pulls data from AKFIN pre1991.foreign_blend table
#'
#' @param year max year to retrieve data from 
#' @param species common name (e.g., "sablefish" or "all flounders") - can call multiple species
#' @param area numeric area digit code - multiples is ok
#' @param db data server to connect to (akfin)
#' @param print_sql outputs the sql query instead of calling the data (default: false) - save must be false
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: true)
#'
#' @return saves catch data as data/raw/for_catch_data.csv or outputs to the global environment, also saves a copy of the SQL code used for the query and stores it in the data/sql folder. 
#' @export q_catch_foreign
#'
#' @examples
#' \dontrun{
#' db <- afscdata::connect()
#' q_catch_foreign(year=2022, species="sablefish", area = 54:57, db = db)
#' }
q_catch_foreign <- function(year, species, area, db, print_sql=FALSE, save=TRUE){
  yr = year
  species=toupper(species)
  
  dplyr::tbl(db, dplyr::sql("pre1991.foreign_blend")) %>% 
    dplyr::rename_with(tolower) %>% 
    dplyr::select(species_name, 
                  tons = blend_tonnage,
                  date = week_date,
                  vessel_class,
                  area_number,
                  area_name) %>% 
    dplyr::mutate(year = lubridate::year(date),
                  month = lubridate::month(date),
                  gear = dplyr::case_when(vessel_class %like% '%POT%' ~ "POT",
                                          vessel_class %like% '%LONG%' ~ "HAL",
                                          TRUE ~ "TRW")) %>% 
    dplyr::group_by(species_name, year, month, area_number, gear, area_name) %>% 
    dplyr::summarise(tons = sum(tons, na.rm = T)) %>% 
    dplyr::filter(year<=yr, species_name %in% species, area_number %in% area) %>% 
    dplyr::rename(species=species_name, area=area_number) %>% 
    dplyr::arrange(species, year, month, area, gear) -> table
  
  if(isTRUE(save)) {
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", "for_catch_data.csv"), 
                         delim = ",")
    capture.output(dplyr::show_query(table), 
                   file = here::here(year, "data", "sql", "for_catch_sql.txt"))
    
    message("foreign catch data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
}

#' fishery observer data query
#' 
#' currently setup for pulling haul level detail for age or length compositions
#'
#'
#' @param year max year to retrieve data from 
#' @param species numeric agency values e.g. c("131", "132") - must be 3 digit codes (norpac species codes)
#' @param area fmp_area (GOA, BSAI) or fmp_subarea (BS, AI, WG, CG, WY, EY, SE) - also available (SEI, PWSI), can use all fmp_areas or all fmp_subareas, but don't mix the two
#' @param db data server to connect to (akfin)
#' @param print_sql outputs the sql query instead of calling the data - save must be false
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: TRUE)
#' 
#' @return saves observer data as data/raw/fsh_obs_data.csv or outputs to the global environment, save also saves a copy of the SQL code used for the query and stores it in the data/sql folder.
#' @export q_fish_obs
#' @examples 
#' \dontrun{
#' db <- afscdata::connect()
#' q_fish_obs(year=2022, species=301, area="goa", db=db)
#' }
#'
q_fish_obs <- function(year, species, area, db, print_sql=FALSE, save=TRUE) {
  # globals 
  area = toupper(area)
  area = if(isTRUE(area == "GOA")){
    area = c("WG", "CG", "WY", "EY", "SE")
  } else if(isTRUE(area=="BSAI")){
    area = c("BS", "AI")
  } else if(sum(sapply(c("BSAI", "GOA"), grepl, area))==2){
    area = c("WG", "CG", "WY", "EY", "SE", "BS", "AI")
  } else {
    area
  }
  yr = year
  sp = species
  
  
  
table <- dplyr::tbl(db, dplyr::sql("norpac.debriefed_spcomp_mv")) %>% 
          dplyr::rename_with(tolower) %>% 
          dplyr::mutate(dplyr::across(c(join_key, haul_join), as.character)) %>% 
          dplyr::left_join(dplyr::tbl(db, dplyr::sql("norpac.debriefed_haul_mv")) %>% 
                             dplyr::rename_with(tolower) %>% 
                             dplyr::select(fmp_subarea, gear_type, join_key) %>% 
                             dplyr::mutate(dplyr::across(contains("key"), as.character))) %>% 
          dplyr::filter(year<=yr, 
                        species %in% sp, 
                        fmp_subarea %in% area) 

  # output
  if(isTRUE(save)) {
    if(isFALSE(dir.exists(here::here(year, "data")))) {
      stop("you must run afscdata::setup_folders() before you can save to the default location")
    }
    
      dplyr::collect(table) %>% 
        vroom::vroom_write(here::here(year, "data", "raw", "fsh_obs_data.csv"), 
                           delim = ",")
      
      capture.output(dplyr::show_query(table), 
                     file = here::here(year, "data", "sql", "fsh_obs_sql.txt"))
    
    
    message("fishery observer data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
}

#' query fish ticket data
#'
#' @param year max year to query through (and location to save results)
#' @param area fmp_subare
#' @param species species group code e.g., "DUSK" 
#' @param area fmp_area (GOA, BSAI) or fmp_subarea (BS, AI, WG, CG, WY, EY, SE) - also available (SEI, PWSI), can use all fmp_areas or all fmp_subareas, but don't mix the two
#' @param db data server to connect to (akfin)
#' @param add_fields add other columns to the database (must currently exist on server). "*" will return all table columns available
#' @param print_sql outputs the sql query instead of calling the data - save must be false
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: TRUE)
#' 
#' @return saves fish ticket data as data/raw/fishticket_data.csv or outputs to the global environment, save also saves a copy of the SQL code used for the query and stores it in the data/sql folder.
#' @export q_fish_ticket
#'
#' @examples 
#' \dontrun{
#' q_fish_ticket(year=2022, species="NORK", area="goa", db=db)
#' }
q_fish_ticket <- function(year, species, area, db, add_fields=NULL, print_sql=FALSE, save=TRUE) {
  # globals 
  area = toupper(area)
  area = if(isTRUE(area == "GOA")){
    area = c("WG", "CG", "WY", "EY", "SE")
  } else if(isTRUE(area=="BSAI")){
    area = c("BS", "AI")
  } else if(sum(sapply(c("BSAI", "GOA"), grepl, area))==2){
    area = c("WG", "CG", "WY", "EY", "SE", "BS", "AI")
  } else {
    area
  }
  yr = year
  
  
  # select columns to import
  if(!is.null(add_fields)) {
    if(grepl("\\*", add_fields)){
      table <- dplyr::tbl(db, dplyr::sql("council.comprehensive_ft")) %>% 
        dplyr::rename_with(tolower) %>% 
        dplyr::filter(year <= yr, fmp_subarea %in% area)
    }
  }  else {
    cols = c("akfin_year",                             
             "cfec_pacfin_species_code",              
             "adfg_h_ticket_type",               
             "fmp_subarea",          
             "management_area_district_code",               
             "cfec_whole_pounds",            
             "fmp_gear",
             tolower(add_fields))
    
    
    table <- dplyr::tbl(db, dplyr::sql("council.comprehensive_ft")) %>% 
      dplyr::rename_with(tolower) %>% 
      dplyr::select(!!!cols) %>% 
      dplyr::filter(year <= yr, fmp_subarea %in% area)
  }
  

    table <- dplyr::filter(table, cfec_pacfin_species_code %in% species) 
  
  # output
  if(isTRUE(save)) {
    if(isFALSE(dir.exists(here::here(year, "data")))) {
      stop("you must run afscdata::setup_folders() before you can save to the default location")
    }
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", "fishticket_data.csv"), 
                         delim = ",")
    
    capture.output(dplyr::show_query(table), 
                   file = here::here(year, "data", "sql", "fishticket_sql.txt"))
    
    message("fishery catch data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
}

#' query fishery specimen data from the AKFIN server
#' 
#'
#' @param year  max year to retrieve data from 
#' @param species 3 digit species codes (e.g., 201) - can place multiple in a vector c(201, 131)
#' @param area bs, ai, goa, or hwc, wc, hg, hbs - can do multiples c("bs","ai")
#' @param db data server to connect to (akfin)
#' @param add_fields add other columns to the database (must currently exist on server). "*" will return all table columns available
#' @param print_sql outputs the sql query instead of calling the data (default: false)
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: true)
#' @return saves fishery specimen data as data/raw/fsh_specimen_data.csv or outputs to the global environment, also saves a copy of the SQL code used for the query and stores it in the data/sql folder. 
#' @export q_fsh_specimen
#'
#' @examples
#' \dontrun{
#' db <- afscdata::connect("akfin")
#' q_fsh_specimen(year=2022, species=301, area = "goa", db = db)
#' } 
q_fsh_specimen <- function(year, species, area, db, add_fields=NULL, print_sql=FALSE, save=TRUE){
  
  # globals 
  area = toupper(area)
  area = if(isTRUE(area == "GOA")){
    area = c("WG", "CG", "WY", "EY", "SE")
  } else if(isTRUE(area=="BSAI")){
    area = c("BS", "AI")
  } else if(sum(sapply(c("BSAI", "GOA"), grepl, area))==2){
    area = c("WG", "CG", "WY", "EY", "SE", "BS", "AI")
  } else {
    area
  }
  
  yr = year
  sp = species
  
  
  # select columns to import
  if(!is.null(add_fields)) {
    if(grepl("\\*", add_fields)){
      table <- dplyr::tbl(db, dplyr::sql("norpac.debriefed_age_mv")) %>% 
        dplyr::rename_with(tolower) %>% 
        dplyr::mutate(dplyr::across(c(join_key, haul_join, port_join), as.character)) %>% 
        dplyr::left_join(dplyr::tbl(db, dplyr::sql("norpac.debriefed_haul_mv")) %>% 
                           dplyr::rename_with(tolower) %>% 
                           dplyr::select(fmp_subarea, gear_type, join_key) %>% 
                           dplyr::mutate(join_key = as.character(join_key))) %>% 
        dplyr::filter(year <= yr & year>0, 
                      fmp_subarea %in% area, 
                      species %in% sp,
                      !is.na(age)) %>% 
        dplyr::arrange(year)
    }
  }  else {
    cols = c("year", "performance", "specimen_type", "join_key", "haul_join", "port_join",
             "species", "fmp_gear", "fmp_area", "fmp_subarea", 
             "age", "length", "weight",
             tolower(add_fields))
    
    
    table <- dplyr::tbl(db, dplyr::sql("norpac.debriefed_age_mv")) %>% 
      dplyr::rename_with(tolower) %>% 
      dplyr::select(!!!cols) %>% 
      dplyr::mutate(dplyr::across(c(join_key, haul_join, port_join), as.character)) %>% 
      dplyr::left_join(dplyr::tbl(db, dplyr::sql("norpac.debriefed_haul_mv")) %>% 
                         dplyr::rename_with(tolower) %>% 
                         dplyr::select(fmp_subarea, gear_type, join_key) %>% 
                         dplyr::mutate(join_key = as.character(join_key))) %>% 
      dplyr::filter(year <= yr & year > 0, 
                    fmp_subarea %in% area, 
                    species %in% sp,
                    !is.na(age)) %>% 
      dplyr::arrange(year)
  }
  
  
  if(isTRUE(save)) {
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", "fsh_specimen_data.csv"), 
                         delim = ",")
    capture.output(dplyr::show_query(table), 
                   file = here::here(year, "data", "sql", "fsh_specimen_sql.txt"))
    
    message("fishery specimen data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
  
  
}


#' query fishery length data from the AKFIN server
#' 
#'
#' @param year  max year to retrieve data from 
#' @param species 3 digit species codes (e.g., 201) - can place multiple in a vector c(201, 131)
#' @param area bs, ai, goa, or hwc, wc, hg, hbs - can do multiples c("bs","ai")
#' @param db data server to connect to (akfin)
#' @param add_fields add other columns to the database (must currently exist on server). "*" will return all table columns available
#' @param print_sql outputs the sql query instead of calling the data (default: false)
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: true)
#' @return saves fishery length data as data/raw/fsh_length_data.csv or outputs to the global environment, also saves a copy of the SQL code used for the query and stores it in the data/sql folder. 
#' @export q_fsh_length
#'
#' @examples
#' \dontrun{
#' db <- afscdata::connect("akfin")
#' q_fsh_length(year=2022, species=301, area="goa", db=db)
#' } 
q_fsh_length <- function(year, species, area, db, add_fields=NULL, print_sql=FALSE, save=TRUE){
  
  # globals 
  area = toupper(area)
  area = if(isTRUE(area == "GOA")){
    area = c("WG", "CG", "WY", "EY", "SE")
  } else if(isTRUE(area=="BSAI")){
    area = c("BS", "AI")
  } else if(sum(sapply(c("BSAI", "GOA"), grepl, area))==2){
    area = c("WG", "CG", "WY", "EY", "SE", "BS", "AI")
  } else {
    area
  }
  
  yr = year
  sp = species
  
  
  # select columns to import
  if(!is.null(add_fields)) {
    if(grepl("\\*", add_fields)){
      table <- dplyr::tbl(db, dplyr::sql("norpac.debriefed_length_mv")) %>% 
        dplyr::rename_with(tolower) %>% 
        dplyr::mutate(dplyr::across(c(join_key, haul_join, port_join), as.character)) %>% 
        dplyr::left_join(dplyr::tbl(db, dplyr::sql("norpac.debriefed_haul_mv")) %>% 
                           dplyr::rename_with(tolower) %>% 
                           dplyr::select(fmp_subarea, gear_type, join_key) %>% 
                           dplyr::mutate(join_key = as.character(join_key))) %>% 
        dplyr::filter(year <= yr & year>0, 
                      fmp_subarea %in% area, 
                      species %in% sp,
                      !is.na(age)) %>% 
        dplyr::arrange(year)
    }
  }  else {
    cols = c("year", "performance", "haul_join", "port_join",
             "species", "fmp_gear", "fmp_area", "fmp_subarea", 
             "sex", "length", "frequency",
             tolower(add_fields))
    
    
    table <- dplyr::tbl(db, dplyr::sql("norpac.debriefed_length_mv")) %>% 
      dplyr::rename_with(tolower) %>% 
      dplyr::mutate(dplyr::across(c(join_key, haul_join, port_join), as.character)) %>% 
      dplyr::left_join(dplyr::tbl(db, dplyr::sql("norpac.debriefed_haul_mv")) %>% 
                         dplyr::rename_with(tolower) %>% 
                         dplyr::select(fmp_subarea, gear_type, join_key) %>% 
                         dplyr::mutate(join_key = as.character(join_key))) %>% 
      dplyr::select(!!!cols) %>% 
      dplyr::filter(year <= yr & year > 0, 
                    fmp_subarea %in% area, 
                    species %in% sp,
                    !is.na(length)) %>% 
      dplyr::arrange(year)
  }
  
  
  if(isTRUE(save)) {
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", "fsh_length_data.csv"), 
                         delim = ",")
    capture.output(dplyr::show_query(table), 
                   file = here::here(year, "data", "sql", "fsh_length_sql.txt"))
    
    message("fishery length data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
  
  
}

# longline survvey ----
#' query nmfs longline survey raw length frequencies at the depth stratum and
#' station level
#' 
#' longline database documentation available on
#' \href{https://akfinbi.psmfc.org/analyticsRes/Documentation/Database_Background_Instructions_AKFIN_20210915.pdf}{akfin}
#' 
#' primarily used in stock assessments for calculating sample size tables
#' 
#' sex-specific lengths are collected for sablefish, giant grenadier, spiny
#' dogfish, pacific cod, and greenland turbot (starting in 2021!). sex codes:
#' 1=male, 2=female, 3=unknown
#' 
#' source table on akfin:
#' lls_length_summary_view
#' 
#' @param year max year to retrieve data from 
#' @param species 5 digit afsc/race species code(s) e.g., 20510 for sablefish or
#'   c(30576, 30050) for both shortraker and rougheye/blackspotted
#' @param area options are 'goa', 'bs', 'ai', or a combo. default=c('goa', 'bs', 'ai')
#' @param use_historical T/F include historical Japanese survey data in the
#'   results (default: false)
#' @param db  the database to query (akfin)
#' @param print_sql outputs the sql query instead of calling the data (default: false) - save must be false
#' @param save save the file in designated folder, if FALSE outputs to global environment
#'  
#' @return saves lls length data as data/raw/lls_length_data.csv or outputs to
#'   the global environment. also saves a copy of the SQL code used for the
#'   query and stores it in the data/sql folder.
#' @export q_lls_length
#'
#' @examples
#' \dontrun{
#' # raw sablefish length frequencies in 1988 (year when the domestic survey started) 
#' # in the gulf of alaska
#' 
#' db <- connect("akfin")
#' q_lls_length(year=1988, species=20510, area="goa", db=db)
#' } 
#' 
q_lls_length <- function(year, species, area=c('goa', 'bs', 'ai'), use_historical=FALSE, 
                         db, print_sql=FALSE, save=TRUE) {

  # adjust filters
  yr = year
  sp = species
  area = toupper(area)
  if(any(area %in% c('GOA'))) {
    a1 <- c('Western Gulf of Alaska', 'Central Gulf of Alaska', 'West Yakutat', 'East Yakutat/Southeast')
  } else {
    a1 <- NULL
  }
  if(any(area %in% c('BS', 'BSAI'))) {
    a2 <- c('Bering Sea')
    } else {
      a2 <- NULL
    }
  if(any(area %in% c('AI', 'BSAI'))) {
    a3 <- c('Aleutians')
    } else {
      a3 <- NULL
    }
  area <- c(a1, a2, a3)

  # message center
  if(any(!area %in% c('Western Gulf of Alaska', 'Central Gulf of Alaska', 'West Yakutat', 'East Yakutat/Southeast', 'Bering Sea', 'Aleutians'))) {
    stop("the appropriate args for area are 'goa', 'bs', 'ai', or a combo. defaults to c('goa', 'bs', 'ai')")
  }
  if(is.logical(use_historical)==FALSE) {
    stop('appropriate args for use_historical are TRUE or FALSE')
  } else {
    if(isFALSE(use_historical)) {
      srv <- 'United States'
    } else {
      srv <- c('United States', 'Japan')
    } 
  }

  table = dplyr::tbl(db, dplyr::sql("afsc.lls_length_summary_view")) %>% 
    dplyr::rename_with(tolower) %>% 
    dplyr::filter(year <= yr, 
                  species_code %in% sp,
                  country %in% srv,
                  council_sablefish_mgmt_area %in% area) %>% 
    dplyr::arrange(year)

  if(isTRUE(save)){
    if(isFALSE(dir.exists(here::here(year, "data")))) {
      stop("you must run afscdata::setup_folders() before you can save to the default location")
    }
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", paste0("lls_length_data.csv")), 
                         delim = ",")
    capture.output(dplyr::show_query(table), 
                   file = here::here(year, "data", "sql", paste0("lls_length_sql.txt")))

    message("longline survey length data can be found in the data/raw folder,\n
            and the associated query is saved in data/sql")
  } else if(isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }

}

#' query nfms longline survey relative population numbers or weights
#' 
#' longline database documentation available on
#' \href{https://akfinbi.psmfc.org/analyticsRes/Documentation/Database_Background_Instructions_AKFIN_20210915.pdf}{akfin}
#' 
#' variables of interest:
#' cpue = numbers per skate (1 skate = 45 hooks); relative population numbers
#' (rpns) = area-weighted cpue (area estimates are defined by depth strata and
#' geographic area), relative population weights (rpw) = rpn multiplied by mean
#' fish weight, which is calculated using an allometric relationship and the
#' mean length of fish collected in a given strata and geographic area. 
#' 
#' methods for variance estimation of these indices are documented on
#' \href{https://apps-afsc.fisheries.noaa.gov/REFM/Docs/2016/GOAsablefish.pdf}{p
#' 26 the 2016 sablefish safe pdf} (p 350 of the goa safe).
#'
#' the time series starts for the domestic longline survey in the bs and ai
#' starts in 1996, and there are historical data available in the bs/ai from the
#' cooperative Japanese/U.S. survey. in the modern/domestic survey, the eastern
#' ai are surveyed in even years, and the bs is surveyed in odd years. the
#' longline survey does not sample the western ai. estimates in nw and sw ai are
#' based on fixed ratios in the ne and se ai, respectively, from historical
#' cooperative Japanese/U.S. surveys in 1979-1994.
#'
#' sablefish: includes data from strata 3-7 (depths 201-1000 m) and rpns are
#' adjusted for sperm whale depredation
#' 
#' all other species: includes data from all strata (depths 151-1000 m)
#' 
#' source tables on akfin:
#' lls_area_stratum_rpn
#' lls_area_stratum_rpn_depred
#' lls_area_rpn_all_strata
#' lls_area_rpn_3_to_7
#' lls_area_rpn_3_to_7_depred
#' lls_council_sablefish_area_all_strata
#' lls_council_sablefish_area_3_to_7_depred
#' lls_fmp_subarea_all_strata
#' lls_fmp_subarea_3_to_7_depred
#' lls_ak_wide_3_to_7_depred
#'  
#' @param year max year to retrieve data from 
#' @param species 5 digit afsc/race species code(s) e.g., 20510 for sablefish or
#'   c(30576, 30050) for both shortraker and rougheye/blackspotted
#' @param area options are 'goa', 'bs', 'ai', or a combo. default=c('goa', 'bs', 'ai')
#' @param by 'depth' (stratum-level), 'geoarea' (e.g. Spencer Gully, Kodiak
#'   slope), 'councilarea' (e.g., West Yakutat, East Yakutat/Southeast),
#'   'fmpsubarea' (e.g., Eastern Gulf of Alaska), or 'akwide' (only for
#'   sablefish). default: 'fmpsubarea' - can only call a single area. note that
#'   variances are not available at the depth stratum level (by = 'depth')
#' @param use_historical T/F include historical Japanese survey data in the
#'   results (default: false)
#' @param db  the database to query (akfin)
#' @param print_sql outputs the sql query instead of calling the data (default: false) - save must be false
#' @param save save the file in designated folder, if FALSE outputs to global environment
#' 
#' @return saves lls rpn/rpw data as data/raw/lls_rpn_(by)_data.csv or outputs to
#'   the global environment. also saves a copy of the SQL code used for the
#'   query and stores it in the data/sql folder.
#' @export q_lls_rpn
#'
#' @examples
#' \dontrun{
#' db <- connect("akfin")
#' # sablefish domestic survey rpn/rpw time series (1990-2022) by goa fmp subarea (wgoa,
#' # cgoa, egoa). note that sablefish rpns are corrected for sperm whale
#' # depredation and only include data from strata 3-7
#' 
#' q_lls_rpn(year=2022, species=20510, area='goa', by='fmpsubarea', db=db, save = FALSE)
#' 
#' # pcod domestic survey rpn/rpw time series by bering sea geographic area and
#' # depth stratum. note that the domestic bs time series is odd years starting in
#' # 1997 (ai starts in 1996).
#' 
#' q_lls_rpn(year=2022, species=21720, area='bs', by='depth', db=db, save=FALSE) 
#' }
q_lls_rpn <- function(year, species, area=c('goa', 'bs', 'ai'), by='fmpsubarea', 
                      use_historical=FALSE, db, print_sql=FALSE, save=TRUE) {
  
   # adjust filters
  yr = year
  sp = species
  if(by %in% c('akwide', 'AKWIDE')) {
    message("when by='akwide', the area argument is ignored within this function.\n")
    area <- c('goa', 'bs', 'ai')
  }
  if(any(area == 'bsai')) {
    area <- c(area[area != 'bsai'], 'bs', 'ai')
  }
  area = toupper(area)
  by = tolower(by)

  # message center
  if(any(!area %in% c('GOA', 'BS', 'AI'))) {
    stop("the appropriate args for area are 'goa', 'bs', 'ai', or a combo. defaults to c('goa', 'bs', 'ai')")
  }

  if(any(!by %in% c('depth', 'geoarea', 'councilarea', 'fmpsubarea', 'akwide')) | length(by) > 1){ 
    stop("appropriate args for by are: 'depth', 'geoarea', 'councilarea', 'fmpsubarea',  or 'akwide'.\n 
         only a single arg may be used.")
  }

  if(is.logical(use_historical)==FALSE) {
    stop('appropriate args for use_historical are TRUE or FALSE')
  } else {
    if(isFALSE(use_historical)) {
      srv <- 'United States'
    } else {
      srv <- c('United States', 'Japan')
    } 
  }
  # special sablefish
  if(any(species %in% 20510)) {
    strata <- '_3_to_7'
    depred <- '_depred'
    message("sablefish rpns:\n
            -are corrected for sperm whale depredation and area summaries \n
            -only include data from depth strata 3-7 (201-1000 m) \n
            -use interpolated bs/ai values in alternating survey years when aggregated to the fmp or ak-wide level \n
            -assume fixed rpn/rpw data in the ai (1990-1995) and bs (1990-1996) when no bs/ai surveys occurred\n
            -assume fixed ak-wide rpn/rpws from 1979-1994 for the historical Japanese survey \n")
    if(base::length(species) > 1) {
      warning("we do not recommended querying sablefish with other species. \n 
               output will only include data from strata 3-7, which may be inappropriate for non-sablefish species.")
    }
  } else {
    strata <- '_all_strata'
    depred <- ''
  }

  # decide which tables to use 
  if(by == 'depth') {
    table = dplyr::tbl(db, dplyr::sql(paste0("afsc.lls_area_stratum_rpn", depred))) %>%
      # Code used to flag geographic areas for calculations of relative
      # population weights and numbers.
      dplyr::filter(EXPLOITABLE == '1')
  } else if(by == 'geoarea') {
    table = dplyr::tbl(db, dplyr::sql(paste0("afsc.lls_area_rpn", strata, depred))) %>%
      dplyr::filter(EXPLOITABLE == '1')
  } else if(by == 'councilarea') {
    table = dplyr::tbl(db, dplyr::sql(paste0("afsc.lls_council_sablefish_area", strata, depred)))
  } else if(by == 'fmpsubarea') {
    table = dplyr::tbl(db, dplyr::sql(paste0("afsc.lls_fmp_subarea", strata, depred)))
  } else if(by == 'akwide') {
    if(species != 20510) {
      stop("ak-wide summaries are only available for sablefish. the lls alternates between the bs/ai in odd/even years, and interpolation methods should be evaluated on a species-specific basis.")
    }
    table = dplyr::tbl(db, dplyr::sql(paste0("afsc.lls_ak_wide", strata, depred)))
  }

  # set up filters that are consistent across all tables
  table <- table %>% 
    dplyr::rename_with(tolower) %>% 
    dplyr::filter(year <= yr, 
                  species_code %in% sp,
                  country %in% srv) %>% 
    dplyr::arrange(year)

  # set up table-specific area filters
  area_lkup = data.frame(fmparea = c('BS', 'AI', 'GOA', 'GOA', 'GOA', 'GOA'),
                         councilarea = c('Bering Sea', 'Aleutians', 
                                         'East Yakutat/Southeast', 
                                         'West Yakutat', 
                                         'Central Gulf of Alaska', 
                                         'Western Gulf of Alaska'),
                         fmpsubarea = c('Bering Sea', 'Aleutians', 
                                        'Eastern Gulf of Alaska', 
                                        'Eastern Gulf of Alaska', 
                                        'Central Gulf of Alaska', 
                                        'Western Gulf of Alaska')) %>% 
    dplyr::filter(fmparea %in% area)

  if(by %in% c('depth', 'geoarea', 'councilarea')) {
    tmparea <- base::unique(area_lkup$councilarea)
    table <- table %>% dplyr::filter(council_sablefish_management_area %in% tmparea) 
  }
  if(by %in% c('fmpsubarea')) {
    tmparea <- base::unique(area_lkup$fmpsubarea)
    table <- table %>% dplyr::filter(council_management_area %in% tmparea) 
  }

  # id for saving data and sql files
  id = by
  if(by == 'depth') {
    id = "depthstratum"
  }

  if(isTRUE(save)){
    if(isFALSE(dir.exists(here::here(year, "data")))) {
      stop("you must run afscdata::setup_folders() before you can save to the default location")
    }
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", paste0("lls_rpn_", id, "_data.csv")), 
                         delim = ",")
    capture.output(dplyr::show_query(table), 
                   file = here::here(year, "data", "sql", paste0("lls_rpn_", id, "_sql.txt")))

    message("longline survey data can be found in the data/raw folder,\n
            and the associated query is saved in data/sql")
  } else if (isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }

}

#' query nfms longline survey rpn-weighted length frequencies 
#' 
#' longline database documentation available on
#' \href{https://akfinbi.psmfc.org/analyticsRes/Documentation/Database_Background_Instructions_AKFIN_20210915.pdf}{akfin}
#' 
#' sablefish: includes data from strata 3-7 (depths 201-1000 m) and rpns are
#' adjusted for sperm whale depredation
#' 
#' all other species: includes data from all strata (depths 151-1000 m)
#' 
#' sex-specific lengths are collected for sablefish, giant grenadier, spiny
#' dogfish, pacific cod, and greenland turbot (starting in 2021!). sex codes:
#' 1=male, 2=female, 3=unknown
#' 
#' results only include geographic/stratum areas that are used for rpns calc
#' (i.e., exploitable == 1). also ' records with length = 999 have been removed.
#' these records are present in the database to account for instances when there '
#' was catch in a stratum/station but no lengths collected. the ' 999 lengths
#' ensure rpns sum properly to the area-level but should not be included in the
#' length compositions.
#' 
#' available source tables on akfin:
#' lls_length_area_3_to_7_depred
#' lls_length_area_all_strata
#'
#' @param year max year to retrieve data from 
#' @param species 5 digit afsc/race species code(s) e.g., 20510 for sablefish or
#'   c(30576, 30050) for both shortraker and rougheye/blackspotted
#' @param area options are 'goa', 'bs', 'ai', or a combo. default=c('goa', 'bs', 'ai')
#' @param by 'depth' (stratum-level), 'geoarea' (e.g. Spencer Gully, Kodiak
#'   slope), 'councilarea' (e.g., West Yakutat, East Yakutat/Southeast),
#'   'fmpsubarea' (e.g., Eastern Gulf of Alaska), or 'akwide' (only for
#'   sablefish). default: 'fmpsubarea' - can only call a single area. note that
#'   variances are not available at the depth stratum level (by = 'depth')
#' @param use_historical T/F include historical Japanese survey data in the
#'   results (default: false)
#' @param db  the database to query (akfin)
#' @param print_sql outputs the sql query instead of calling the data (default: false) - save must be false
#' @param save save the file in designated folder, if FALSE outputs to global environment
#' 
#' @return saves lls rpn-weighted length frequency data as
#'   data/raw/lls_rpn_length_data.csv or outputs to the global environment. also
#'   saves a copy of the SQL code used for the query and stores it in the
#'   data/sql folder.
#' @export q_lls_rpn_length
#'
#' @examples
#' \dontrun{
#' db <- afscdata::connect("akfin")
#' # sablefish domestic longline survey rpn-weighted length frequencies (1990-2022) for
#' # goa and bsai. note that sablefish rpns are corrected for sperm whale 
#' # depredation and only include data from strata 3-7
#' 
#' q_lls_rpn_length(year=2022, species=20510, area=c('bsai','goa'), db=db, save=FALSE)
#' 
#' # pcod domestic longline survey rpn-weighted length frequencies (1997-2022,
#'# odd years only) for the bering sea
#'
#' q_lls_rpn_length(year=2022, species=21720, area='bs', db=db, save=FALSE) 
#' }
#' 
q_lls_rpn_length <- function(year, species, area=c('goa', 'bs', 'ai'), by='fmpsubarea', 
                      use_historical=FALSE, db, print_sql=FALSE, save=TRUE) {

  # adjust filters
  yr = year
  sp = species
  area = toupper(area)
  if(any(area %in% c('GOA'))) {
    a1 <- c('Western Gulf of Alaska', 'Central Gulf of Alaska', 'West Yakutat', 'East Yakutat/Southeast')
  } else {a1 <- NULL}
  if(any(area %in% c('BS', 'BSAI'))) {
    a2 <- c('Bering Sea')} else {a2 <- NULL}
  if(any(area %in% c('AI', 'BSAI'))) {
    a3 <- c('Aleutians')} else {a3 <- NULL}
  area <- c(a1, a2, a3)

  # message center
  if(any(!area %in% c('Western Gulf of Alaska', 'Central Gulf of Alaska', 'West Yakutat', 'East Yakutat/Southeast', 'Bering Sea', 'Aleutians'))) {
    stop("the appropriate args for area are 'goa', 'bs', 'ai', or a combo. defaults to c('goa', 'bs', 'ai')")
  }
  if(is.logical(use_historical)==FALSE) {
    stop('appropriate args for use_historical are TRUE or FALSE')
  } else {
    if(isFALSE(use_historical)) {
      srv <- 'United States'
    } else {
      srv <- c('United States', 'Japan')
    } 
  }
  # special sablefish
  if(any(species %in% 20510)) {
    strata <- '_3_to_7'
    depred <- '_depred'
    message("sablefish rpn-weighted length frequencies:\n
            -are corrected for sperm whale depredation and area summaries \n
            -only include data from depth strata 3-7 (201-1000 m) \n")
    if(base::length(species) > 1) {
      warning("we do not recommended querying sablefish with other species. \n 
               output will only include data from strata 3-7, which may be inappropriate for non-sablefish species.")
    }
  } else {
    strata <- '_all_strata'
    depred <- ''
  }

  # length freq table 
  aa <- dplyr::tbl(db, dplyr::sql(paste0('afsc.lls_length_rpn_by_area', strata, depred))) %>%
    dplyr::rename_with(tolower) 

  # area look up table
  bb <- dplyr::tbl(db, dplyr::sql('afsc.lls_area_view')) %>% 
    dplyr::rename_with(tolower) 

  # join, filter and query
  table <- aa %>% 
    dplyr::left_join(dplyr::distinct(bb, fmp = fmp_management_area, council_sablefish_management_area, 
                                     council_management_area, geographic_area_name, exploitable)) %>%
    dplyr::filter(year <= yr,                  
                  species_code %in% species,
                  council_sablefish_management_area %in% area,
                  country %in% srv,
                  # only include geographic/stratum areas that are used for rpns calc
                  exploitable == 1,
                  # records with length = 999 account for instances when there
                  # was catch in a stratum/station but no lengths collected. the
                  # 999 lengths ensure rpns sum properly to the area-level but
                  # should not be included in the length compositions.
                  length < 999) %>%
    dplyr::arrange(year)

  if(isTRUE(save)){
    if(isFALSE(dir.exists(here::here(year, "data")))) {
      stop("you must run afscdata::setup_folders() before you can save to the default location")
    }
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", paste0("lls_rpn_length_data.csv")), 
                         delim = ",")
    capture.output(dplyr::show_query(table), 
                   file = here::here(year, "data", "sql", paste0("lls_rpn_length_sql.txt")))

    message("longline survey rpn-weighted length frequencies can be found in the data/raw folder,\n
            and the associated query is saved in data/sql")
  } else if(isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }

}
 
#' query nmfs longine survey specimen data (age, length, weight, sex, maturity).
#' only available for sablefish!
#' 
#' longline database documentation available on
#' \href{https://akfinbi.psmfc.org/analyticsRes/Documentation/Database_Background_Instructions_AKFIN_20210915.pdf}{akfin}
#' 
#' source table on akfin:
#' afsc.lls_age_view
#' 
#' @param year max year to retrieve data from 
#' @param area options are 'goa', 'bs', 'ai', or a combo. default=c('goa', 'bs', 'ai')
#' @param use_historical T/F include historical Japanese survey data in the
#'   results (default: false)
#' @param db  the database to query (akfin)
#' @param print_sql outputs the sql query instead of calling the data (default: false) - save must be false
#' @param save save the file in designated folder, if FALSE outputs to global environment
#'  
#' @return saves lls sablefish specimen data as data/raw/lls_specimen_data.csv or outputs to
#'   the global environment. also saves a copy of the SQL code used for the
#'   query and stores it in the data/sql folder.
#' @export q_lls_sable_specimen
#'
#' @examples
#' \dontrun{
#' # sablefish specimen data in 1996 (first year ages were collected in the goa domestic survey) 
#' # in the gulf of alaska
#' 
#' q_lls_specimen(year=2000, area="goa", db=db)
#' } 
q_lls_sable_specimen <- function(year, area=c('goa', 'bs', 'ai'), use_historical=FALSE, 
                         db=akfin, print_sql=FALSE, save=TRUE) {

  # adjust filters
  yr = year
  area = toupper(area)
  if(any(area %in% c('GOA'))) {
    a1 <- c('Western Gulf of Alaska', 'Central Gulf of Alaska', 'West Yakutat', 'East Yakutat/Southeast')
  } else {
    a1 <- NULL
    }
  if(any(area %in% c('BS', 'BSAI'))) {
    a2 <- c('Bering Sea')
    } else {
      a2 <- NULL
      }
  if(any(area %in% c('AI', 'BSAI'))) {
    a3 <- c('Aleutians')
    } else {
      a3 <- NULL
      }
  area <- c(a1, a2, a3)

  # message center
  if(any(!area %in% c('Western Gulf of Alaska', 'Central Gulf of Alaska', 'West Yakutat', 'East Yakutat/Southeast', 'Bering Sea', 'Aleutians'))) {
    stop("the appropriate args for area are 'goa', 'bs', 'ai', or a combo. defaults to c('goa', 'bs', 'ai')")
  }
  if(is.logical(use_historical)==FALSE) {
    stop('appropriate args for use_historical are TRUE or FALSE')
  } else {
    if(isFALSE(use_historical)) {
      srv <- 'United States'
    } else {
      srv <- c('United States', 'Japan')
    } 
  }

  table = dplyr::tbl(db, dplyr::sql('afsc.lls_age_view')) %>%
    dplyr::rename_with(tolower) %>% 
    dplyr::filter(year <= yr, 
                  country %in% srv,
                  council_sablefish_management_area %in% area) %>% 
    dplyr::arrange(year)

  if(isTRUE(save)){
    if(isFALSE(dir.exists(here::here(year, "data")))) {
      stop("you must run afscdata::setup_folders() before you can save to the default location")
    }
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", paste0("lls_specimen_data.csv")), 
                         delim = ",")
    capture.output(dplyr::show_query(table), 
                   file = here::here(year, "data", "sql", paste0("lls_specimen_sql.txt")))

    message("sablefish longline survey specimen data can be found in the data/raw folder,\n
            and the associated query is saved in data/sql")
  } else if(isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }

}





