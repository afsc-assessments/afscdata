#' @param year max year to retrieve data from 
#' @param species species group code e.g., "DUSK" or numeric agency values e.g. c("131", "132") - must be either all 4 digit or 3 digit codes
#' @param area sample area "GOA", "AI" or "BS" (or combos)
#' @param db data server to connect to (akfin)
#' @param area fmp_area (GOA, BSAI) or fmp_subarea (BS, AI, WG, CG, WY, EY, SE) - also available (SEI, PWSI)
#' @param add_fields add other columns to the database (must currently exist on server)
#' @param print_query outputs the sql query instead of calling the data
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: TRUE)
#' 
q_catch <- function(year, species, area, db, add_fields=NULL, print_query=FALSE, save=TRUE) {
  
  area = toupper(area)
  area = if(isTRUE(area == "GOA")){
    area = c("WG", "CG", "WY", "EY", "SE")
  } else if(isTRUE(area=="BSAI")){
    area = c("BS", "AI")
  } else {
    area
  }
  
  yr = year
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
  
  table <- tbl(db, sql("council.comprehensive_blend_ca")) %>% 
              dplyr::rename_with(tolower) %>% 
              dplyr::select(!!!cols) %>% 
              dplyr::filter(year <= yr, fmp_subarea %in% area)
  
  
  if(isTRUE(sum(stringi::stri_length(species) - 3) == 0)){
    dplyr::filter(table, agency_species_code %in% species) -> table
  } else {
    dplyr::filter(table, species_group_code %in% species) -> table
  }

  if(isTRUE(save)) {
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", "fsh_catch_data.csv"), 
                         delim = ",")
    message("fishery catch data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_query)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
  

}


q_bts_length <- function(year, species, area, db, print_query=FALSE, save=TRUE){
  
  # globals
  yr = year
  area = toupper(area)
  
  # pull data sources
  dplyr::tbl(db, sql("racebase.cruise")) %>% 
    rename_with(tolower) -> aa
  
  dplyr::tbl(db, sql("racebase.haul")) %>% 
    dplyr::rename_with(tolower) -> bb
  
  dplyr::tbl(db, sql("racebase.length")) %>% 
    dplyr::rename_with(tolower) -> cc
  # join, filter and query
  dplyr::select(aa, cruisejoin, region, survey_name, start_date) %>% 
    dplyr::left_join(dplyr::select(bb, cruisejoin, hauljoin, end_latitude, 
                                   end_longitude, bottom_depth, abundance_haul, 
                                   stratum)) %>%
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
    message("bottom trawl survey length data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_query)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
  
}


q_bts_specimen <- function(year, species, area, db, print_query=FALSE, save=TRUE){
  
  # globals
  yr = year
  area = toupper(area)
  
  # pull data sources
  dplyr::tbl(db, sql("racebase.cruise")) %>% 
    rename_with(tolower) -> aa
  
  dplyr::tbl(db, sql("racebase.haul")) %>% 
    dplyr::rename_with(tolower) -> bb
  
  dplyr::tbl(db, sql("racebase.specimen")) %>% 
    dplyr::rename_with(tolower) -> cc
  
  # join, filter and query
  dplyr::select(aa, cruisejoin, region, survey_name, start_date) %>% 
    dplyr::left_join(dplyr::select(bb, cruisejoin, hauljoin, end_latitude, 
                                   end_longitude, bottom_depth, abundance_haul, 
                                   stratum)) %>%
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
    message("bottom trawl survey specimen data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_query)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
  
  
}


