#' query fishery catch data
#'
#' Note that this function will produce results that are confidential. They can be hidden from git/GitHub by adding fsh_catch_data.csv to your .gitignore file
#'
#' @param year max year to retrieve data from 
#' @param species species group code e.g., "DUSK" or numeric agency values e.g. c("131", "132") - must be either all 4 digit or 3 digit codes
#' @param area fmp_area (GOA, BSAI) or fmp_subarea (BS, AI, WG, CG, WY, EY, SE) - also available (SEI, PWSI)
#' @param db data server to connect to (akfin)
#' @param add_fields add other columns to the database (must currently exist on server). "*" will return all table columns available
#' @param print_sql outputs the sql query instead of calling the data
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: TRUE)
#' 
q_catch <- function(year, species, area, db, add_fields=NULL, print_sql=FALSE, save=TRUE) {
  
  # globals 
  area = toupper(area)
  area = if(isTRUE(area == "GOA")){
    area = c("WG", "CG", "WY", "EY", "SE")
  } else if(isTRUE(area=="BSAI")){
    area = c("BS", "AI")
  } else {
    area
  }
  
  yr = year
  
  # select columns to import
  if(add_fields == "*") {
    table <- dplyr::tbl(db, dplyr::sql("council.comprehensive_blend_ca")) %>% 
      dplyr::rename_with(tolower) %>% 
      dplyr::filter(year <= yr, fmp_subarea %in% area)
  } else {
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
    if(isFALSE(dir.exists(here::here(year, "data")))) stop("you must run afscdata::setup_folders() before you can save to the default location")
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", "fsh_catch_data.csv"), 
                         delim = ",")
    
    capture.output(show_query(table), 
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
#' something about the species names and area codes?
#' pulls data from akfin.pre1991.foreign_blend
#'
#' @param year  max year to retrieve data from 
#' @param species common name (e.g., "sablefish" or "all flounders") - can call multiple species
#' @param area numeric area digit code
#' @param db data server to connect to (akfin)
#' @param print_sql outputs the sql query instead of calling the data (default: false)
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: true)
#'
#' @return
#' @export
#'
#' @examples
q_for_catch <- function(year, species, area, db, print_sql=FALSE, save=TRUE){
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
    capture.output(show_query(table), 
                   file = here::here(year, "data", "sql", "for_catch_sql.txt"))
    
    message("foreign catch data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
}

#' query bottom trawl survey length data from the AFSC server
#' 
#'
#' @param year  max year to retrieve data from 
#' @param species 5 digit species code (e.g., 10110) - can place multiple in a vector c(10110, 10130)
#' @param area bs, hwc, wc, hg, ai, goa, or hbs
#' @param db data server to connect to (afsc)
#' @param print_sql outputs the sql query instead of calling the data (default: false)
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: true)
#' 
q_bts_length <- function(year, species, area, db, print_sql=FALSE, save=TRUE){
  
  # globals
  yr = year
  area = toupper(area)
  
  # pull data sources
  dplyr::tbl(db, dplyr::sql("racebase.cruise")) %>% 
    rename_with(tolower) -> aa
  
  dplyr::tbl(db, dplyr::sql("racebase.haul")) %>% 
    dplyr::rename_with(tolower) -> bb
  
  dplyr::tbl(db, dplyr::sql("racebase.length")) %>% 
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
    capture.output(show_query(table), 
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
#' @param area bs, hwc, wc, hg, ai, goa, or hbs
#' @param db data server to connect to (afsc)
#' @param print_sql outputs the sql query instead of calling the data (default: false)
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: true)
#' 
q_bts_specimen <- function(year, species, area, db, print_sql=FALSE, save=TRUE){
  
  # globals
  yr = year
  area = toupper(area)
  
  # pull data sources
  dplyr::tbl(db, dplyr::sql("racebase.cruise")) %>% 
    rename_with(tolower) -> aa
  
  dplyr::tbl(db, dplyr::sql("racebase.haul")) %>% 
    dplyr::rename_with(tolower) -> bb
  
  dplyr::tbl(db, dplyr::sql("racebase.specimen")) %>% 
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
    capture.output(show_query(table), 
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
#' bs = bering sea + northwest 1987-present (includes nw stations) - recommended
#' bsslope = bering sea slope 
#' nbs = northern bering sea
#' ai = aleutian islands 
#' goa = gulf of alaska
#' old_bs = bering sea standard 1982-present (minus ~20 stations in nw) - not recommended 
#' 
#' @param year  max year to retrieve data from 
#' @param area options are bs, bsslope, nbs, ai, goa, old_bs - can only call a single area
#' @param species 5 digit afsc species code(s) e.g., 79210 or c(79210, 10110)
#' @param by "depth", "stratum", "area", "total", "inpfc", "inpfc_depth" - only available for goa/ai (default: "total") 
#' @param db  the database to query (akfin)
#' @param print_sql outputs the sql query instead of calling the data (default: false)
#' @param save save the file in designated folder, if FALSE outputs to global environment
#' 
q_bts_biomass <- function(year, area, species, by='total', db, print_sql=FALSE, save=TRUE) {
  
  # adjust filters
  yr = year
  area = toupper(area)
  by = tolower(by)
  
  # message center
  if(by!="total") {
    if(by %in% c("depth", "stratum", "area", "total", "inpfc", "inpfc_depth")==FALSE){ 
      stop("appropriate args for by are: 'depth', 'stratum' 'area', or 'total'.\n 
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
  } else if(area %in% c("GOA", "AI") & by=="depth"){
    table = dplyr::tbl(db, dplyr::sql("afsc.race_biomassdepthaigoa"))
  } else if(area %in% c("GOA", "AI") & by=="area"){
    table = dplyr::tbl(db, dplyr::sql("afsc.race_biomassareaaigoa"))    
  } else if(area %in% c("GOA", "AI") & by=="stratum"){
    table = dplyr::tbl(db, dplyr::sql("afsc.race_biomassstratumaigoa"))    
  } else if(area %in% c("GOA", "AI") & by=="inpfc"){
    table = dplyr::tbl(db, dplyr::sql("afsc.race_biomassinpfcaigoa")) 
  } else if(area %in% c("GOA", "AI") & by=="inpfc_depth"){
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
  if(area %in% c("ai", "goa")) id = paste0(by, "_")
  id = paste0(area, "_", id)
  
  if(isTRUE(save)){
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", paste0(id, "bts_biomass_data.csv")), 
                         delim = ",")
    capture.output(show_query(table), 
                   file = here::here(year, "data", "sql", paste0(id, "bts_biomass_sql.txt")))
    
    message("bottom trawl survey length data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
  
}

#' query nfms longline survey relative population numbers or weights
#' 
#' available source tables on akfin:
#' lls_area_stratum_rpn
#' lls_area_stratum_rpn_depred
#' lls_area_rpn_all_strata
#' lls_area_rpn_3_to_7
#' lls_area_rpn_3_to_7_depred
#' lls_council_sablefish_area_all_strata
#' lls_council_sablefish_area_3_to_7_depred
#' lls_fmp_subarea_all_strata
#' lls_fmp_subarea_3_to_7_depred
#' lls_fmp_all_strata -- not available using this fcn
#' lls_ak_wide_3_to_7_depred
#'  
#' @param year  max year to retrieve data from 
#' @param area options are 'goa', 'bs', 'ai', or a combo. default=c('goa', 'bs', 'ai')
#' @param species 5 digit afsc/race species code(s) e.g., 79210 or c(79210, 10110)
#' @param by 'depth', 'geoarea', 'councilarea', 'fmpsubarea', or 'akwide'
#'   (only for sablefish). default = 'fmpsubarea' - can only call a single area
#' @param use_historical T/F include historical Japanese survey data in the query (defaults to FALSE) 
#' @param db  the database to query (akfin)
#' @param print_sql outputs the sql query instead of calling the data (default: false)
#' @param save save the file in designated folder, if FALSE outputs to global environment
#' 
q_lls_rpn <- function(year, area, species, by='fmpsubarea', use_historical=FALSE, db, print_sql=FALSE, save=TRUE) {
  
  # adjust filters
  yr = year
  sp = species
  if(!exists(quote(area)) | by %in% c('akwide', 'AKWIDE')) {area <- c('goa', 'bs', 'ai')}
  if(any(area == 'bsai')) {area <- c('bs', 'ai')}
  area = toupper(area)
  by = tolower(by)
  
  # message center
  if(!any(area %in% c('GOA', 'BS', 'AI'))) {
    stop("the appropriate args for area are 'goa', 'bs', 'ai', or a combo. defaults to c('goa', 'bs', 'ai')")
  }

  if(any(!by %in% c('depth', 'geoarea', 'councilarea', 'fmpsubarea', 'akwide'))){ 
    if(length(by) > 1){
      stop("appropriate args for by are: 'depth', 'geoarea', 'councilarea', 'fmpsubarea',  or 'akwide'. only a single arg may be used.")
    }
  }
  
  if(is.logical(use_historical)==FALSE) {
    stop('appropriate args for use_historical are TRUE or FALSE')
  } else {
    if(isFALSE(use_historical)) {
      srv <- 'United States'
    } else {srv <- c('United States', 'Japan')
      } 
  }
  # special sablefish
  if(species == 20510) {
    strata <- '_3_to_7'
    depred <- '_depred'
    message("sablefish rpns:\n
            -are corrected for sperm whale depredation and area summaries exlude gully stations (stratum 2b) \n
            -use interpolated bs/ai values in alternating survey years when aggregated to the fmp or ak-wide level \n
            -assume fixed rpn/rpw data in the ai (1990-1995) and bs (1990-1996) when no bs/ai surveys occurred\n
            -assume fixed ak-wide rpn/rpws from 1979-1994 for the historical Japanese survey")
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
      # Code used to flag geographic areas for calculations of relative
      # population weights and numbers.
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
                  country %in% srv)
    
  # set up table-specific area filters
  area_lkup = data.frame(area = c('BS', 'AI', 'GOA', 'GOA', 'GOA', 'GOA'),
                         councilarea = c('Bering Sea', 'Aleutians', 'East Yakutat/Southeast', 'West Yakutat', 'Central Gulf of Alaska', 'Western Gulf of Alaska'),
                         fmpsubarea = c('Bering Sea', 'Aleutians', 'Eastern Gulf of Alaska', 'Eastern Gulf of Alaska', 'Central Gulf of Alaska', 'Western Gulf of Alaska')) %>% 
    dplyr::filter(area %in% area)

  if(by %in% c('depth', 'geoarea', 'councilarea')) {
    table <- table %>% 
      dplyr::filter(council_sablefish_management_area %in% unique(area_lkup$councilarea)) 
  }
  if(by %in% c('fmp_subarea')) {
    table <- table %>% 
      dplyr::filter(council_sablefish_management_area %in% unique(area_lkup$fmpsubarea)) 
  }
  
  
  # prefix area and type (for goa/ai) to file name
  area = tolower(area)
  id = NULL
  if(by == 'akwide') {
    id = by
  } else {
    if(identical(sort(area), c('ai', 'bs', 'goa'))) {
      id = paste0('allareas_by_', by)
    }
    if(identical(sort(area), c('ai', 'bs',))) {
      id = paste0('bsai_by_', by)
    }
    
  }
  
  
  by
  id = paste0(area, "_", id)
  
  if(isTRUE(save)){
    dplyr::collect(table) %>% 
      vroom::vroom_write(here::here(year, "data", "raw", paste0(id, "lls_rpn_data.csv")), 
                         delim = ",")
    capture.output(show_query(table), 
                   file = here::here(year, "data", "sql", paste0(id, "lls_rpn_sql.txt")))
    
    message("longline survey data can be found in the data/raw folder")
  } else if (isFALSE(save) & isFALSE(print_sql)) {
    dplyr::collect(table)
  } else {
    dplyr::show_query(table)
    message("this sql code is passed to the server")
  }
  
  }
  
  
  
  