#' query prohibited species catch (psc) estimate
#'
#' @param year assessment year
#' @param target targeted species: 'p' = pollock-mid, 'b' = pollock-bottom, x' = rex, 'h' = shallow flats, 'k' = rockfish, 'w' = arrowtooth, 'c' = pcod, 'i' = halibut
#' @param area fmp_area (GOA, BSAI) or fmp_subarea (BS, AI, WG, CG, WY, EY, SE) - also available (SEI, PWSI)
#' @param db data server to connect to (akfin)
#' @param save save the file in designated folder (default = T) or the global environment
#'
#' @return a csv of prohibited species catch by trip target group, saved in the data/output folder
#' @export 
#' @description prohibited species catch (PSC) estimates reported in tons for halibut and herring, counts for salmon, crabs and other fish. Note that you can combine trip target codes c("k", "x") and regions - though results will be lumped together
#' @examples
#' \dontrun{
#' akfin = connect()
#' q_psc(year=2022, target="k", area="goa", db=akfin, save=FALSE)
#' disconnect(akfin)
#' }
#'
q_psc <- function(year, target, area, db, save = TRUE) {
  # globals 
  area = toupper(area)
  area = if(isTRUE(area == "GOA")){
    area = c("WG", "CG", "WY", "EY", "SE")
  } else if(isTRUE(area=="BSAI")){
    area = c("BS", "AI")
  } else {
    area
  }
  
  target = toupper(target)
  yr = year
  
  # call table
  dplyr::tbl(db, dplyr::sql("council.comprehensive_psc")) %>% 
    dplyr::rename_with(tolower) %>% 
      dplyr::filter(trip_target_code %in% target,
                  year >= yr-4, year <= yr,
                  fmp_subarea %in% area) %>% 
    dplyr::select(year, fmp_subarea, trip_target_code, 
                  species = species_group_name, 
                  psc = pscnq_estimate,
                  vessel_id, processor = processor_permit_id) %>% 
    dplyr::collect() %>% 
    dplyr::group_by(year, species) %>% 
    dplyr::summarise(psc = round(sum(psc, na.rm = T),3),
                     n_vessels = dplyr::n_distinct(vessel_id),
                     n_processor= dplyr::n_distinct(processor),
                     .groups = "drop" ) %>% 
    dplyr::mutate(psc = dplyr::if_else(n_vessels <= 2 | n_distinct(n_processor)<=2, "conf.", as.character(psc))) %>% 
    dplyr::select(-c(n_vessels, n_processor)) %>% 
    tidytable::pivot_wider(names_from = year, values_from = psc, values_fill = "0") -> psc
  
  if(isTRUE(save)){
    vroom::vroom_write(psc, here::here(year, "data", "output", "psc_catch.csv"),
                delim = ",")
    message("PSC table written to data/output folder.")
  } else {
    psc
  }
}

#' query non-target species catch estimate
#'
#' @param year assessment year
#' @param target targeted species: 'p' = pollock-mid, 'b' = pollock-bottom, x' = rex, 'h' = shallow flats, 'k' = rockfish, 'w' = arrowtooth, 'c' = pcod, 'i' = halibut
#' @param area fmp_area (GOA, BSAI) or fmp_subarea (BS, AI, WG, CG, WY, EY, SE) - also available (SEI, PWSI)
#' @param db data server to connect to (akfin)
#' @param save save the file in designated folder (default = T) or the global environment
#'
#' @return a csv of non-target species catch by trip target group, saved in the data/output folder
#' @export 
#' @description non-target catch estimates by weight (or numbers)
#' @examples
#' \dontrun{
#' akfin = afscdata::connect()
#' q_nontarget(year=2022, target="k", area="goa", db=akfin, save=FALSE)
#' disconnect(akfin)
#' }
#'
q_nontarget <- function(year, target, area, db, save = TRUE) {
  # globals 
  area = toupper(area)
  area = if(isTRUE(area == "GOA")){
    area = c("WG", "CG", "WY", "EY", "SE")
  } else if(isTRUE(area=="BSAI")){
    area = c("BS", "AI")
  } else {
    area
  }
  
  target = toupper(target)
  yr = year
  
  # call table
  dplyr::tbl(db, dplyr::sql("council.comprehensive_nontarget")) %>% 
    dplyr::rename_with(tolower) %>% 
      dplyr::filter(trip_target_code %in% target,
                  year >= yr-4, year <= yr,
                  fmp_subarea %in% area) %>% 
    dplyr::select(year, fmp_subarea, trip_target_code, 
                  species = nontarget_group_name, 
                  count = nontarget_estimate_count, 
                  weight = nontarget_estimate_weight,
                  vessel_id, processor = processor_permit_id) %>% 
    dplyr::collect() %>% 
    dplyr::group_by(year, species) %>% 
    dplyr::summarise(weight = round(sum(weight, na.rm = T), 3),
                    count = round(sum(count, na.rm = T), 3),
                     n_processor= dplyr::n_distinct(processor),
                     n_vessels = dplyr::n_distinct(vessel_id),
                     .groups = "drop" ) %>% 
    dplyr::mutate(weight = ifelse(is.na(weight), count, weight),
                  weight = dplyr::if_else(n_vessels <= 2 | n_distinct(n_processor)<=2, "conf.", as.character(weight))) %>% 
    dplyr::select(-c(count, n_vessels, n_processor)) %>% 
    tidytable::pivot_wider(names_from = year, values_from = weight, values_fill = "0") -> tbl
  
  if(isTRUE(save)){
    vroom::vroom_write(tbl, here::here(year, "data", "output", "nontarget_catch.csv"),
                       delim = ",")
    message("nontarget table written to data/output folder.")
  } else {
    tbl
  }
}


#' query incidental FMP species catch estimates
#'
#' @param year assessment year
#' @param target targeted species: 'p' = pollock-mid, 'b' = pollock-bottom, x' = rex, 'h' = shallow flats, 'k' = rockfish, 'w' = arrowtooth, 'c' = pcod, 'i' = halibut
#' @param area fmp_area (GOA, BSAI) or fmp_subarea (BS, AI, WG, CG, WY, EY, SE) - also available (SEI, PWSI)
#' @param db data server to connect to (akfin)
#' @param save save the file in designated folder (default = T) or the global environment
#'
#' @return a csv of non-target species catch by trip target group, saved in the data/output folder
#' @export 
#' @description non-target catch estimates by weight (or numbers)
#' @examples
#' \dontrun{
#' akfin = afscdata::connect()
#' q_incidental(year=2022, target="k", area="goa", db=akfin, save=FALSE)
#' disconnect(akfin)
#' }
#'
q_incidental <- function(year, target, area, db, save = TRUE) {
  # globals 
  area = toupper(area)
  area = if(isTRUE(area == "GOA")){
    area = c("WG", "CG", "WY", "EY", "SE")
  } else if(isTRUE(area=="BSAI")){
    area = c("BS", "AI")
  } else {
    area
  }
  
  target = toupper(target)
  yr = year
  
  # call table
  dplyr::tbl(db, dplyr::sql("council.comprehensive_blend_ca")) %>% 
    plyr::rename_with(tolower) %>% 
      dplyr::filter(trip_target_code %in% target,
                  year >= yr-4, year <= yr,
                  fmp_subarea %in% area) %>% 
    dplyr::select(year, fmp_subarea, trip_target_code, 
                  species = species_group_name, 
                  processor = processor_permit_id,
                  weight = weight_posted,
                  vessel_id) %>% 
    dplyr::collect() %>% 
    dplyr::group_by(year, species) %>% 
    dplyr::summarise(weight = round(sum(weight, na.rm = T), 3),
                     n_processor= dplyr::n_distinct(processor),
                     n_vessels = dplyr::n_distinct(vessel_id),
                     .groups = "drop" ) %>% 
    dplyr::mutate(weight = dplyr::if_else(n_vessels <= 2 | n_distinct(n_processor)<=2, "conf.", as.character(weight))) %>% 
    dplyr::select(-c(n_vessels, n_processor)) %>% 
    tidytable::pivot_wider(names_from = year, values_from = weight, values_fill = "0") -> tbl
  
  if(isTRUE(save)){
    vroom::vroom_write(tbl, here::here(year, "data", "output", "incidental_catch.csv"),
                       delim = ",")
    message("incidental table written to data/output folder.")
  } else {
    tbl
  }
}

# discards ----
#' query fishery discard data from AKFIN server
#'
#'
#' @param year assessment year
#' @param species species group code e.g., "DUSK" or numeric agency values e.g. c("131", "132") - must be either all 4 digit or 3 digit codes
#' @param area fmp_area (GOA, BSAI) or fmp_subarea (BS, AI, WG, CG, WY, EY, SE) - also available (SEI, PWSI), can use all fmp_areas or all fmp_subareas, but don't mix the two
#' @param db data server to connect to (akfin)
#' @param save saves a file to the data/output folder, otherwise sends output to global enviro (default: TRUE)
#' 
#' @return saves discard data as data/raw/fish_discard_data.csv or outputs to the global environment
#' @export 
#' @examples 
#' \dontrun{
#' db <- afscdata::connect()
#' q_discards(year=2022, species="NORK", area="goa", db=db)
#' }
#'  
q_discards <- function(year, species, area, db, save=TRUE) {
  
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
  
  
  # select columns to import
    table <- dplyr::tbl(db, dplyr::sql("council.comprehensive_blend_ca")) %>% 
      dplyr::rename_with(tolower) %>% 
      dplyr::select(fmp_subarea, agency_species_code, agency_group_code, retained_or_discarded) %>% 
      dplyr::filter(fmp_subarea %in% area)

  # filter species
  if(isTRUE(sum(stringi::stri_length(species) - 3) == 0)){
    dplyr::filter(table, agency_species_code %in% species) -> table
  } else {
    dplyr::filter(table, species_group_code %in% species) -> table
  }
  

  dplyr::collect(table) %>% 
  dplyr::arrange(year) %>% 
  tidyr::pivot_wider(names_from = retained_or_discarded, values_from = wt) %>% 
  dplyr::summarise(discard_percent = D / (D+R), .by = year) -> tbl
  
  # output
  if(isTRUE(save)){
    vroom::vroom_write(tbl, here::here(year, "data", "output", "discards.csv"),
                       delim = ",")
    message("discards table written to data/output folder.")
  } else {
    tbl
  }
}

