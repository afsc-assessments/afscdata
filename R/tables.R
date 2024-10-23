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
    dplyr::select(year, 
                  species = species_group_name, 
                  psc = pscnq_estimate, 
                  fmp_subarea, trip_target_code) %>% 
    dplyr::filter(year >= yr-4, year <= yr, 
                  fmp_subarea %in% area,
                  trip_target_code %in% target) %>% 
    dplyr::group_by(year, species) %>% 
    dplyr::summarise(psc = round(sum(psc, na.rm = T),3)) %>% 
    dplyr::collect() %>% 
    tidytable::pivot_wider(names_from = year, values_from = psc) -> psc
  
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
#' akfin = afscdaya::connect()
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
    dplyr::select(year, fmp_subarea, trip_target_code, 
                  species = nontarget_group_name, 
                  count = nontarget_estimate_count, 
                  weight = nontarget_estimate_weight) %>% 
    dplyr::filter(trip_target_code %in% target,
                  year >= yr-4, year <= yr,
                  fmp_subarea %in% area) %>% 
    dplyr::group_by(year, species) %>% 
    dplyr::summarise(weight = round(sum(weight, na.rm = T), 3),
                     count = round(sum(count, na.rm = T), 3)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(weight = ifelse(is.na(weight), count, weight)) %>% 
    dplyr::select(-count) %>% 
    dplyr::collect() %>% 
    tidytable::pivot_wider(names_from = year, values_from = weight) -> tbl
  
  if(isTRUE(save)){
    vroom::vroom_write(tbl, here::here(year, "data", "output", "nontarget_catch.csv"),
                       delim = ",")
    message("nontarget table written to data/output folder.")
  } else {
    tbl
  }
}