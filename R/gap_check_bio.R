#' compare GAP updated survey biomass to (now) retired design-based biomass estimates
#'
#' @param year current year
#' @param species afsc species codes e.g., 30420
#' @param area options = ai, goa, ebs, bss, nbs
#' @param type = region, subarea, area, stat_area, stratum, inpfc, inpfc_depth, depth, reg_area_depth
#'
#' @return a list with orig values, gap values, and a basic report
#' @export
#'
#' @examples
#' \dontrun{
#' out <- gap_check_bio(year = 2024, species = 30420, type = 'total', area = 'AI' )
#' out$report
#' }
gap_check_bio <- function(year, species, area, type) {

  area = toupper(area)
  type = tolower(type)
  data.frame(id = c('AI', 'GOA', 'EBS', 'BSS', 'NBS'),
             srv_id = c(52, 47, 98, 78, 143)) %>%  
    dplyr::filter(id %in% area) %>% 
    dplyr::pull(srv_id) -> srv_id
  
  if(area == 'EBS') area = 'BS'
  if(area =='BSS') area = 'BSSLOPE'
  
  db = connect()
  db2 = connect('afsc')
  # og data 
  orig = q_bts_biomass(year, species = species, type = type, area = area, db = db, save = FALSE)
  # gap data '

  
  if(type == 'area') {
    aid = 'REGULATORY AREA'
  } else if(type == 'stat_area') {
    aid = 'NMFS STATISTICAL AREA'
  } else if(type == 'reg_area_depth') {
    aid = 'REGULATORY AREA BY DEPTH'
  } else if(type == 'inpfc_depth') {
    aid = 'INPFC BY DEPTH'
  } else if(type %in% c('REGION', 'total')) {
    aid = 'REGION'
  } else {
    aid = toupper(type)
  }
  
  dplyr::tbl(db2, dplyr::sql('gap_products.area')) %>% 
    rename_all(tolower) %>% 
    filter(survey_definition_id %in% srv_id,
           area_type == aid,
           design_year < year) %>% 
    left_join(dplyr::tbl(db2, dplyr::sql('gap_products.biomass')) %>% 
                rename_all(tolower) %>% 
                filter(species_code %in% species)) %>% 
    collect() -> gap
  
  if(type=='total'){
    orig %>% 
      group_by(year) %>% 
      summarise(tot = sum(total_biomass, na.rm = T),
                var = sum(biomass_var, na.rm = T)) %>% 
      left_join(gap %>%
                  group_by(year) %>% 
                  summarise(tot_g = sum(biomass_mt, na.rm = T),
                            var_g = sum(biomass_var, na.rm = T))) %>% 
      mutate(bio_diff = round(tot - tot_g, 3),
             var_diff = round(var - var_g, 3)) -> report
    
  } else if(type =='stratum'){
    orig %>% 
      group_by(year) %>% 
      summarise(tot = sum(stratum_biomass, na.rm = T),
                var = sum(biomass_var, na.rm = T)) %>% 
      left_join(gap %>%
                  group_by(year) %>% 
                  summarise(tot_g = sum(biomass_mt, na.rm = T),
                            var_g = sum(biomass_var, na.rm = T))) %>% 
      mutate(bio_diff = round(tot - tot_g, 3),
             var_diff = round(var - var_g, 3)) -> report
  } else {
    
    orig %>% 
      group_by(year) %>% 
      summarise(tot = sum(area_biomass, na.rm = T),
                var = sum(biomass_var, na.rm = T)) %>% 
      left_join(gap %>%
                  group_by(year) %>% 
                  summarise(tot_g = sum(biomass_mt, na.rm = T),
                            var_g = sum(biomass_var, na.rm = T))) %>% 
      mutate(bio_diff = round(tot - tot_g, 3),
             var_diff = round(var - var_g, 3)) -> report
  }
  
  list(orig = orig, gap = gap, report = report)
}