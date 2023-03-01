#' @param year max year to retrieve data from 
#' @param species species group code e.g., "DUSK" or numeric agency values e.g. c(131, 132)
#' @param area sample area "GOA", "AI" or "BS" (or combos)
#' @param db data server to connect to
#' @param area fmp_area or fmp_subarea (GOA, BSAI, BS, AI, WG, CG, WY, EY, SE) - also available (SEI, PWSI)
#' @param add_fields add other columns to the database (must currently exist on server)
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: TRUE)
#' 
q_test <- function(year, species, area, db, add_fields=NULL, save=TRUE) {
  
  area = toupper(area)
  if(area=="GOA") area = c("WG", "CG", "WY", "EY", "SE")
  if(area=="BSAI") area = c("BS", "AI")
  
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
  
  
  if(is.numeric(species)){
    table %>% 
      filter(agency_species_code %in% species) %>% 
      collect()
  } else {
    table %>% 
      filter(species_group_code %in% species) %>% 
      collect()
  }

  
  
}
