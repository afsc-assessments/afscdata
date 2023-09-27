#' raw data query for BSAI octopus
#'
#' @param year assessment year
#'
#' @return a suite of raw data .csv files and a time stamp of when the query was done 
#' @export bsai_octopus
#'
#' @examples
#' \dontrun{
#' bsai_octopus(year = 2023)
#'}
bsai_octopus <- function(year) {
  
  # these functions are being "handrolled" until I get the base functions to pull from the gap_products tables
  # globals 
  area = "bsai"
  srv_area = c(99900, 99904, 99905)
  
  species = 870
  afsc_species = c(78010, 78011, 78012, 78013, 78020, 78021, 78022, 78023, 
                   78030, 78040, 78210, 78300, 78301, 78352, 78353, 78403, 
                   78404, 78452, 78454, 78455)
  # giant = 78403
  # smoothskin = 78012
  # octopus unid = 78010
  
  # 99905 = EBS slope
  # 99904 = AI all 
  # 99903 = GOA
  # 99902 = NBS
  # 99901 = EBS standard - don't use
  # 99900 = EBS+NW
  

  # catch 
  db = connect()
  q_catch(year=year, species=species, area=area, db=db)

 # survey biomass
  db2 = connect("afsc")
  dplyr::tbl(db2, dplyr::sql("gap_products.biomass")) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::filter(area_id %in% srv_area, 
                  species_code %in% afsc_species) %>% 
    dplyr::collect()  %>% 
    vroom::vroom_write(here::here(year, "data", "raw", "survey_biomass.csv"), ",")
  
  q_date(year=year)

}


