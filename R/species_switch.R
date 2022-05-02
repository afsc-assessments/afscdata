#' assign species codes
#'
#' @param species species group name e.g., "DUSK"
#' @param area area of interest ("GOA", "BS" or "AI")
#'
#' @return
#' @export
#'
#' @examples
#' species_switch("DUSK", "GOA")

species_switch <- function(species, area){
  
  # goa ----
  if(species == 'ARTH' & area == 'GOA'){
    afsc_species = 10110
    norpac_species = 141
  }
  
  if(species == 'DUSK' & area == 'GOA'){
    species = c("DUSK", "PEL7", "PELS")
    afsc_species1 = 30150
    afsc_species2 = 30152
    norpac_species = 330
  }
  
  if(species == 'NORK' & area == 'GOA'){
    afsc_species = 30420
    norpac_species = 303
  }
  
  if(species == 'REYE' & area == 'GOA'){
    afsc_species1 = 30050 # Rougheye and Blackspotted rockfish unidentified, use this code for Longline Survey Data
    afsc_species2 = 30051 # Rougheye rockfish, Sebastes aleutianus, use for Trawl Survey Data
    afsc_species3 = 30052 # Blackspotted rockfish, Sebastes melanostictus, use for Trawl Survey Data
    norpac_species = 307 # Rougheye and Blackspotted rockfish unidentified, use for NORPAC Data
    norpac_species2 = 357 # Blackspotted rockfish, use for NORPAC Data
  }
  
  # bsai ----
  
  ## NOT SURE ABOUT THIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if(species == "FSOL" & area == "BS"){
    norpac_species = 103
  }
  
  
}