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
  # globals 
  area = "bsai"
  species = 870
  afsc_species = c(78010, 78011, 78012, 78013, 78020, 78021, 78022, 78023, 
                   78030, 78040, 78210, 78300, 78301, 78352, 78353, 78403, 
                   78404, 78452, 78454, 78455)
  
  db = connect()
  q_catch(year=year, species=species, area=area, db=db, save=F)
 
    
  
}

