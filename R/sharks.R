#' raw data query for GOA/BSAI sharks
#'
#' @param year assessment year
#' @param area what region? eith goa or bsai
#' 
#' @return a suite of raw data .csv files and a time stamp of when the query was done 
#' @export sharks
#'
#' @examples
#' \dontrun{
#' sharks(year = 2023, area = "goa")
#'}
sharks <- function(year, area) {
  
  species = 689:692

  # catch 
  db = connect()
  q_catch(year=year, species=species, area=area, db=db)
  
  q_date(year=year)
  
}
