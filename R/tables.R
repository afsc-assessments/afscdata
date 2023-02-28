#' query prohibited species catch (psc) estimate
#'
#' @param year assessment year
#' @param trip_target 'p' = pollock-mid, 'b' = pollock-bottom, x' = rex, 'h' = shallow flats, 'k' = rockfish, 'w' = arrowtooth, 'c' = pcod, 'i' = halibut
#' @param area goa or bsai
#' @param akfin the database to query
#' @param save save the file in designated folder (default = T) or the global environment
#'
#' @return a csv of prohibited species catch by trip target group, saved in the data/output folder
#' @export 
#' @description prohibited species catch (PSC) estimates reported in tons for halibut and herring, counts for salmon, crabs and other fish. Note that you can combine trip target codes c("k", "x") and regions - though results will be lumped together
#' @examples
#' \dontrun{
#' akfin = connect()
#' q_psc(year=2022, trip_target="k", area="goa", afkin, save=FALSE)
#' disconnect(akfin)
#' }
#'
q_psc <- function(year, trip_target, area, server, save = TRUE) {
  area = toupper(area)
  trip_target = toupper(trip_target)
  
  # call sql
  psc = sql_read("psc.sql")
  
  # filter years 
  psc = sql_filter(sql_precode = "", x = year-4, sql_code = psc, flag = "-- insert year")
  psc = sql_filter(sql_precode = "", x = year, sql_code = psc, flag = "-- year2")
  
  # region filter
  psc = sql_filter(x = area, sql_code = psc, flag = "-- insert region")
 
  # species filter
  psc = sql_filter(x = trip_target, sql_code = psc, flag = "-- insert species")
  
  sql_run(server, psc) %>%
    tidytable::rename_with(tolower) %>%
    tidytable::pivot_wider(names_from = year, values_from = psc) -> psc
  
  if(isTRUE(save)){
    psc %>%
      write.csv(here::here(year, "data", "output", "psc_catch.csv"),
                row.names = FALSE)
    message("PSC table written to data/output folder.")
  } else {
    psc
  }
}