#' Setup folder structure
#'
#' Creates a common folder structure for assessment data
#'
#' @param year assessment year
#' @param dirs directories to write
#' @param tier assessment tier to change the folders used - not currently implemented
#' @return creates a designated/named folder structure 
#' @export setup_folders
#'
#' @examples
#' \dontrun{
#' setup(2022)
#'}
setup_folders <- function(year, dirs = c("raw", "user_input", "output", "sara"), tier = NULL){
  
    for(i in 1:length(dirs)){
      if(dir.exists(here::here(year, "data", dirs[i])) == FALSE){
        dir.create(here::here(year, "data", dirs[i]), recursive=TRUE)
      }
    }
}