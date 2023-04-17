#' copy the previous SSC accepted model as the new "base" model
#'
#' @description copy of all base model files, adds a "README.md" file that identifies the model used (or appends the current README), predicated on using the 'afscdata::setup_folders()' function, though has an option for other folder structures. Note, this function copies everything over (e.g., retrospectives) and may take a few minutes to complete
#' @param base_year year of the base model
#' @param base_model name of the base model (folder name)
#' @param year current year
#' @param folder if the base model is in a different location than the afscdata structure is looking for
#'
#' @return a 'base' model folder with all of the prior years inputs/outputs
#' @export accepted_model
#'
#' @examples
#' \dontrun{
#' accepted_model(base_year = 2020, base_model = "m18.2b", year = 2021)
#' }
accepted_model <- function(base_year, base_model, year, folder = NULL){
  
  
  x0 = dir.exists(here::here(base_year, base_model))
  
  if(!x0 & is.null(folder)){
    stop("incorrect base model location: maybe provide a file?")
  }
  
  x = dir.exists(here::here(year, "base"))
  y = NA
  
  if (!x) {
    dir.create(here::here(year, "base"))
  } else {
    y = readline("To overwrite the base folder enter 1, else 0: ")
  }
  
  if(y!=1 & !is.na(y)){
    stop("something has to give, maybe create a new folder?")
  }
  
  if(y==1 & is.null(folder) | is.na(y) & is.null(folder)){
    
    file.copy(list.files(here::here(base_year, base_model), full.names = TRUE),
              here::here(year, "base"),
              recursive = TRUE, overwrite = TRUE)

    write.table(c(paste0("Using SSC accepted model ", base_model, " from ", base_year, " as the base model for ", year, ".")), 
                file = here::here(year, "README.md"),
                sep = "\t", col.names = F, row.names = F, append=TRUE)


  } else if(y==1 & !is.null(folder) | is.na(y) & !is.null(folder)){
    file.copy(list.files(folder, full.names = TRUE),
              here::here(year, "base"),
              recursive = TRUE, overwrite = TRUE)
    
    write.table(c(paste0("Using SSC accepted model ", base_model, " from ", base_year, " as the base model for ", year, ".")), 
                file = here::here(year, "README.md"),
                sep = "\t", col.names = F, row.names = F, append=TRUE)
  } else {
    cat("folder not overwritten.\n")
  }
  
  
}