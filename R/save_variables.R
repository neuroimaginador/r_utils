save_variables <- function(..., 
                           envir = parent.frame(), 
                           folder_save = tempdir(),
                           remove_after = TRUE) {
  
  # Names of the mentioned variables 
  names <- as.character(substitute(list(...)))[-1L]
  
  # Create directory if no present
  if ( !dir.exists(folder_save) )
    dir.create(folder_save, showWarnings = FALSE)
  
  # Iterate over variable names
  for (i in 1:length(names)) {
    
    # Make use of the environment to avoid too many copies of the object
    saveRDS(object = envir[[names[i]]], file = file.path(folder_save, paste0(names[i], ".rds")))
    
  }
  
  # Remove variables if needed
  if (remove_after) {
    
    rm(list = names, envir = envir)
    gc()
    
  }
  
  return(invisible(TRUE))
  
}
