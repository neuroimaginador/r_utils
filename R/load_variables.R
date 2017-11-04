load_variables <- function(..., 
                           envir = parent.frame(),
                           folder_save) {
  
  # Names of the mentioned variables 
  names <- as.character(substitute(list(...)))[-1L]
  
  # Iterate over variable names
  for (i in 1:length(names)) {
    
    # Make use of the environment to avoid too many copies of the object
    assign(x = names[i], 
           value = readRDS(file = file.path(folder_save, paste0(names[i], ".rds"))), envir = envir)
    
  }
  
  return(invisible(NULL))
  
}
