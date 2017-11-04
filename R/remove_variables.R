remove_variables <- function(...,
                             envir = parent.frame()) {
  
  # Names of the mentioned variables 
  names <- as.character(substitute(list(...)))[-1L]
  
  rm(list = names, envir = envir)
  gc()
  
}