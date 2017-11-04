build_Rdata_from_rds_folder <- function(folder_save, output_folder, prefix = "t1_struct") {
  
  files <- list.files(path = folder_save, pattern = "*.rds", full.names = FALSE)
  
  vars <- gsub(x = files, pattern = "\\.rds", replacement = "")
  
  envir <- new.env()
  
  for (i in 1:length(vars)) {
    
    assign(vars[i], value = readRDS(file = file.path(folder_save, files[i])), envir = envir)
    
  }
  
  save(list = ls(envir = envir), file = file.path(output_folder, paste0(prefix, ".Rdata")), envir = envir)
  
}