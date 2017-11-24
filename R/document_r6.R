document_R6_methods <- function(my_class) {
  
  stopifnot(inherits(my_class, "R6ClassGenerator"))
  
  methods <- my_class$public_methods
  class_name <- my_class$classname
  
  string <- c()
  for (method_name in names(methods)) {
    
    string <- paste0(string, 
                     my_makeOxygen(obj = methods[[method_name]], print = FALSE), "#'\n",
                     paste0("#' @name ", class_name, ".", method_name, "\n"),
                     "NULL\n\n")
    
  }
  
  return(string)
  
}

document_r6_class <- function(my_class) {
  
  preface <- c(
    paste0("#' ", my_class$classname, " Class"),
    "#'",
    "#' @docType class",
    "#' @importFrom R6 R6Class",
    "#'",
    "#' @export",
    "#' @keywords data",
    "#'",
    paste0("#' @return Object of \\code{\\link{R6Class}} and \\code{", my_class$classname, "}."),
    "#'",
    "#' @format \\code{\\link{R6Class}} object.",
    "#'",
    "#' @examples",
    paste0("#' ", my_class$classname, "$new()"),
    "#'",
    "#' @section Methods:",
    "#' \\describe{",
    "#'   \\item{Documentation}{For full documentation of each method follow the corresponding link. }"
  )
  
  methods <- c()
  
  class_methods <- my_class$public_methods
  
  for (method_name in names(class_methods)) {
    
    args <- formals(my_class$public_methods[[method_name]])
    
    args_str <- c()
    
    for (i in seq_along(args)) {
      
      if (!is.null(args[[i]])) {
        
        if (!inherits(args[[i]], "name") & !inherits(args[[i]], "call")) {
          
          # print(method_name)
          # print(names(args)[i])
          # print(args[[i]])
          # print(class(args[[i]]))
          # 
          if (is.na(args[[i]])) {
            
            args_str <- c(args_str, paste0(names(args)[i], " = NA"))
            next
            
          }
          
        }
        
        if (args[[i]] == "") {
          
          args_str <- c(args_str, paste0(names(args)[i]))
          
        } else {
          
          if (inherits(args[[i]], "call")) {
            
            default <- capture.output(args[[i]])
            
          } else {
            
            default <- args[[i]]
            if (is.character(default)) {
              
              default <- paste0("'", default, "'")
              
            }
            
          }
          
          args_str <- c(args_str, paste0(names(args)[i], " = ", default))
          
        }
        
      } else {
        
        args_str <- c(args_str, paste0(names(args)[i], " = NULL"))
        
      }
      
      
    }
    
    args_str <- paste0(args_str, collapse = ", ")
    
    methods <- c(methods,
                 paste0("#'   \\item{\\code{", method_name, "(", args_str, ")}}", 
                        "{METHOD DESCRIPTION. Documented in \\link{", my_class$classname, ".", method_name,"}.}")
                 )
    
  }
  
  end <- "#'  }\n"

  class_doc <- paste0(c(preface, methods, end), collapse = "\n")
  methods_doc <- document_R6_methods(my_class)

  return(list(class_doc = class_doc, methods_doc = methods_doc))
  
}


document_r6_file <- function(filename, overwrite = FALSE) {
  
  lines <- readLines(con = filename)
  lines <- lines[!grepl("^\\s*#'", lines)]
  
  nenv <- new.env()
  
  sys.source(filename, envir = nenv, keep.source = TRUE)
  
  class_name <- ls(envir = nenv)
  
  my_class <- get(x = class_name, envir = nenv)
  
  strings <- document_r6_class(my_class)
  
  new_lines <- paste0(strings$class_doc, paste0(c(lines, ""), collapse = "\n"))
  
  if (overwrite) {
    
    new_file <- filename
     
  } else {
    
    new_file <- file.path(dirname(normalizePath(filename)), paste0("doc_", basename(filename)))
    
  }
  
  cat(new_lines, file = new_file)
  
  methods_file <- file.path(dirname(normalizePath(filename)), paste0("methods_", class_name, ".R"))
  cat(strings$methods_doc, file = methods_file)
  
}
