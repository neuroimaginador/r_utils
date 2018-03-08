# https://www.r-bloggers.com/testing-for-valid-variable-names/
is_valid_variable_name <- function(x, allow_reserved = TRUE, unique = FALSE) {

  ok <- rep.int(TRUE, length(x))

  #is name too long?
  max_name_length <- if(getRversion() < "2.13.0") 256L else 10000L

  #is it a reserved variable, i.e.
  #an ellipsis or two dots then a number?
  if(!allow_reserved)
  {
    ok[x == "..."] <- FALSE
    ok[grepl("^\\.{2}[[:digit:]]+$", x)] <- FALSE
  }

  #are names valid (and maybe unique)
  ok[x != make.names(x, unique = unique)] <- FALSE

  ok

}
