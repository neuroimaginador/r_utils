`:=` <- function(lhs, rhs) {

  LHS <- as.character(substitute(lhs))[-1L]

  is_matrix <- is.matrix(rhs)
  is_list <- is.list(rhs)
  is_vector <- is.vector(rhs)

  if (length(LHS) == 1) {

    assign(x = LHS, value = rhs, envir = parent.frame())

  } else {

    if (length(LHS) <= length(rhs)) {

      for (i in seq_along(LHS)) {

        if (is_valid_variable_name(LHS[i])) {

          if (is_list)
            assign(x = LHS[i], value = rhs[[i]], envir = parent.frame())

          if (is_matrix)
            assign(x = LHS[i], value = rhs[, i], envir = parent.frame())

          if (is_vector & !is_list)
            assign(x = LHS[i], value = rhs[i], envir = parent.frame())

        }

      }

    }

  }

}

conc <- function(lhs, rhs) {

  LHS <- as.character(substitute(lhs))

  is_matrix <- is.matrix(rhs)
  is_df <- is.data.frame(rhs)
  is_list <- is.list(rhs)
  is_vector <- is.vector(rhs)

  tmp <- get(x = LHS, envir = parent.frame())

  if (is_df | is_matrix) {

    assign(x = LHS, value = cbind(tmp, rhs), envir = parent.frame())

  }

  if (is_list) {

    assign(x = LHS, value = c(tmp, rhs), envir = parent.frame())

  }

}
