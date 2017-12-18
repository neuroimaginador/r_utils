check_package <- function(path = ".",
                          test_coverage = TRUE,
                          type = c("test", "vignette", "example",
                                   "all", "none")) {

  safe_require("devtools")
  safe_require("testthat")

  if (test_coverage) {

    safe_require("covr")

    cov <- package_coverage(path = path, type = type[1])

    csv <- file.path(path, "tests", "test_results.csv")

    df <- read.delim(csv, sep = " ")

  } else {

    df <- devtools::test(path)

  }

  cat("Test results:\n")
  print(df)

  if (test_coverage) {

    safe_require("shiny")

    cat("\nOpening coverage")
    shine(cov)

    return(list(test_results = df, coverage = cov))

  }

  return(list(test_results = df))

}
