add_test_infrastructure <- function(pkg = ".") {

  testthat_template <- c("suppressPackageStartupMessages(library(testthat))",
                         "suppressPackageStartupMessages(library({{{ name }}}))","",
                         'write.table(test_package("{{{ name }}}"), "test_results.csv")')

  pkg <- devtools::as.package(pkg)

  devtools:::check_suggested("testthat")
  if (devtools::uses_testthat(pkg)) {

    message("* testthat is already initialized")
    return(invisible(TRUE))

  }

  dir.create(file.path(pkg$path, "tests", "testthat"), showWarnings = FALSE,
             recursive = TRUE)

  writeLines(whisker::whisker.render(template = testthat_template, data = list(name = pkgname)),
             file.path(pkg$path, "tests", "testthat.R"))

  devtools:::add_desc_package(pkg, "Suggests", "testthat")

  invisible(TRUE)

}
