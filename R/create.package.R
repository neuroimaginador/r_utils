create_package <- function(pkgname) {

  # Auxiliary functions
  create <- devtools::create

  add_pkg_doc <- function(pkg = ".") {

    pkgdoc_template <- c("#' {{{ name }}}.", "#'", "#' @name {{{ name }}}", "#' @docType package", "NULL")

    pkg <- devtools::as.package(pkg)

    writeLines(whisker::whisker.render(template = pkgdoc_template, data = list(name = pkgname)),
               file.path(pkg$path, "R", paste0(pkgname, ".R")))

    invisible(TRUE)

  }

  cat("Creating package", pkgname, "\n")
  create(pkgname)
  cat("   Adding test infrastructure...\n")
  add_test_infrastructure(pkgname)
  cat("   Adding documentation file for package...")
  add_pkg_doc(pkgname)
  cat("   Done!")

}
