get_coverage <- function (path = ".",
                          type = "test",
                          ...,
                          relative_path = TRUE, quiet = TRUE,
                          exclude_source = TRUE,
                          exclusions = NULL, exclude_pattern = getOption("covr.exclude_pattern"),
                          exclude_start = getOption("covr.exclude_start"), exclude_end = getOption("covr.exclude_end")) {

  pkg <- devtools::as.package(path)
  clean <- TRUE

  dots <- covr:::dots(...)
  sources <- covr:::sources(pkg$path)
  tmp_lib <- tempdir()

  if (length(sources) > 0 && !exclude_source) {

    flags <- c(CFLAGS = "-g -O0 -fprofile-arcs -ftest-coverage",
               CXXFLAGS = "-g -O0 -fprofile-arcs -ftest-coverage",
               FFLAGS = "-g -O0 -fprofile-arcs -ftest-coverage",
               FCFLAGS = "-g -O0 -fprofile-arcs -ftest-coverage",
               LDFLAGS = "--coverage")

    if (is_windows()) {
      flags[["SHLIB_LIBADD"]] <- "--coverage"
    }

    covr:::with_makevars(flags, {

      covr:::subprocess(clean = clean,
                        quiet = quiet,
                        coverage_results <- run_test_dir(pkg, tmp_lib, dots, quiet))

    })

    coverage_results$coverage <- c(coverage_results$coverage, covr:::run_gcov(pkg$path, sources, quiet))

    if (isTRUE(clean)) {

      devtools::clean_dll(pkg$path)
      covr:::clear_gcov(pkg$path)

    }

  } else {

    covr:::subprocess(clean = clean,
                      quiet = quiet,
                      coverage_results <- run_test_dir(pkg, tmp_lib, dots, quiet = quiet))

  }

  coverage <- coverage_results$coverage
  test_results <- coverage_results$results

  for (i in seq_along(coverage)) {

    display_path <- if (isTRUE(relative_path)) pkg$path else NULL

    covr:::display_name(coverage[[i]]$srcref) <- covr:::generate_display_name(coverage[[i]],
                                                                              display_path)

    class(coverage[[i]]) <- "expression_coverage"

  }

  attr(coverage, "type") <- type
  attr(coverage, "package") <- pkg
  class(coverage) <- "coverage"

  coverage <- coverage[!rex::re_matches(display_name(coverage),
                                        rex::rex("R", one_of("/", "\\"), "BasicClasses.R"))]
  class(coverage) <- "coverage"

  list(coverage = covr:::exclude(coverage, exclusions = exclusions, exclude_pattern = exclude_pattern,
                                 exclude_start = exclude_start, exclude_end = exclude_end,
                                 path = if (isTRUE(relative_path)) pkg$path else NULL),
       test_results = test_results)

}
