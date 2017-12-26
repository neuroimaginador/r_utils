run_test_dir <- function(pkg, tmp_lib, dots, quiet = TRUE) {

  testing_dir <- file.path(covr:::test_directory(pkg$path), "testthat")

  covr:::RCMD("INSTALL", options = c(pkg$path, "--no-docs", "--no-multiarch",
                                     "--preclean", "--with-keep.source", "--no-byte-compile",
                                     "--no-test-load", "--no-multiarch", "-l", tmp_lib), quiet = quiet)

  if (isNamespaceLoaded(pkg$package)) {

    covr:::try_unload(pkg$package)
    on.exit(loadNamespace(pkg$package), add = TRUE)

  }

  covr:::with_lib(tmp_lib, {

    ns_env <- loadNamespace(pkg$package)
    env <- new.env(parent = ns_env)

    exprs <- c(dots,
               quote("library(methods)"),
               if (file.exists(testing_dir)) {

                 bquote(try(test_results <- testthat::test_dir(path = .(testing_dir), env = .(env))))

               }
    )

    enc <- environment()
    cov <- covr:::environment_coverage_(ns_env, exprs, enc)

    test_results <- get("test_results", envir = enc)

    covr:::try_unload(pkg$package)
    list(coverage = cov, results = test_results)

  })

}
