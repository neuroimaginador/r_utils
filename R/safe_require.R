safe_require <- function(pkg = NULL, username = NULL, package_name = NULL) {

  silent_require <- function(pkg_name) suppressMessages(base::require(package = pkg_name, character.only = TRUE))

  require(devtools)

  if (is.null(pkg) && (is.null(username) || is.null(package_name))) {

    stop("Insufficient data to load package.")

  }

  if (is.null(pkg)) {

    pkg <- paste0(username, "/", package_name)

  }

  # Determine if the package is from github (there is a "/" in the name)
  # In this case, extract user name and package name
  if (grepl(x = pkg, pattern = "/", fixed = TRUE)) {

    from_github <- TRUE
    attributes <- regmatches(x = pkg, regexpr(text = pkg, pattern = "/", fixed = TRUE), invert = TRUE)[[1]]
    username <- attributes[1]
    package_name <- attributes[2]

  } else {

    from_github <- FALSE
    package_name <- pkg

  }

  if (!silent_require(package_name)) {

    cat("Package ", package_name, " not installed yet. Attempting installation.\n")

    if (from_github) {

      suppressMessages(devtools::install_github(repo = pkg, upgrade_dependencies = FALSE))

    } else {

      install.packages(pkg)

    }

    silent_require(package_name)

  }

  return(invisible())

}
