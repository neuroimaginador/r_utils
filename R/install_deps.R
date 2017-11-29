# Install dependencies for NEUTILUS, based on the local repository

# Function to extract Dependencies from NEUTILUS DESCRIPTION file.
pkgDescriptionDependencies <- function(file) {
  
  fields <- c("Depends", "Imports", "Suggests", "LinkingTo")
  
  if (!file.exists(file)) stop("no file '", file, "'")
  DESCRIPTION <- read.dcf(file)
  requirements <- DESCRIPTION[1, fields[fields %in% colnames(DESCRIPTION)]]
  
  ## Remove whitespace
  requirements <- gsub("[[:space:]]*", "", requirements)
  
  ## Parse packages + their version
  parsed <- vector("list", length(requirements))
  for (i in seq_along(requirements)) {
    x <- requirements[[i]]
    splat <- unlist(strsplit(x, ",", fixed = TRUE))
    res <- lapply(splat, function(y) {
      if (grepl("(", y, fixed = TRUE)) {
        list(
          Package = gsub("\\(.*", "", y),
          Version = gsub(".*\\((.*?)\\)", "\\1", y, perl = TRUE),
          Field = names(requirements)[i]
        )
      } else {
        list(
          Package = y,
          Version = NA,
          Field = names(requirements)[i]
        )
      }
    })
    parsed[[i]] <- list(
      Package = sapply(res, "[[", "Package"),
      Version = sapply(res, "[[", "Version"),
      Field = sapply(res, "[[", "Field")
    )
  }
  
  result <- do.call(rbind, lapply(parsed, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
  
  ## Don't include 'base' packages
  ip <- installed.packages()
  basePkgs <- ip[Vectorize(isTRUE)(ip[, "Priority"] == "base"), "Package"]
  result <- result[!(result$Package %in% basePkgs), ]
  
  ## Don't include R
  result <- result[ !result$Package == "R", ]
  
  result
  
}

# # These are the packages to be installed
# pkgs <- pkgDescriptionDependencies("../../neutilus/DESCRIPTION")

# # Local Repository
# localCRAN <- normalizePath("~/local-CRAN-repo")
# repos <- getOption("repos")
# options(repos = localCRAN)

# # Installed packages
# installed <- installed.packages()

# # Packages to be installed
# cat("Installing packages:\n")
# toInstall <- setdiff(pkgs$Package, installed)
# cat(sort(toInstall), sep = ", ")

# # Actual installation
# contriburl <- paste0("file://", 
#                      contrib.url(repos = normalizePath(localCRAN), 
#                                  type = "source"))
# install.packages(toInstall, contriburl = contriburl, type = "source")
