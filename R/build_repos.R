# Install miniCRAN package, and load it, if not already loaded
options(repos = c(CRAN = "http://cran.us.r-project.org/"))

if (!require(miniCRAN)) {
  
  install.packages("miniCRAN", type = "source")
  require(miniCRAN)
}

# Function to extract Dependencies from NEUTILUS DESCRIPTION file.
pkgDescriptionDependencies <- function(file) {
  
  fields <- c("Depends", "Imports", "Suggests", "LinkingTo")
  
  if (!file.exists(file)) 
    stop("no file '", file, "'")

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

# These are the packages to be installed
pkgs <- pkgDescriptionDependencies("../../neutilus/DESCRIPTION")

# Create Repository
localCRAN <- "~/local-CRAN-repo"
if ( (!file.exists(localCRAN)) | (length(list.files(localCRAN)) == 0) ) {
  
  # Creation of the repository
  dir.create(localCRAN)
  alreadyInstalled <- c()
  
} else {
  

  # The repository already exists... Which packages are already installed?
  alreadyInstalled <- available.packages(contriburl = paste0("file://", 
                                                             contrib.url(repos = normalizePath(localCRAN), 
                                                                         getOption("pkgType"))))[, "Package"]
  
  options(repos = c(CRAN = "http://cran.us.r-project.org/"))
  
}

# Remove from the list to be installed those already in the repository
pkgs <- pkgs[!(pkgs$Package %in% alreadyInstalled), ]

if (nrow(pkgs) > 0) {
  
  # There are packages to be installed
  cat("Packages to be downloaded to the Repository:\n")
  print(pkgs)
  
  # First, old version of packages
  oldIdx <- which(!is.na(pkgs$Version))
  oldDeps <- c()
  
  if (length(oldIdx) > 0) {
    
    oldVersions <- pkgs$Package[oldIdx]
    versions <- pkgs$Version[oldIdx]
    versions <- gsub(pattern = "[<=|<|=|>|>=]", replacement = "", x = versions)
    
    addOldPackage(pkgs = oldVersions, path = localCRAN, vers = versions)
    deps <- setdiff(pkgDep(oldVersions), c(oldVersions, alreadyInstalled))
    
    cat("Dependencies to be downloaded (first step):\n")
    cat(sort(deps), sep = ", ")
    
    makeRepo(deps, path = localCRAN, type = "source", quiet = TRUE)
    #makeRepo(deps, path = localCRAN, type = "mac.binary", quiet = TRUE)
    
    # These are the packages downloaded in this step
    oldDeps <- c(oldVersions, deps)
  }
  
  # Current versions of packages (only not already installed dependencies)
  currentVersions <- pkgs$Package[is.na(pkgs$Version)]
  
  newDeps <- setdiff(pkgDep(currentVersions), c(oldDeps, alreadyInstalled))
  
  cat("Dependencies to be downloaded (second step):\n")
  cat(sort(setdiff(newDeps, currentVersions)), sep = ", ")
  
  makeRepo(newDeps, path = localCRAN, type = "source", quiet = TRUE)
  #makeRepo(newDeps, path = localCRAN, type = "mac.binary", quiet = TRUE)
  
}

