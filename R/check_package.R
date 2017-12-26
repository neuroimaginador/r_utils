check_package <- function(path = ".",
                          test_coverage = TRUE,
                          type = c("test", "vignette", "example",
                                   "all", "none")) {

  safe_require("devtools")
  safe_require("testthat")

  if (test_coverage) {

    safe_require("covr")

    cov <- get_coverage(path = path, type = type[1])

    return(cov)

  } else {

    df <- devtools::test(path)

    return(list(test_results = df))

  }

}

view_check_results <- function(L, interactive = FALSE) {

  cat("Test results:\n")
  pretty_print_test_results(L$test_results)

  if (!is.null(L$coverage)) {

    print(L$coverage)

    if (interactive) {

      safe_require("shiny")

      cat("\nOpening coverage\n")
      covr::shine(L$coverage)

    }

  }

}

pretty_print_test_results <- function(test_results) {

  library(tidyverse)
  library(tidyr)

  leading_zeros <- function(x) {

    res <- vector(mode = "character", length = length(x))

    res[x >= 10] <- as.character(x[x >= 10])
    res[x < 10] <- paste0("0", as.character(x[x < 10]))

    return(res)

  }


  pretty_results <- test_results %>%
    as.data.frame() %>%
    group_by(context) %>%
    summarise(number = sum(nb),
              failures = sum(failed),
              warnings = sum(warning),
              skips = length(which(skipped)),
              total_time = sum(real))

  skipped_contexts <- pretty_results %>% filter(skips == number) %>% select(context) %>% map(as.character) %>% unlist()
  pretty_results <- pretty_results %>% filter(number > skips)

  contexts <- pretty_results %>% select(context) %>% map(as.character) %>% unlist()

  df <- as.data.frame(pretty_results %>% select(-context))

  df <- rbind(df, colSums(df))
  rownames(df) <- c(contexts, "TOTAL")

  if (nrow(df) > 0) {

    max_length <- contexts %>% map_int(nchar) %>% max()

    library(crayon)

    header <- sprintf(paste0("%-", max_length + 3, "s %5s %5s %5s %5s %6s"), "context", "OK", "F", "W", "S", "time")
    cat(header, "\n")

    for (row in seq(nrow(df))) {

      row_init <- sprintf(paste0("%-", max_length + 3, "s"), rownames(df)[row])
      oks <- sprintf("%3s", as.character(df$number[row] - df$failures[row]))
      fails <- sprintf("%3s", as.character(df$failures[row]))
      warns <- sprintf("%3s", as.character(df$warnings[row]))
      skips <- sprintf("%3s", as.character(df$skips[row]))

      time_secs <- as.integer(df$total_time[row])
      time_frac <- leading_zeros(round((df$total_time[row] - time_secs) * 100))
      time <- sprintf("%+3s.%-2s", as.character(time_secs), as.character(time_frac))

      cat(blue(row_init), " ",
          green(oks), " ",
          if (df$failures[row] == 0) green(fails) else red(fails), " ",
          if (df$warnings[row] == 0) green(warns) else red(warns), " ",
          yellow(skips), " ",
          blue(time), "\n")

    }

  }

  if (length(skipped_contexts) > 0) {

    cat("Skipped contexts:\n", paste0(red(skipped_contexts), collapse = ", "), "\n")

  }

}


create_test_files <- function(path = ".") {

  safe_require("testthis")

  safe_require("devtools")

  pkg <- devtools::as.package(path)
  test_path <- file.path(pkg$path, test_path())

  if (!file.exists(test_path)) {

    add_test_infrastructure(path)

  }

  r_files <- list.files(path = file.path(pkg$path, "R"), pattern = "*.R")

  for (f in r_files) {

    testfile_name <- file.path(test_path, paste0("test_", f))
    testthis::test_skeleton(testfile_name, open = FALSE)

  }

}
