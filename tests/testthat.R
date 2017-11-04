suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(r_utils))

write.table(test_package("r_utils"), "test_results.csv")
