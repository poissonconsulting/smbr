Sys.setenv("R_TESTS" = "")

library(testthat)
library(smbr)

test_check("smbr")
