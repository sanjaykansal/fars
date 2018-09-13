expect_error(fars_read(file.path("inst/extdata", "InvalidFileName.csv"),
                       "file 'InvalidFileName.csv' does not exist"))
