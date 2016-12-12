
library(datastorr)

our_info <- github_release_info("SrivastavaLab/cesabfunctionalwebsdata",
                                readRDS,
                                private = FALSE)

## this line corrects a small error. It will hopefully become unnecessary very soon.
github_release_create1 <- datastorr:::github_release_create_

## creating a release
github_release_create(our_info,
                      description = "0.5.0 contains updated and corrected data for several new sites.",
                      filename = "RELEASE/all_data.rds")
