
library(datastorr)

our_info <- github_release_info("SrivastavaLab/cesabfunctionalwebsdata",
                                readRDS,
                                private = FALSE)

## this line corrects a small error. It will hopefully become unnecessary very soon.
github_release_create1 <- datastorr:::github_release_create_

## creating a release
github_release_create(our_info,
                      description = "small improvements over 0.1.0 : This version contains better checking for the formato of the WBE matrices.",
                      filename = "~/Desktop/all_data.rds")
