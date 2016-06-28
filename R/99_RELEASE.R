
library(datastorr)

our_info <- github_release_info("SrivastavaLab/cesabfunctionalwebsdata",
                                readRDS,
                                private = FALSE)

## this line corrects a small error. It will hopefully become unnecessary very soon.
github_release_create1 <- datastorr:::github_release_create_

## creating a release
github_release_create(our_info,
                      description = "Update to the CESAB functionalwebs datasets. This release contains all the data in the format of a list. There are five parts to this list:

                      * `datasets`: a data frame containing the datasets table (with minimal changes)
                      * `visits`: data frame with the visits
                      * `traits`: data frame with trait data. Updated with new traits: **MD**, **CP**, and **BF**.
                      * `bromeliads`: data frame with all the bromeliad-level information. it has as many rows as bromeliads in the dataset, and as many columns as we have bromeliad-level information. This includes a column for every detritus category, as well as estimated variables such as total detritus and total water. it is very wide.
                      * `abundance`: this data frame has 5 columns and > 56 000 rows. It contains count data for all macroinvertebrates in all bromeliads in the database
                      * `WBE`: another list, containing specially-formatted W, B and E matrices. Also, a list of four different vectors, which allow us to subset W according to the number of complete cases in each of the response variables in the E matrix (or in all of them).",
                      filename = "~/Desktop/all_data.rds")
