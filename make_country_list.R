#!/usr/bin/env Rscript
library(whisker)

site_names <- c("Puerto_Rico", "French_Guiana", "Argentina", "Brazil", "Dominica",
                "Netherlands_Antilles", "Honduras", "Colombia", "Costa_Rica")

vals <- list(site_names = iteratelist(site_names, value = "site_name"))

str <- whisker.render(readLines("Rfunctions/remake_sites.yml.whisker"), vals)
writeLines(str, "remake_sites.yml")
