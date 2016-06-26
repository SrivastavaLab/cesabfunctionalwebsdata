# Loading the data

library(dplyr)
library(assertr)
library(readr)
library(tidyr)
library(daff)
library(purrr)

## read_csv2 uses ; as a field separator

Alltraits <- read_csv("data-raw/07_traits.csv")

## remember to take out the NAS that are coced as characters

# Create genera table

unique_taxa <- function(taxon){
  Alltraits %>%
    select_(taxon) %>%
    {.[!is.na(.[[taxon]]), ]} %>%
    {.[.[[taxon]] != "NA", ]} %>%
    # filter(!is.na(genus)) %>% ## complete cases; filter NA s that are words
    distinct() %>%
    arrange()
}

unique_taxa("genus")
unique_taxa("family")

taxon_list <- names(Alltraits)[3:14] %>%
  set_names() %>%
  map(unique_taxa)

### HERE ANDREW
for i

familyt <- Alltraits %>%
  select(family) %>%
  distinct() %>%
  arrange(family)


