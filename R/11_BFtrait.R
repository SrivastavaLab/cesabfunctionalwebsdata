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

numbers <- as.character(rev(seq_along(taxon_list)))
numbers <- ifelse(nchar(numbers) == 1, yes = paste0("0", numbers), no = numbers)

names(numbers) <- names(taxon_list)

### write tables out as csvs
for (i in names(taxon_list)) {
  write_csv(taxon_list[[i]], path = paste0("data-intermediate/BF_traits/BF_", numbers[[i]],"_", i, ".csv"))
}

familyt <- Alltraits %>%
  select(family) %>%
  distinct() %>%
  arrange(family)


