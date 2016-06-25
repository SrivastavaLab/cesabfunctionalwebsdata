# Loading the data

library(dplyr)
library(assertr)
library(readr)
library(tidyr)
library(daff)

## read_csv2 uses ; as a field separator

Alltraits <- read_csv("data-raw/07_traits.csv")

# Create genera table

genus <- Alltraits %>%
  select(genus) %>%
  filter(!is.na(genus)) %>%
  distinct() %>%
  arrange(genus) %>%  as.data.frame()

familyt <- Alltraits %>%
  select(family) %>%
  distinct() %>%
  arrange(family)


