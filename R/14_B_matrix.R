library(dplyr)
require(readr)
require(stringr)
require(purrr)

Tmatrix <- read.csv("data-raw/13_T_matrix.csv")

Wmat <- read_csv("data-raw/51_W_matrix.csv")

Tmatrix_macro_aq <- Tmatrix %>%
  filter(realm != "terrestrial", micro_macro != "micro")

ordered_traits <-  Tmatrix_macro_aq %>%
  select(matches("^[A-Z]{2}\\d")) %>%
  mutate_all(as.character) %>%
  mutate_all(ordered, levels = c("0", "1", "2", "3")) %>%
  bind_cols(select(Tmatrix_macro_aq, species_id), .)

rownames(ordered_traits) <- ordered_traits$species_id

ordered_traits_noname <- select(ordered_traits, -species_id)

dimnames(ordered_traits_noname)

spp_names <- colnames(Wmat)[-1] %>%
  str_split("_") %>%
  map_chr(2)

length(spp_names)

order_traits_rep_spp <- ordered_traits_noname[spp_names,]
row.names(order_traits_rep_spp) <- colnames(Wmat[-1])

glimpse(order_traits_rep_spp)

head(order_traits_rep_spp)

saveRDS(order_traits_rep_spp, "data-raw/14_B_matrix.RDS")
