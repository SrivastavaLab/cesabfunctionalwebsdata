## summarizing a few of the bigger datasets
## Before summarizing, we modify the data with allometric equations.

require(dplyr)
require(readr)
require(tidyr)

# bromeliads --------------------------------------------------------------

broms <- read_csv("data-raw/01_broms.csv")

## the "problems" are probaby OK?
problems(broms)
broms$ph
## yes --- they look just fine

detritus_wide <- broms %>%
  select(bromeliad_id, min, max, mass) %>%
  unite(min_max, min, max) %>%
  mutate(min_max = paste0("detritus", min_max)) %>%
  spread(min_max, mass)

### this script summarizes detritus amounts -- run it after you have imputed missing values
det <- broms %>%
  select(bromeliad_id, min, max, mass) %>%
  semi_join(manys %>% filter(n > 4)) ## just to check with the eye.
  group_by(bromeliad_id) %>%
  summarise(detritus_mass_total = sum(mass, na.rm = TRUE))

summ_brom <- broms %>%
  select(-min, -max, -mass) %>%
  distinct %>%
  left_join(det)

write_csv(summ_brom, "data-raw/02_broms.csv")


# abundance ---------------------------------------------------------------

abund <- read_csv("data-raw/01_abundance.csv")

glimpse(abund)

summ_abund <- abund %>%
  select(-category_range, -measurement) %>%
  group_by(dataset_id, species_id, bwg_name, brm) %>%
  summarise(abundance = sum(abd, na.rm = TRUE))

write_csv(summ_abund, "data-raw/02_abundance.csv")

