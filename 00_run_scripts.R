## Reproducibly downloading and processing the Bromeliad working group data
## Andrew MacDonald
## Oct 2025

## this is the MAIN script that runs the other scripts.
## if you want to create a new release of the dataset, this is where you should work!

# the workflow is divided into two parts (which are called "projects" in targets).
# 01_download_data -- downloads the data
# 02_process_data -- processes, corrects and combines data

## The reason for this division is that data downloads should happen only when
## the BWG database has changed. Data processing, on the other hand, probably
## needs regular maintenance and updating of the code and the process

library(targets)
# library(osfr)
# library(tidyverse)

download_now <- FALSE

if(download_now){
  Sys.setenv(TAR_PROJECT = "project_download_data")
  tar_make()
}

Sys.setenv(TAR_PROJECT = "project_process_data")
tar_make()
tar_make("checked_data")
tar_load("checked_data")
checked_data

## while working it is helpful to visualize the process.

tar_visnetwork(targets_only = TRUE,
               script = "01_download_data.R")


tar_visnetwork(names = "supp_size_model_info", targets_only = TRUE,
               script = "02_process_data.R")


## for just one object,
## here, the object called "abundance"
tar_visnetwork(
  TRUE,
  # names = "broms_date",
  script = "02_process_data.R")

tar_make("volume_estimated",
         script = "01_download_data.R")

tar_make("abundance_no_81",
         script = "02_process_data.R")

tar_read(fpom_fg_data)



tar_load(mertensii,
         store = "store_process_data")


tar_read()

tar_load(taxonomy_cols,
         store = "store_process_data")
tar_load(lowest_taxonomic,
         store = "store_process_data")



tar_load(broms_date,
         store = "store_process_data")
tar_load(detritus_wide,
         store = "store_process_data")
tar_load(fpom_brom,
         store = "store_process_data")


make_detritus_wider(
      broms_date, detritus_wide,
      visitnames, diam_brom, fpom_brom)



  tar_load(dats)
glimpse(dats)


parse_column_types_reader(dats)

## NOTE I killed the content of parse_column_types_reader. does it do anything?

glimpse(visits)

## why doesn't visits have a visit id?? did it download like this?


dd <- tidyr::unnest(abundance, measurements) |>
  dplyr::glimpse()

dd$abd |>
  purrr::map_dbl(length) |>
  sort(decreasing = TRUE)

# okay so there is always only one abundance going on


#' 10 Nov: error in taxon_lowest_names. this was a function made in order to
#' assign traits to the lowest taxonomic level possible.
#'
#' looks like it i reading species as having NA_NA as a species name -- which
#' should not be the case, NA should be a true missing value.
#' sheet must be read in wrong.
#'
#' FIX added a function to replace NA strings with NA values

## fixing the model development, fitting code

#'
#'
aa<-tribble(
  ~m_id, ~src_species,         ~formula_text,                                 ~.f, ~family,
  "v1", "Aechmaea_mertensii",  "log(max_water)~log(diameter)",                 glm,  "gaussian",
  "v2", "Aechmaea_aquilega",   "log(max_water)~log(diameter) + log(num_leaf)", glm,  "gaussian"
)

aa |>
  rowwise() |>
  mutate(mf = list(as.formula(model_formula)))

tar_make("supp_size_model_fits",
         script = "02_process_data.R")


tar_load(supp_size_model_data,
         store = "store_process_data")


#'   Bromeliad detritus
#'
#'   correcting errors, and probably streamlining the modelling and imputation of, bromeliad detritus.
#'

tar_visnetwork(targets_only = TRUE,
               names = "bromeliad_detritus",
               script = "02_process_data.R")

#' need to look at correct_cardoso_detritus_wider
#' seems we need visit_id. check parent data


tar_load(detritus_wide,
         store = "store_process_data")
# its visit.x and visit.y instead of just visit_id . caused by a merge somewhere.

#' fixed error in make_detritus_wider to merge also by visit_id, thus preventing renamed columns

tar_make("bromeliad_detritus",
         script = "02_process_data.R")

#' now need to find detritus150_NA, why is it missing
#' load and glimpst detritus_wider_bromeliad_names
tar_load(detritus_wider_bromeliad_names,
         store = "store_process_data")
glimpse(detritus_wider_bromeliad_names)


# column detritus150_NA is just GONE. what happened here?
tar_load(broms_date,
         store = "store_process_data")
glimpse(broms_date)

View(broms_date |> filter(visit_id == 21))

#' instead, deciding to leave it as-is and not "correct" this part of cardoso,
#' since the column detritus150_NA is equivalent to detritus150_20000


tar_make("bromeliad_detritus",
         script = "02_process_data.R")

#' appears to be an error -- missing column names?
tar_load(detritus_wider_cardoso_corrected)
glimpse(detritus_wider_cardoso_corrected)


# tar_delete(detritus_wider,
#          store = "store_process_data")



## guts of the function correct_frenchguiana_detritus
detritus_wider_cardoso_corrected %>%
  filter(dataset_id == "211")
    # mutate(detritus0_150 = ifelse(dataset_id=="211", fpom_g, detritus0_150))%>%
    # mutate(detritus150_20000 = ifelse(dataset_id=="211", cpom_g, detritus150_20000))%>%
    # mutate(detritus20000_NA= ifelse(dataset_id=="211", dead_leaves, detritus20000_NA))

# what.. dataset 211 is just gone??



detritus_wider_cardoso_corrected %>% glimpse()

## and the column dataset_id is also gone??

broms_date
broms

visitnames
tar_load(fpom_brom,
         store = "store_process_data")

glimpse(fpom_brom)

broms_date |> names() |> sort()

##' decision == to comment out the detritus-renaming step in
##' correct_frenchguiana_detritus function. the role of this line was to
##' transfer data from qualitative columns such as "cpom" and "fpom" to the
##' correct quantitative columns. However these original source columns are no
##' longer there!

tar_make("bromeliad_detritus",
         script = "02_process_data.R")

#' issue with Nourages 2006 -- code that was written to correct an error now
#' doesn't work -- error fixed in database?
#'
#' from the code:
#'
#' ## Nourages 2006 (dataset 201) accidentally has particle counts in detritus
#' categories #this needs to be corrected in BWGdb but for now
#'
    # mutate(detritus30_150 = if_else(dataset_id==201,NA_real_, detritus30_150)) %>%
    # mutate(detritus0_30 = if_else(dataset_id==201,NA_real_, detritus0_30)) %>%
    # mutate(detritus150_300 = if_else(dataset_id==201,NA_real_, detritus150_300))

#' correcting now with ALL bromeliads
#' hitting a problem merging the trait data from the different parts of the database:
#' these animals don't have a fuzzy trait, apparently:
#   species_id taxon_level taxon_name    taxon_number
#        <int> <chr>       <chr>                <dbl>
# 1       8036 family      Dolicopodidae            9
# 2       8171 family      Psephenidae              9

#' FILTERING THEM OUT so remember to remove these lines in merge_trait_by_taxonomy!!
#'
# .lowest_taxonomic <- .lowest_taxonomic |>
#     filter(species_id != c(8036, 8171))


## missing the cpom data from french guana, my function
## correct_frenchguiana_detritus is now erroring constantly.

## let's see the network leading up to correct_frenchguiana_detritus looks like
tar_visnetwork(targets_only = TRUE,names =detritus_wider_correct_frenchguiana)

#' look first at broms_date
#'
tar_load(broms_date)
glimpse(broms_date)
# its gone! and several new columns are introduced
tar_load(broms_rename_unnest)
glimpse(broms_rename_unnest)
names(broms_rename_unnest) |> sort()

# I note that there are several columns that seem novel.
# Are these from new sites?

#### NEW approach: focus on final release

# how close are we?

tar_visnetwork(TRUE, "dats_date")
tar_make(dats_date)
# this is OK and needs no changes
## POSSIBLY fix the dates

tar_visnetwork(TRUE, "visit_no_81")


tar_visnetwork(TRUE, "traits")

tar_visnetwork(TRUE, "bromeliads_visit_no_81")
## stuck in the data processing pipeline

tar_visnetwork(TRUE, "abundance_no_81")

tar_visnetwork(TRUE, "synonymous_names")
tar_make(synonymous_names)



tar_visnetwork(TRUE, "spp_abundances_wide")
tar_make(synonymous_names)

## New mission -- separate imputation from error correction.


## back to drawing board -- just fix the pipeline as it exists

## frenchguiana

### looking just at abundance:

tar_visnetwork(TRUE, "abundance_no_zero")

## let abundance depend on one of the first bromeliad datasets, not the last it
## depeds on bromeliads because we remove all bromeliads from visit 81, then use
## those IDs to remove rows from abundance.

tar_load("diam_brom")
diam_brom
tar_load("visit_no_81")
visit_no_81
sort(visit_no_81$visit_id)

library(dplyr)

tar_load("abundance_no_zero")

tar_visnetwork(TRUE, "abundance_no_81")

## fixing more of this "formulae" nonsense
tar_load("detritus_wider_new_variables")
tar_load("model_table")
derive_modelling_information(model_table, detritus_wider_new_variables)

tar_make(modelling_information)
tar_load(modelling_information)
modelling_information

dd <- modelling_information |>
  # head(2) |>
  # mutate(nn = nrow(src_df))
  mutate(ff = list(purrr::safely(glm)(as.formula(fml_string), data = src_df)))

test <- dd |>
  select(m_id, ff) |>
  mutate(isnull = is.null(ff$result),
         mod = if_else(is.null(ff$result), list(NA), list(ff$result)))

test$mod



modelling_information[2,]$src_df |> map(names) |> unlist() |> sort()

modelling_information[2,]$src_df |> glimpse()

penguins |>
  nest_by(species) |>
  mutate(fml = "bill_len ~ bill_dep") |>
  mutate(mod = list(glm(as.formula(fml), data = data)))

names(modelling_information$src_df[[1]])

tar_load(detritus_wider_150_name_changed)
detritus_wider_150_name_changed |> View()


## continuing -- errors in detritus estimation
# -- does this go back to the missing detritus from one field site? which one?

tar_load(detritus_estimate_equation_filt)
debugonce(do_mutate_new_col)
do_mutate_new_col(detritus_estimate_equation_filt)

## instead, actually trying to add in the missing columns from Sinnamary 2011 and see if it helps

# Sinnamary 2011 is visit_id 311
library(dplyr)
visits |>
  filter(visit_id == 336) |> glimpse()


broms <- tar_read(broms, store = "store_download_data")
broms |> glimpse()
broms |>
  filter(visit_id == 336) |>
  # drop columns not recorded for this dataset
  select(where(~ !all(is.na(.x)))) |>
  View()

# Can visually confirm that in Sinnamary2011, visit ID 336, there is a single
# column missing in the dataset output. Where did it go?

## where is the right place to add this missing column back in?


tar_make("checked_data")

## back to checking this problem:

library(targets)
library(tidyverse)
tar_make("detritus_estimate_equation_filt")
debugonce(mutate_new_col)
do_mutate_new_col(detritus_estimate_equation_filt)

## trying again to make object
tar_make("detritus_estimated_with_equation")
## oka it seems
tar_load(detritus_estimate_equation_filt)
detritus_estimate_equation_filt
detritus_estimate_equation_filt$filtered_data[[3]] |> glimpse()
## turns out this thing is not even used!!

tar_load(observed_model_fit)
tar_load(modelling_information)
tar_load(detritus_wider_new_variables)
library(tidyverse)
## added browser, let's check it out
estimate_missing_detritus_new_site(.observed_model_fit = observed_model_fit,
                                   .modelling_information = modelling_information,
                                   .detritus_data = detritus_wider_new_variables)

tar_make()

## checking next step -- will need to put predictions in the correct part of dataframe
tar_make(detritus_all_preds)
tar_load(detritus_all_preds)
detritus_all_preds

tar_make(detritus_estimated_with_model)
tar_load(detritus_estimated_with_model)

## stuck on water volume
