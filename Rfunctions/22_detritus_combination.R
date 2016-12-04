# join together all the detritus into a single variable, with perhaps some
# supporting information. For example, a single "total detritus" column, and one
# to say if the answer was observed, or partly predicted, and over what exact
# range the predictions apply

remake::dump_environment()

combine_detritus_predictions <- function(.detritus_estimated_with_model){
  # begin with just the detritus that has been predicted:
  detritus_all_predictions <- .detritus_estimated_with_model %>%
    select(pred_data) %>% as.list() %>% flatten %>%
    # get rid of anything not about detritus
    map(~ select(.x, bromeliad_id, dplyr::contains("fitted"), -starts_with("fpom"))) %>%
    reduce(full_join, by = "bromeliad_id")

  # make sure that this process did NOT DOUBLE ANY BROMELIADS
  detritus_all_predictions %>% group_by(bromeliad_id) %>% tally %>% .[["n"]]
  count_of_each_brom <- detritus_all_predictions %>% group_by(bromeliad_id) %>% tally %>% .[["n"]]
  assert_that(all(count_of_each_brom == 1))

  return(detritus_all_predictions)

}

detritus_all_preds <- combine_detritus_predictions(detritus_estimated_with_model)

visdat::vis_dat(detritus_all_preds)

combine_all_detritus_values <- function(.detritus_wider_new_variables, .detritus_all_preds, .broms_date){  # then get the bromeliads which were never estimated for anything:
   detritus_all_observations <- .detritus_wider_new_variables %>%
     anti_join(.detritus_all_preds, by = "bromeliad_id") %>%
    select(bromeliad_id, starts_with("detritus"))

  # nothing in common
  common_broms <- intersect(detritus_all_observations$bromeliad_id,
                            .detritus_all_preds$bromeliad_id)

  assert_that(length(common_broms) == 0)

  # nothing left out
  missing_broms <- setdiff(.broms_date$bromeliad_id, c(detritus_all_observations$bromeliad_id,
                                                       .detritus_all_preds$bromeliad_id))

  assert_that(length(missing_broms) == 0)

  # gather each then put them on top of each other
  det_all_pred_long <- .detritus_all_preds %>%
    gather(detritus_category, detritus_amount, starts_with("detritus")) %>%
    # we can filter out NAs directly since we are doing this at the bromeliad level:
    filter(!is.na(detritus_amount))

  det_all_obs_long <- detritus_all_observations %>%
    # there is only one column with character. drop that one!
    select(-detritus0_150_src) %>%
    gather(detritus_category, detritus_amount, starts_with("detritus")) %>%
    # we can filter out NAs directly since we are doing this at the bromeliad level:
    filter(!is.na(detritus_amount))

  all_detritus <- bind_rows(det_all_pred_long, det_all_obs_long)

  return(all_detritus)
}

detritus_long_categories <- combine_all_detritus_values(detritus_wider_new_variables, detritus_all_preds, broms_date)

detritus_long_categories %>% select(detritus_category) %>% distinct %>% View

# are .x and .y versions of the same variable actually different?!

split_cats <- detritus_long_categories %>%
  filter(!str_detect(detritus_category, "_se")) %>%
  separate(detritus_category, c("category", "xy"), sep = "\\.", fill = "right", extra = "merge") %>%
  filter(!is.na(xy))
split_cats %>%
  group_by(bromeliad_id, category) %>%
  nest %>%
  filter(map_dbl(data, nrow) > 2) %>%
  verify(nrow(.) == 0)
# note that this is only for the dublicates (.x and .x.x etc) -- another way of generating
split_cats %>%
  select(-xy) %>%
  spread(category, detritus_amount) %>%
  visdat::vis_miss()

# many categories of detritus are the result of summation for the purposes of
# regression -- they should be dropped! So keep only those columns which either
# were there at the start or have "fitted" in the name.

filter_just_orig_fitted <- function(.detritus_wider_df, .detritus_long_categories) {

  # First, clear the x and y thing away
  detritus_long_clean_cats <- .detritus_long_categories %>%
    mutate(detritus_category = str_replace_all(detritus_category, "\\.[xy]", ""))

  # get the names of the last version of the data before these "extra" names were added.
  starting_names <- .detritus_wider_df %>%
    names %>%
    # extra
    keep(~ str_detect(.x, "^detritus")) %>%
    discard(~ str_detect(.x, "detritus0_150_src"))

  # find the ones which are estimated from models (i.e. "fitted")
  all_fitted <- detritus_long_clean_cats$detritus_category %>%
    unique %>%
    keep(~ str_detect(.x, "fitted"))

  detritus_long_clean_cats %>%
    filter(detritus_category %in% c(starting_names, all_fitted))
}


detritus_long_filtered <- filter_just_orig_fitted(detritus_wider_150_name_changed, detritus_long_categories)

det_long_just_fitted <- detritus_long_filtered %>%
  filter(!str_detect(detritus_category, "_se.fitted"))

det_long_broken_up <- det_long_just_fitted %>%
  mutate(obs_or_fit = if_else(str_detect(detritus_category, "_fitted"), true = "fit", false = "obs")) %>%
  mutate(detritus_broken_up = str_split(detritus_category, "_")) %>%
  tbl_df

det_long_broken_up

find_range <- function(x){
  # NA at the end of a string (or near the end) indicates that that sieve
  # included detritus up to the biggest size present
  is_NA_present <- any(str_detect(x, "NA"))
  # use readr functions to parse it to numbers

  numeric_detritus <- quietly(parse_number)(x)[["result"]]

  if(is_NA_present){
    maxval <- Inf
  } else {
    maxval <- max(numeric_detritus, na.rm = TRUE)
  }

  minval <- min(numeric_detritus, na.rm = TRUE)

  data.frame(min_detritus = minval, max_detritus = maxval)
}

det_long_min_max <- det_long_broken_up %>%
  mutate(detritus_max = map(detritus_broken_up, find_range)) %>%
  unnest(detritus_max) %>%
  # tests here?
  # can ditch the broken_up col
  select(-detritus_broken_up)

# check -- are the ranges unique?
cats_in_brom <- det_long_min_max %>%
  group_by(min_detritus, max_detritus, bromeliad_id) %>%
  nest %>%
  mutate(nr = map_dbl(data, nrow))

# check -- there should be no duplicates
cats_in_brom %>% filter(nr > 1) %>% verify(nrow(.)==0)
cats_in_brom %>% filter(nr > 1) %>% .[["data"]]


# within each bromeliad, check to make sure that sequence is consecutive
for_testing <- det_long_min_max %>%
  group_by(bromeliad_id) %>%
  arrange(min_detritus, max_detritus) %>%
  nest %>%
  filter(map_dbl(data, nrow) > 1) %>%
  mutate(is_consec = map_lgl(data, ~ identical(c(.x$min_detritus[-1],Inf), .x$max_detritus)))

for_testing$is_consec[[1]]

for_testing[1,"is_consec"][[1]]

for_testing %>% filter(!is_consec) %>% .[1,]

## oh crap, there are some detritus values missing!
broms_date %>% filter(bromeliad_id == "5181")

# what's missing??
det_long_min_max %>%
  select(bromeliad_id, detritus_category, detritus_amount) %>%
  spread(detritus_category, detritus_amount) %>%
  visdat::vis_miss(cluster = TRUE)

# what's happening is tha some detritus_category names are not parsing, so we
# are getting the wrong min and max. Either go back and re-change the offending names, or do this over.
det_long_min_max %>%
  select(detritus_category) %>%
  distinct

summarize_detritus <- function(det_df){

  # drop the ones that are all empty
  categories_ses <- detritus_category_df %>%
    # need to get rid of the detritus0_150 and detritus0_150_combo columns -- deal
    # with this earlier? if the SE here is used as a weight, then that will
    # "filter through" to later estimates and be caught below
    filter(!detritus_category %in% c("detritus0_150", "detritus0_150_src")) %>%
    mutate(is_empty = map_lgl(data, ~ all(is.na(.x$detritus_amount)))) %>%
    filter(!is_empty) %>%
    select(-is_empty)

  # we need to split up _fitted values (to get the range) but I want to leave the
  # _se.fitted alone. so just filter out the _se

  # get the unfiltered things
  categories_ses %>% anti_join(fitted_and_obs_vals, by = "detritus_category")

  get_det_vals <- fitted_and_obs_vals %>%
    mutate(obs_or_fit = if_else(str_detect(detritus_category, "_fitted"), true = "fit", false = "obs")) %>%
    mutate(detritus_broken_up = str_split(detritus_category, "_"))

  find_range <- function(x){
    # NA at the end of a string (or near the end) indicates that that sieve
    # included detritus up to the biggest size present
    is_NA_present <- any(str_detect(x, "NA"))
    # use readr functions to parse it to numbers

    numeric_detritus <- parse_number(x)

    if(is_NA_present){
      maxval <- Inf
    } else {
      maxval <- max(numeric_detritus, na.rm = TRUE)
    }

    minval <- min(numeric_detritus, na.rm = TRUE)

    data.frame(min_detritus = minval, max_detritus = maxval)
  }

  detritus_total <- get_det_vals %>%
    mutate(detritus_max = map(detritus_broken_up, find_range)) %>%
    unnest(detritus_max) %>%
    # tests here?
    # can ditch the broken_up col
    select(-detritus_broken_up)

  # test if they are consecutive

  consec_test_vec <- detritus_total %>%
    # by using lead we can be cunning: skipping the final maximum size (which may be Inf)
    mutate(is_consec = lead(min_detritus) == max_detritus) %>%
    .[["is_consec"]]
  # should all be true except the last (which is NA)
  all(consec_test_vec[-length(consec_test_vec)])

  # if that passed, add em together.There are two things to add -- the column "obs_or_fit" and the data itself
  det_by_bromeliad <- detritus_total %>%
    select(-detritus_category) %>%
    unnest(data) %>%
    group_by(bromeliad_id) %>%
    nest

  det_by_bromeliad$data[[1]]

  det_by_bromeliad %>%
    mutate(summary_df = map(data, ~data_frame(min_det = min(.x$min_detritus),
                                              max_det = max(.x$max_detritus),
                                              obs_or_fit = paste0(.x$obs_or_fit, collapse = "_"),
                                              total_detritus = sum(as.numeric(.x$detritus_amount))))) %>%
    unnest(summary_df) %>%
    select(-data)
}

detritus_total_model <- detritus_estimated_with_model %>%
  mutate(det_sum = map(pred_data, summarize_detritus)) %>%
  select(det_sum) %>%
  unnest

detritus_total_model


detritus_total_obs <- detritus_wider_new_variables %>%
  anti_join(detritus_total_model, by = "bromeliad_id") %>%
  group_by(dataset_id, visit_id) %>%
  nest %>%
  mutate(det_sum = map(data, summarize_detritus))


detritus_total_obs %>% View

# here join on the se column, (what if there is two?). also can rename or drop detritus_category


