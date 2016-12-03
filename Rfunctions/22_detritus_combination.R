# join together all the detritus into a single variable, with perhaps some
# supporting information. For example, a single "total detritus" column, and one
# to say if the answer was observed, or partly predicted, and over what exact
# range the predictions apply

# for the moment, this is the dataset that holds the predicitons:
predicted_detritus <- detritus_estimated_with_model$pred_data %>% bind_rows()

glimpse(predicted_detritus)
# no you'll just have to split it right up again.

detritus_estimated_with_model$pred_data %>%
  map(~ select(.x, bromeliad_id) %>% group_by(bromeliad_id) %>% tally)


detritus_category_df <- detritus_estimated_with_model$pred_data[[2]] %>%
  tbl_df() %>%
  # just the detritus
  select(bromeliad_id, starts_with("detritus")) %>%
  # remove any categories which are all NA
  # keep(~ negate(all)(is.na(.x))) # that's one option but I think its weak
  gather(detritus_category, detritus_amount, starts_with("detritus")) %>%
  group_by(detritus_category) %>%
  nest



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

fitted_and_obs_vals <- categories_ses %>%
  filter(!str_detect(detritus_category, "_se.fitted$"))

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
  unnest(summary_df)

detritus_total %>%
  select(-detritus_category) %>%
  .[["data"]]

  summarize(,
            data2 = bind_rows(data))


# here join on the se column, (what if there is two?). also can rename or drop detritus_category


