# join together all the detritus into a single variable, with perhaps some
# supporting information. For example, a single "total detritus" column, and one
# to say if the answer was observed, or partly predicted, and over what exact
# range the predictions apply

combine_detritus_predictions <- function(.detritus_estimated_with_model){
  # begin with just the detritus that has been predicted:
  detritus_all_predictions <- .detritus_estimated_with_model %>%
    select(pred_data) %>%
    as.list() %>% flatten %>%
    # get rid of anything not about detritus
    map(~ select(.x, bromeliad_id, dplyr::contains("fitted"), -starts_with("fpom"))) %>%
    reduce(full_join, by = "bromeliad_id")

  # make sure that this process did NOT DOUBLE ANY BROMELIADS
  detritus_all_predictions %>% group_by(bromeliad_id) %>% tally %>% .[["n"]]
  count_of_each_brom <- detritus_all_predictions %>% group_by(bromeliad_id) %>% tally %>% .[["n"]]
  assert_that(all(count_of_each_brom == 1))

  return(detritus_all_predictions)

}

# visdat::vis_dat(detritus_all_preds)

combine_all_detritus_values <- function(.detritus_wider_new_variables, .detritus_all_preds, .broms_date){
  # before combining observed and fitted values, check that they have no columns
  # in common
  # should be 0
  commonnames <- intersect(names(.detritus_all_preds) %>% discard(str_detect, "bromeliad_id"),
                           names(.detritus_wider_new_variables) %>% discard(str_detect, "bromeliad_id"))
  assert_that(length(commonnames) == 0)


  detritus_all_observations <- .detritus_wider_new_variables %>%
    left_join(.detritus_all_preds, by = "bromeliad_id") %>%
    select(bromeliad_id, starts_with("detritus"))

  det_all_obs_long <- detritus_all_observations %>%
    # there is only one column with character. drop that one!
    select(#-detritus0_150_src,
           -`detritus>10000`) %>%
    gather(detritus_category, detritus_amount, starts_with("detritus"), convert = TRUE) %>%
    # we can filter out NAs directly since we are doing this at the bromeliad level:
    filter(!is.na(detritus_amount))

  # a quick test:
  split_cats <- det_all_obs_long %>%
    filter(!str_detect(detritus_category, "_se")) %>%
    separate(detritus_category, c("category", "xy"), sep = "\\.", fill = "right", extra = "merge") %>%
    filter(!is.na(xy))
  split_cats %>%
    group_by(bromeliad_id, category) %>%
    nest %>%
    filter(map_dbl(data, nrow) > 2) %>%
    verify(nrow(.) == 0)
  return(det_all_obs_long)
}

# maybe print this as a table
# detritus_long_categories %>% select(detritus_category) %>% distinct %>% View

# # note that this is only for the dublicates (.x and .x.x etc) -- another way of generating
# split_cats %>%
#   select(-xy) %>%
#   spread(category, detritus_amount) %>%
#   visdat::vis_miss()

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

split_detritus_categories <- function(.detritus_long_filtered) {

  det_long_just_fitted <- .detritus_long_filtered %>%
    filter(!str_detect(detritus_category, "_se.fitted"))

  det_long_broken_up <- det_long_just_fitted %>%
    mutate(obs_or_fit = if_else(str_detect(detritus_category, "_fitted"), true = "fit", false = "obs")) %>%
    mutate(detritus_broken_up = str_split(detritus_category, "_")) %>%
    tbl_df

  return(det_long_broken_up)
}

extract_numeric_min_max <- function(.det_long_broken_up){
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

  output <- .det_long_broken_up %>%
    mutate(detritus_max = map(detritus_broken_up, find_range)) %>%
    unnest(detritus_max) %>%
    # tests here?
    # can ditch the broken_up col
    select(-detritus_broken_up)

  # check -- are the ranges unique?
  output %>%
    group_by(min_detritus, max_detritus, bromeliad_id) %>%
    nest %>%
    mutate(nr = map_dbl(data, nrow)) %>%
    # check -- there should be no duplicates
    filter(nr > 1) %>% verify(nrow(.)==0)

  return(output)
}


# # do the values of the range and the range itself together make sense?
# det_long_min_max %>%
#   select(detritus_category, min_detritus, max_detritus) %>%
#   distinct()

add_consecutive_detritus_col <- function(.det_long_min_max) {
  detect_consecutive <- function(df){
    # single-row dataframes don't have anything to worry about
    if (nrow(df) == 1){
      answer <- TRUE
    } else {
      answer <- identical(c(df$min_detritus[-1], Inf),
                          df$max_detritus)
    }
    return(answer)
  }

  # within each bromeliad, check to make sure that sequence is consecutive
  .det_long_min_max %>%
    group_by(bromeliad_id) %>%
    # arrange in ascending order
    arrange(min_detritus, max_detritus) %>%
    nest %>%
    # "each interval should begin where the last one ended"
    mutate(is_consec = map_lgl(data, detect_consecutive))

}

create_detritus_summary <- function(.det_long_check_consec){
  .det_long_check_consec %>%
    mutate(summary_df = map(data, ~tibble(min_det = min(.x$min_detritus),
                                              max_det = max(.x$max_detritus),
                                              obs_or_fit = paste0(unique(.x$obs_or_fit), collapse = "_"),
                                              total_detritus = sum(.x$detritus_amount)))) %>%
    select(-data) %>%
    unnest(summary_df)

}

add_detritus_summary <- function(.detritus_wider_150_name_changed, .detritus_summary){
  .detritus_wider_150_name_changed %>%
    select(-starts_with("detritus"), -starts_with("fpom")) %>%
    left_join(.detritus_summary, by = "bromeliad_id")
}



# we don't have every bromeliad here?
# detritus_wider_150_name_changed %>% anti_join(detritus_summary, by = "bromeliad_id") %>% visdat::vis_miss(cluster = TRUE)
