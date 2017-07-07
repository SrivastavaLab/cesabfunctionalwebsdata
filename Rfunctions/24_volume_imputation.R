#  working with the size data

supp_data_rename <- function(.supp_data_additional){
  .supp_data_additional %>%
    rename(diameter = Diam1, num_leaf = NL, max_water = Vmax) %>%
    select(-Diam2)
}

# generate the table of the models we are going to calculate with the Supplementary data
#  src_dat defines the subset of the data used for the model -- here, the species name used
make_model_data <- function(){
  frame_data(
    ~m_id, ~src_species,         ~xvar,                            ~yvar,          ~.f,       ~family,
    "v1", "Aechmaea_mertensii",  "~log(diameter)",                 "~log(max_water)",    glm, "gaussian",
    "v2", "Aechmaea_aquilega",   "~log(diameter) + log(num_leaf)", "~log(max_water)",    glm, "gaussian"
  )  %>%
    mutate(xvar = xvar %>% map(as.formula),
           yvar = yvar %>% map(as.formula))
}

derive_modelling_information_simpler <- function(.model_table, .obs_data){
  .model_table %>%
    # select the required input rows
    mutate(src_df = map(src_species,
                        ~ .obs_data %>%
                          # TODO: ? add select() to contain only some variables??
                          filter(species %in% .x)),
           # create modelling function
           fml = map2(.x = xvar, .y = yvar, ~ formulae(.y, .x)),
           fml = flatten(fml)) %>%
    mutate(x_symb = xvar %>% map(find_symbols),
           y_symb = yvar %>% map(find_symbols))
}

make_model_target_data <- function(){
  frame_data(
    ~m_id, ~target_dat,
    "v1",  c("286"),
    "v2",  c("301", "296")
  )
}

# add in the target data (this is not strictly necessary for the model itself,
# but i guess  you should not make one of these imputation models if you don't
# know why you are making it)

fit_size_models_to_data <- function(.mod_info, .supp_size_model_data){
  .mod_info %>%
    left_join(.supp_size_model_data) %>%
    do_fit_predictive_model()
}

# combining these data with observed data and creating predictions ---


# this function will need to be divided in two to preserve the models used for imputation? maybe?
predict_add_imputed <- function(.supp_size_model_fits, .bromeliad_detritus) {

  # select the model and the target data


  # check that the number of rows has not changed

  # unnest the target data, duplicating models where necessary
  supp_size_model_ready_to_apply <- .supp_size_model_fits %>%
    unnest(target_dat, .drop = FALSE) %>%
    mutate(predicting_model = flatten(predicting_model))

  # select just the datasets which are needed for this analysis.
  size_data_to_impute <- .bromeliad_detritus %>%
    semi_join(supp_size_model_ready_to_apply, by = c("visit_id" = "target_dat")) %>%
    group_by(visit_id) %>%
    nest

  predicted_max_volume <- size_data_to_impute %>%
    left_join(supp_size_model_ready_to_apply, by = c("visit_id" = "target_dat")) %>%
    mutate(predicted_size = map2(data, predicting_model, add_predictions, var = "predicted_water"))

  visit_and_predictions <- predicted_max_volume %>%
    unnest(predicted_size) %>%
    select(visit_id, bromeliad_id, predicted_water)

  # finally combine with the final thing

  observed_and_guesses <- .bromeliad_detritus %>%
    # select the maximum volume
    select(visit_id, bromeliad_id, max_water) %>%
    # remove sites which were not imputed
    anti_join(visit_and_predictions, by = "visit_id") %>%
    # then stick the new values on the bottom (this should fill in NA for the new volume things)
    bind_rows(visit_and_predictions)

  # a little check to make sure everything is OK
  stopifnot(nrow(observed_and_guesses) == nrow(.bromeliad_detritus))

  # it should be impossible to predict an observed number! It should only have been done where observations are absent
  observed_and_guesses %>%
    filter(!is.na(max_water) & !is.na(predicted_water)) %>%
    {stopifnot(nrow(.) == 0)}


  # finally combine with the final thing

  new_water <- observed_and_guesses %>%
    mutate(max_water_combined = if_else(!is.na(predicted_water), predicted_water, max_water),
           max_water_estimated = if_else(!is.na(predicted_water), "estimated", "observed")) %>%
    # can drop predicted size -- but let's keep it anyhow
    # select(predicted_water) %>%
    # drop max water because it is in the original data
    select(-max_water)

  .bromeliad_detritus %>%
    left_join(new_water, by = c("visit_id", "bromeliad_id"))

}

# This last bit of code supplies the workflow that can be adapted to the next
# thing too -- the recorded, estimated water values

read_volume_estimated <- function(fname) {
  read_csv(fname, col_types = cols(
    bromeliad_id = col_character(),
    max_water = col_double()
  ))
}

# There's a problem in here!! produces NA visits -- probably because visit was
# not in the "added in data"

add_24_volum_data <- function(.volume_estimated, .bromeliad_detritus_vol_imputed) {

  # prepare for merging -- take only the non-NA values
  .volume_estimated_values <- .volume_estimated %>%
    filter(!is.na(max_water)) %>%
    rename(max_water_from_24_volume = max_water)

  # follow this workflow

  # replace .bromeliad_detritus with whatever
  observed_and_guesses <- .bromeliad_detritus_vol_imputed %>%
    # working right now only with volume measurements --
    # use "combined" max water, which contains a few values from the body.
    # max_water_estimated is here for bookkeeping.
    select(bromeliad_id, max_water_combined, max_water_estimated) %>%
    # remove sites which were not estimated
    anti_join(.volume_estimated_values, by = "bromeliad_id") %>%
    # then stick the new values on the bottom (this should fill in NA for the new volume things)
    bind_rows(.volume_estimated_values)

  # a little check to make sure everything is OK
  stopifnot(nrow(observed_and_guesses) == nrow(.bromeliad_detritus_vol_imputed))

  # it should be impossible to predict an observed number! It should only have
  # been done where observations are absent
  observed_and_guesses %>%
    filter(!is.na(max_water_combined) & !is.na(max_water_from_24_volume)) %>%
    {stopifnot(nrow(.) == 0)}


  new_values <- observed_and_guesses %>%
    mutate(max_water_combined = if_else(!is.na(max_water_from_24_volume),
                                        max_water_from_24_volume,
                                        max_water_combined),
           max_water_estimated_2 = if_else(!is.na(max_water_from_24_volume),
                                         "estimated",
                                         max_water_estimated)) #%>%
    # can drop predicted size
    # select(-max_water_from_24_volume)

  # browser()

  # take only the bromeliad_id and the new water (max_water_combined) and the new estimated column

  # need to clear this up by not duplicating columns
  old_data_minus_intermediate_error <- .bromeliad_detritus_vol_imputed %>%
    select(-max_water_combined, -max_water_estimated)


  combined_water <- old_data_minus_intermediate_error %>%
    left_join(new_values, by = "bromeliad_id")


  with(combined_water, table(max_water_estimated, max_water_estimated_2))

  combined_water %>%
    select(-max_water_estimated) %>%
    rename(max_water_estimated = max_water_estimated_2)

}
