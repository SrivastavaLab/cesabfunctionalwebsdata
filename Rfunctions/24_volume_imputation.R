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
    unnest(predicted_size)

  # finally combine with the final thing

  observed_and_guesses <- .bromeliad_detritus %>%
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


  observed_and_guesses %>%
    mutate(max_water_combined = if_else(!is.na(predicted_water), predicted_water, max_water),
           max_water_imputed = if_else(!is.na(predicted_water), "imputed", "observed")) %>%
    # can drop predicted size
    select(-predicted_water)
}

# This last bit of code supplies the workflow for all the rest


