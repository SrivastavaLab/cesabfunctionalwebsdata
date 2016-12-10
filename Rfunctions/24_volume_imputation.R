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
