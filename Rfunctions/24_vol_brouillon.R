#  working with the size data
remake::dump_environment()
size_dat <- read_csv("data-intermediate/size_all_data.csv")

make_model_data <- function(){
  frame_data(
    ~m_id, ~src_dat,                             ~xvar,                   ~yvar,          ~.f,     ~target_dat,      ~family,
    "v1", "mertensii",                           "~log(Diam1)",           "~log(Vmax)",    glm,      286,              "gaussian",
    "v2", c("aquilega_Biog","aquilegaKT"),       "~log(Diam1) + log(NL)", "~log(Vmax)",    glm,      286,              "gaussian"
  )  %>%
    mutate(xvar = xvar %>% map(as.formula),
           yvar = yvar %>% map(as.formula))
}

mods <- make_model_data()

renamed_size_dat <- size_dat %>% rename(dataset_id = filename)

derive_modelling_information_simpler <- function(.model_table, .obs_data){
  .model_table %>%
    # select the required input rows
    mutate(src_df = map(src_dat,
                        ~ .obs_data %>%
                          # TODO: ? add select() to contain only some variables??
                          filter(dataset_id %in% .x)),
           # create modelling function
           fml = map2(.x = xvar, .y = yvar, ~ formulae(.y, .x)),
           fml = flatten(fml)) %>%
    mutate(x_symb = xvar %>% map(find_symbols),
           y_symb = yvar %>% map(find_symbols))
}

mod_info <- derive_modelling_information_simpler(.model_table = mods, .obs_data = renamed_size_dat)

mod_info %>% glimpse

fit_models <- mod_info %>% do_fit_predictive_model()

fit_models$predicting_model[[2]][[1]] %>% tidy
# try looking in some various FrenchGuianaAechmea2007

