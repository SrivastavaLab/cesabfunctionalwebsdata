

source("Rfunctions/03_detritus_equations.R")

equation_table <- create_equation_table()

## could easily add an "equation meant to be used on dataset" arguement, which
## would convert used_on_dataset to dataset_name, then add to ggtitle

## generate plots & add to data.frame
equation_plots <- plot_data_with_equation_table(equation_table,
                                                .detritus_data = detritus_wider_FG_detritus_corrected)

detritus_wider_correct_frenchguiana %>%
  filter(dataset_id == 6)

# plot
equation_plots %>%
  select(.out) %>%
  walk(print)


# applying functions to data ----------------------------------------------

detritus_estimate_function_filt <- do_filter_dataset_id(equation_table, detritus_wider_FG_detritus_corrected)

new_detritus <- do_mutate_new_col(detritus_estimate_function_filt)

new_detritus$.out %>% map(select, 36) %>% map(head)

# what if it is a model tho -----------------------------------------------

# first, create any combinations of detritus values which are needed in later
# modelling

detritus_wider_new_variables <- add_new_columns_for_prediction(detritus_wider_FG_detritus_corrected)

detritus_wider_new_variables %>%
  filter(!is.na(detritus10_1500_2000_NA)) %>%
  select(dataset_id) %>%
  distinct

## first row -- why not 136??

# create the table of formulae
model_table <- create_model_table()

modelling_information <- derive_modelling_information(model_table, detritus_wider_new_variables)

# demo of bootstrapping a model
mi <- modelling_information %>%
  mutate(boot_src_dat = map(src_df, modelr::bootstrap, n = 10))

mi$boot_src_dat %>% str(max.level = 4)


test_mod <-
# TODO cut back to only what is needed later


# add back in what is needed for plotting

plotting_information <- test_mod %>%
  select(m_id, src_df, predicting_model) %>%
  left_join(modelling_information %>%
              select(m_id, target_dat, x_funs, y_funs, x_vars, y_vars),
            by = "m_id")


make_prediction_xs <- function(m_id, src_df, x_vars) {
  dd <- src_df[[1]]

  dd %>%
    .[[x_vars]] %>%
    seq_range(n = 30) %>%
    list(.) %>%
    set_names(x_vars) %>%
    as.data.frame
}

# create the x range over which all the models should be predicted. TODO make n a variable??
plotting_information %>%
  select(m_id, src_df, x_vars) %>%
  by_row(make_prediction_xs %>% lift, .to = "xs_range") %>%
  select(-src_df, -x_vars)

# genrate an appropriate prediction funciton for each varible

make_prediction_df <- function(m_id, src_df, predicting_model, y_vars){
  # browser()

  dd <- src_df[[1]]

  predict_dat <- partial(add_predictions, data = dd, var = y_vars)

  modlist <- predicting_model[[1]] # just because it is a list-column, get the first element (which is a list lol)
  map_df(modlist, predict_dat)
}


wtf <- plotting_information %>%
  select(m_id, src_df, predicting_model, y_vars) %>%
  # mutate(predicting_model = flatten(predicting_model)) %>%
  by_row(make_prediction_df %>% lift, .to = "curve_data")

wtf$curve_data[[1]] %>% glimpse

# here is where we bootstrap if we bootstrap
plotting_info_pred <- plotting_information %>%
  select(m_id, src_df, x_vars, predicting_model, y_vars) %>%
  by_row(make_prediction_df %>% lift, .to = "curve_data") %>%
  left_join(plotting_information %>%
              select(m_id, x_funs, y_funs))



plot_fn <- function(src_df, x_funs, y_funs, x_vars, y_vars, curve_data,...){
  dd <- src_df[[1]]

  # give "identity" if x_funs or y_funs is ""
  switch_trans <- function(x) switch(x, "log" = "log", "identity")
  # get correct axis transformations
  xt <- switch_trans(x_funs)
  yt <- switch_trans(y_funs)

  # browser()
  # back transformation function for y axis (x not necessary because it is in the function?)
  back_trans <- switch(y_funs, log = exp, I)

  ld <- curve_data[[1]] %>%
    select_(xs = x_vars,
            ys = y_vars) %>%
    mutate(ys = back_trans(ys))

  dd %>%
    select_(xs = x_vars,
           ys = y_vars) %>%
    ggplot(aes(x = xs, y = ys)) +
    geom_point() +
    geom_line(data = ld) +
    labs(x = x_vars, y = y_vars) +
    coord_trans(x = xt, y = yt)

}

plots <- plotting_info_pred %>%
  by_row(plot_fn %>% lift, .to = "model_fit_plot")

plots %>% select(model_fit_plot) %>% walk(print)


## for validating

list(rmse, rsquare, mae) %>%
  map(invoke_rows, .d = test_mod %>% select(m_id, data = src_df, model = predicting_model), .collate = "cols")
# TODO extract these into a data_frame

list(rmse, rsquare, mae) %>%
  invoke_map(list(model = test_mod$predicting_model,
                  data = test_mod$src_df))

# split by target or source data, predict (use same name) then combine -- labels
# to new column called something like obs_or_prediction

df_list <- test %>%
  select(target_dat, src_newv, mod) %>%
  # get the targetd df just as above
  mutate(target_df = map(target_dat, ~ detritus_wider_correct_frenchguiana %>%
                        filter(dataset_id %in% .x)),
         # add predictions to it
         target_df_pred = map2(target_df, mod, ~ add_predictions(.x, .y[[1]], "detritus0_NA")),
         target_df_pred = map(target_df_pred, ~ .x %>% mutate(detritus0_NA = exp(detritus0_NA)))
  ) %>%
  {list(observed = .[["src_newv"]],
        predicted = .[["target_df_pred"]]
        )}

df_list %>%
  flatten %>%
  bind_rows(.id = "detritus0_NA_pred") %>%
  ggplot(aes(x = diameter, y = detritus0_NA, colour = detritus0_NA_pred)) +
  geom_point() +
  coord_trans(y = "log")


## maybe one huge data.frame is not helpful -- try invoke_rows with smaller, function-specific data_frames that can then be gathered
