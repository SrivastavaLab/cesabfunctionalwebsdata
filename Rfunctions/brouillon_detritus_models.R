

## could easily add an "equation meant to be used on dataset" arguement, which
## would convert used_on_dataset to dataset_name, then add to ggtitle

# plot
detritus_model_plots %>%
  select(model_fit_plot) %>%
  walk(print)


# applying functions to data ----------------------------------------------

new_detritus <- do_mutate_new_col()

detritus_estimated_with_equation$.out %>% map(select, 36) %>% map(head)

# what if it is a model tho -----------------------------------------------

detritus_wider_new_variables %>%
  filter(!is.na(detritus10_1500_2000_NA)) %>%
  select(dataset_id) %>%
  distinct

## first row -- why not 136??



# demo of bootstrapping a model
mi <- modelling_information %>%
  mutate(boot_src_dat = map(src_df, modelr::bootstrap, n = 10))

mi$boot_src_dat %>% str(max.level = 4)


# add back in what is needed for plotting


detritus_model_plots %>% select(model_fit_plot) %>% walk(print)



# #pitilla 2004 dissection visit 66 ---------------------------------------
remake::dump_environment()
## creating a dataset for generating a model.

library(visdat)
detritus_wider_new_variables %>%
  filter(dataset_id == 56, visit_id != 51) %>%
  select(starts_with("det")) %>%
  keep(~ !all(is.na(.x))) %>%
  vis_dat()


test_2_ways <- detritus_wider_new_variables %>%
  mutate(detritus0_NA_sum = if_else(visit_id == 51,
                                    true  = detritus0_150 + detritus150_850 + detritus850_20000 + detritus20000_NA,
                                    false = detritus0_150 + detritus150_850 + detritus850_1500 + detritus1500_20000 + detritus20000_NA),
         detritus0_NA_sum2 = detritus0_150 + detritus150_850 + detritus850_20000_sum + detritus20000_NA)

test_2_ways$detritus0_NA_sum %>% all.equal(test_2_ways$detritus0_NA_sum2)

# true


test_2_ways %>%
  filter(dataset_id==56) %>%
  filter(detritus0_NA_sum != detritus0_NA_sum2)
  select(starts_with("detritus0_NA")) %>%
  vis_miss()


# add to original data ----------------------------------------------------


require(prediction)


detritus_estimated_with_model %>%
  mutate(predicting_model = flatten(predicting_model),
         new_values = map2(predicting_model, incoming_data, prediction)) %>%
  select(m_id, incoming_data, new_values) %>%
# %>%
#   mutate(new_values = map2(predicting_model, incoming_data, ~ prediction(model = .x[[1]],
  #                                                                          data  = .y))) %>%
  mutate(add_new = map2(incoming_data, new_values, bind_cols)) %>%
  unnest(add_new) %>%
  glimpse




fit_to_real_life$pred_data %>% map(select, 36) %>% map(head)


summary_model_predict

modelr_summaries <- function(m_id, predicting_model, src_df){

  m <- predicting_model[[1]][[1]]
  list(rmse = rmse, rsquare = rsquare, mae = mae) %>%
    invoke_map(model = m, data = src_df[[1]])
}

observed_model_fit %>%
  select(m_id, predicting_model, src_df) %>%
  by_row(modelr_summaries %>% lift) %>%
  mutate(outcol = map(.out, as.data.frame)) %>%
  unnest(outcol)



observed_model_fit$predicting_model %>% flatten %>% map(glance)

remake::dump_environment()
observed_model_fit %>%
  mutate(predicting_model = flatten(predicting_model)) %>%
  mutate(mod_tidy = map(predicting_model, tidy)) %>%
  unnest(mod_tidy)


#
# ## for validating
#
# list(rmse, rsquare, mae) %>%
#   map(invoke_rows, .d = test_mod %>% select(m_id, data = src_df, model = predicting_model), .collate = "cols")
# # TODO extract these into a data_frame
#
# list(rmse, rsquare, mae) %>%
#   invoke_map(list(model = test_mod$predicting_model,
#                   data = test_mod$src_df))
#
# # split by target or source data, predict (use same name) then combine -- labels
# # to new column called something like obs_or_prediction
#
# df_list <- test %>%
#   select(target_dat, src_newv, mod) %>%
#   # get the targetd df just as above
#   mutate(target_df = map(target_dat, ~ detritus_wider_correct_frenchguiana %>%
#                         filter(dataset_id %in% .x)),
#          # add predictions to it
#          target_df_pred = map2(target_df, mod, ~ add_predictions(.x, .y[[1]], "detritus0_NA")),
#          target_df_pred = map(target_df_pred, ~ .x %>% mutate(detritus0_NA = exp(detritus0_NA)))
#   ) %>%
#   {list(observed = .[["src_newv"]],
#         predicted = .[["target_df_pred"]]
#         )}
#
# df_list %>%
#   flatten %>%
#   bind_rows(.id = "detritus0_NA_pred") %>%
#   ggplot(aes(x = diameter, y = detritus0_NA, colour = detritus0_NA_pred)) +
#   geom_point() +
#   coord_trans(y = "log")


## maybe one huge data.frame is not helpful -- try invoke_rows with smaller, function-specific data_frames that can then be gathered
