

# what if it is a model tho -----------------------------------------------

detritus_wider_new_variables %>%
  filter(!is.na(detritus10_1500_2000_NA)) %>%
  select(dataset_id) %>%
  distinct

## first row -- why not 136??


mi$boot_src_dat %>% str(max.level = 4)


# add back in what is needed for plotting

remake::dump_environment()
detritus_model_plots[10] %>% select(model_fit_plot) %>% walk(print)

detritus_model_plots$model_fit_plot[[10]] +
  coord_trans()+
  stat_function(fun = function(x) exp(0.694 * log(x)- 0.468), colour = "forestgreen")


c# #pitilla 2004 dissection visit 66 ---------------------------------------
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
  select(starts_with("detritus0_NA")) %>%
  vis_miss()


detritus_wider_new_variables %>%
  select(starts_with("detritus")) %>%
  vis_miss_ly()

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
  unnest(mod_tidy) %>% tail


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


## redownload
require(remake)


# guessing cardoso --------------------------------------------------------

detritus_wider_new_variables %>%
  filter_(.dots = list("!is.na(detritus150_850)")) %>%
  ggplot(aes(x = detritus150_850, y = detritus0_150_combo, colour = dataset_name)) +
  geom_missing_point(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  theme_dark()


detritus_wider_new_variables %>%
  filter_(.dots = list("!is.na(detritus150_850)")) %>%
  ggplot(aes(x = detritus150_850, y = detritus0_150_combo, colour = dataset_name)) +
  geom_missing_point(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  theme_dark() #+
  # coord_trans(x = "log")

library(visdat)
df <- detritus_wider_new_variables %>%
  mutate(detritus150_1500 = detritus150_850 + detritus850_1500,
         detritus150_1500_plus = if_else(is.na(detritus150_1500),
                                         true = detritus150_NA,
                                         false = detritus150_1500)) %>%
  select(dataset_id, visit_id, detritus150_850, detritus850_1500, detritus150_1500,detritus150_NA, detritus150_1500_plus) %>%
  filter(dataset_id %in% c("146", "56"))

df %>%
  filter(dataset_id == "56") %>%
  vis_dat()

# you can see the difference -- only some of the pitilla data have detritus in
# the 850 to 1500 data. This is actually different visits

df %>% vis_dat()
# + facet_wrap(~dataset_id)

plotly::ggplotly()

# what variables are present in Cardoso2011


detritus_wider_new_variables %>%
  filter(visit_id == 231) %>%
  keep(~ !all(is.na(.x)))





# misc for later ----------------------------------------------------------


# demo of bootstrapping a model
mi <- modelling_information %>%
  mutate(boot_src_dat = map(src_df, modelr::bootstrap, n = 10))

