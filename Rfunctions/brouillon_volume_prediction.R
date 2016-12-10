remake::dump_environment()

prediction_raw_material <- supp_size_model_fits %>%
  mutate(target_df = map (target_dat, ~ bromeliad_detritus %>% filter(visit_id %in% .x)))


response_function_info <- supp_size_model_info %>%
  select(m_id, y_symb) %>%
  mutate(y_funs = map_chr(y_symb, "functions"),
         y_vars = map_chr(y_symb, "variables"))

output <- prediction_raw_material %>%
  left_join(response_function_info) %>%
  # get the precise variables we need to add predictions
  select(m_id, incoming_data = target_df, predicting_model, y_vars, y_funs) %>%
  by_row(make_prediction_df %>% lift, .to = "pred_data")

head(output$pred_data[[1]])

# omg the estimated max_water is impossibly small, smaller than the observed data.
