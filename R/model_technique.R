remake::dump_environment()

detritus_data <- detritus_wider_cardoso_corrected %>%
  select(visit_id, dataset_id, bromeliad_id, starts_with("detritus")) %>%
  gather(detritus_category, detritus_amount, starts_with("detritus")) %>%
  nest(bromeliad_id, detritus_amount)

existing_data <- detritus_data %>%
  mutate(n_values = map_dbl(data, ~ sum(!is.na(.x$detritus_amount)))) %>%
  filter(n_values > 0) %>%
  select(-n_values)

data_renamed <- existing_data %>%
  filter(dataset_id %in% 111) %>%
  split(., .$detritus_category) %>%
  map_at("detritus1500_20000", ~ .x %>% rename(x_variable = detritus_category)) %>%
  map_at("detritus20000_NA",   ~ .x %>% rename(y_variable = detritus_category))

detritus_models <- left_join(data_renamed[["detritus1500_20000"]],
          data_renamed[["detritus20000_NA"]],
          by = c("dataset_id", "visit_id")) %>%
  mutate(m = map2(data.x, data.y,
                  ~ left_join(.x, .y, by = "bromeliad_id") %>%
                    lm(detritus_amount.y ~ detritus_amount.x, data = .)))

detritus_models %>%
  select(visit_id, dataset_id, x_variable, y_variable, m) %>%
  mutate(g = map(m, glance)) %>%
  unnest(g) %>%
  glimpse()
