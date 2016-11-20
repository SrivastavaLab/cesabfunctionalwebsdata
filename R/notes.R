


newx <- runif(20, 0, 50)
new <- predict_function(mlinear)(newx)

ggplot(aes(x = fpom_ml, y = fpom_g), data = fpom_fg_data) +
  geom_point() +
  coord_cartesian(xlim = c(0, 40)) +
  stat_smooth(method = "gam") +
  geom_pointrange(aes(ymin = ymin,
                      ymax = ymax),
                  data = data_frame(fpom_ml = newx,
                                    fpom_g = new$fit,
                                    ymin = new$fit - new$se.fit,
                                    ymax = new$fit + new$se.fit), colour = "red")


## presently concerned about the fpom cpom business.
## examine them

detritus_wider_cardoso_corrected %>%
  tbl_df %>%
  select(dataset_id, visit_id, dataset_name, dplyr::contains("pom_")) %>%
  gather(lil_detritus, value, dplyr::contains("pom_")) %>%
  nest(value) %>%
  arrange(visit_id, dataset_name) %>%
  mutate(n_na = map_dbl(data, ~ sum(is.na(.x$value)))) %>%
  filter(n_na == 0)

## work with cardoso and do sinnamary later

detritus_wider_cardoso_corrected %>%
  filter(dataset_id %in% c(186, 216)) %>%
  magrittr::extract2("dataset_name")


