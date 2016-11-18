

# plotting functions ------------------------------------------------------


# the goal of these functions is to compare a function to any available datasets
# with the same predictor and response variables. These functions are not from
# anywhere that I know of, so this is the closest I can think of a way to
# validate them
show_function_with_all_data <- function(df, xvar, yvar, est_f){
  requireNamespace("assertthat")

  assert_that(has_name(detritus_wider_correct_frenchguiana, xvar))
  assert_that(has_name(detritus_wider_correct_frenchguiana, yvar))


  dat <- df %>%
    select_(xs = xvar, ys = yvar, "dataset_name") %>%
    filter(complete.cases(.))

  assert_that(nrow(dat) > 0)

  # extract the function: TODO make sure it is length 1 before doing this
  f <- est_f[[1]]

  curve_dat <- data_frame(xs     = seq_range(dat$xs, n = 40, expand = FALSE),
                          ys     = f(xs))

  dat %>%
    ggplot(aes(x = xs, y = ys, colour = dataset_name)) +
    geom_point() +
    geom_line(aes(colour = NULL), data = curve_dat) +
    viridis::scale_color_viridis(discrete = TRUE) +
    coord_trans("log", "log") +
    xlab(xvar) +
    ylab(yvar)
}

## make the setup data
estimating_equation_data <- frame_data(
  ~xvar,                  ~yvar,              ~est_f,                                                   ~used_on_dataset,
  "detritus150_NA",    "detritus0_150",    function(coarse) {exp(0.68961 * log(coarse) - 0.11363)},   c(6),
  "detritus1500_20000",   "detritus10_1500",  function(med)    {exp(0.79031 * log(med) - 0.070033)},     c(111)
)

## generate plots & add to data.frame
equation_plots <- estimating_equation_data %>%
  select(-used_on_dataset) %>%
  by_row(show_function_with_all_data %>%
           # use the most recent dataset for these calculations
           partial(df = detritus_wider_correct_frenchguiana) %>%
           # lift from using named arguements to using a
           lift_dl)

# plot
equation_plots %>%
  select(.out) %>%
  walk(print)


# applying functions to data ----------------------------------------------

filter_by_dataset_id <- function(used_on_dataset, xvar, df, ...){
  df %>%
    filter(dataset_id %in% used_on_dataset) %>%
    # check to make sure predictors actually exist
    assert_(not_na, xvar) %>%
    # and that dataset is not 0 from the filter above
    verify(nrow(.) > 0)
}

mutate_new_col <- function(xvar, yvar, est_f, filtered_data){
  # predicted flag as column or different header?
  # test for all(is.na()) in target
  newname <- paste0(yvar, "_function")

  assert_that(length(est_f) == 1)
  ff <- est_f[[1]]

  mexp <- lazyeval::interp(~ ff(x), x = as.name(xvar))

  ll <-  list(mexp) %>% set_names(newname)

  filtered_data[[1]] %>%
    mutate_(.dots = ll)

}


detritus_estimate_function_filt <- estimating_equation_data %>%
  by_row(filter_by_dataset_id %>% lift,
         df = detritus_wider_correct_frenchguiana,
         .to  = "filtered_data")


detritus_estimate_function_filt %>%
  select(-used_on_dataset) %>%
  by_row(mutate_new_col %>%
           lift)


# what if it is a model tho -----------------------------------------------

estimating_function_data <-
  frame_data(
    ~target_dat, ~src_dat,                       ~eqn,                                                              ~xvar,               ~yvar,
    116,         c("131", "126", "121", "221"),  list(~ detritus10_1500 + detritus1500_20000 + detritus20000_NA),  list(~diameter),  list(~log(detritus0_NA))
  )

test <- estimating_function_data %>%
  # select the required input rows
  mutate(src_df = map(src_dat, ~ detritus_wider_correct_frenchguiana %>%
                        filter(dataset_id %in% .x)),
         # create a "total" column, if any, using the source data and the equation
         src_newv = map2(src_df, eqn,  ~ mutate_(.x, "detritus0_NA" = .y[[1]])),
         # create modelling function
         fml = map2(.x = xvar, .y = yvar, ~ formulae(.y[[1]], .x[[1]])),
         mod = map2(.x = src_newv, .y = fml, ~ fit_with(data = .x, .formulas = .y, .f = glm))
  )
# note that fml and mod are lists, not a formula and a model respectively.


## for validating
test %>%
  select(src_newv, fml, mod)

test$src_newv[[1]] %>%
  add_predictions(model = test$mod[[1]][[1]], var = "detritus0_NA_pred") %>%
  mutate(detritus0_NA_pred = exp(detritus0_NA_pred)) %>%
  ggplot(aes(x = diameter, y = detritus0_NA)) +
  geom_point() +
  geom_line(aes(y = detritus0_NA_pred)) +
  coord_trans(y = "log")

rmse(test$mod[[1]][[1]], test$src_newv[[1]])
rsquare(test$mod[[1]][[1]], test$src_newv[[1]])
mae(test$mod[[1]][[1]], test$src_newv[[1]])

list(rmse, rsquare, mae) %>%
  invoke_map(model = test$mod[[1]][[1]], data = test$src_newv[[1]])

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
