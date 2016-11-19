

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
  # browser()

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
  "detritus150_20000",       "detritus0_150",    function(coarse) {exp(0.68961 * log(coarse) - 0.11363)},   c(6),
  "detritus1500_20000",   "detritus10_1500",  function(med)    {exp(0.79031 * log(med) - 0.070033)},     c(111),
  "detritus150_850",      "detritus0_150",    function(med)    { ((0.9857 *med) + 1.496)},               c(166,171,181)
)

## could easily add an "equation meant to be used on dataset" arguement, which
## would convert used_on_dataset to dataset_name, then add to ggtitle

## generate plots & add to data.frame
equation_plots <- estimating_equation_data %>%
  select(-used_on_dataset) %>%
  by_row(show_function_with_all_data %>%
           # use the most recent dataset for these calculations
           partial(df = detritus_wider_correct_frenchguiana) %>%
           # lift from using named arguements to using a list
           lift_dl %>%
           possibly(otherwise = NA_real_))


detritus_wider_correct_frenchguiana %>%
  filter(dataset_id == 6)

# plot
equation_plots %>%
  select(.out) %>%
  walk(print)


# applying functions to data ----------------------------------------------


#' filter the dataset to just the data that this particular model uses.
#'
#' Before using an equation to predict a certain detritus volume, the data
#' should be filtered to show just the data that will be used to do the
#' prediction. This simplifies things like validation, plotting, and combining
#' with the rest of the data later.
#'
#' @param used_on_dataset Which dataset will this be used on?
#' @param xvar the predictor variable used. only used for checking to make sure
#'   it exists at this site
#' @param df the dataset that this is used on (the most recent one)
#' @param ... cheating! allowing unused columns to pass through and come out the
#'   other end of by_row
filter_by_dataset_id <- function(used_on_dataset, xvar, df, ...){

  ds_id <- unlist(used_on_dataset)

  df %>%
    filter(dataset_id %in% ds_id) %>%
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


new_detritus <- detritus_estimate_function_filt %>%
  select(-used_on_dataset) %>%
  by_row(mutate_new_col %>%
           lift)

new_detritus$.out %>% map(select, 36) %>% map(head)

# what if it is a model tho -----------------------------------------------

# first, create any combinations of detritus values which are needed in later
# modelling

detritus_wider_new_variables <- detritus_wider_FG_detritus_corrected %>%
  mutate(detritus10_1500_2000_NA = detritus10_1500 + detritus1500_20000 + detritus20000_NA)


detritus_wider_new_variables %>%
  filter(!is.na(detritus10_1500_2000_NA)) %>%
  select(dataset_id) %>%
  distinct

## first row -- why not 136??

# create the table of formulae
estimating_function_data <-
  frame_data(
    ~target_dat, ~src_dat,                       ~xvar,        ~yvar,                      ~.f, ~family,
    116,         c("131", "126", "121", "221"),  "~diameter",  "~detritus10_1500_2000_NA", glm, "gaussian"
  )

## do something like
# make the formulae into actual formulae
estimating_function_data_f <- estimating_function_data %>%
  mutate(xvar = xvar %>% map(as.formula),
         yvar = yvar %>% map(as.formula))


# create a dataframe that holds everything we need to run the models:
test <- estimating_function_data_f %>%
  # select the required input rows
  mutate(src_df = map(src_dat,
                      ~ detritus_wider_new_variables %>%
                        filter(dataset_id %in% .x)),
         # create modelling function
         fml = map2(.x = xvar, .y = yvar, ~ formulae(.y, .x)),
         fml = unlist(fml))


# write a function which uses all these arguements to create a model
fit_predictive_model <- function(src_df, fml, .f, family, target_dat) {
  # mod_list = fit_with(src_df)
# browser()
  ff <- .f[[1]]
  mod <- fit_with(data = src_df[[1]], .f = ff, .formulas = fml, family = family)


  return(mod)
}

test$src_df[[1]] %>% names

test %>%
  select(-src_dat, -xvar, -yvar) %>%
  by_row(fit_predictive_model %>% lift)



         mod = map2(.x = src_df, .y = fml, ~ fit_with(data = .x, .formulas = .y, .f = glm)))

# note that fml and mod are lists, not a formula and a model respectively.
test %>%
  mutate(ff = fml %>% unlist) %>%
  select(-src_df)

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
