# plotting functions ------------------------------------------------------


# the goal of these functions is to compare a function to any available datasets
# with the same predictor and response variables. These functions are not from
# anywhere that I know of, so this is the closest I can think of a way to
# validate them
plot_equation_with_all_data <- function(df, xvar, yvar, est_f){
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
create_equation_table <- function() {
  frame_data(
    ~xvar,                  ~yvar,              ~est_f,                                                   ~used_on_dataset,
    "detritus150_20000",    "detritus0_150",    function(coarse) {exp(0.68961 * log(coarse) - 0.11363)},   c(6),
    "detritus1500_20000",   "detritus10_1500",  function(med)    {exp(0.79031 * log(med) - 0.070033)},     c(111),
    "detritus150_850",      "detritus0_150",    function(med)    { ((0.9857 *med) + 1.496)},               c(166,171,181)
  )
}


plot_data_with_equation_table <- function(.equation_table, .detritus_data) {
  .equation_table %>%
    select(-used_on_dataset) %>%
    by_row(plot_equation_with_all_data %>%
             # use the most recent dataset for these calculations
             partial(df = .detritus_data) %>%
             # lift from using named arguements to using a list
             lift_dl %>%
             possibly(otherwise = NA_real_))
}


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

  # unlist function(it is in a list column)
  ff <- est_f[[1]]

  mexp <- lazyeval::interp(~ ff(x), x = as.name(xvar))

  ll <-  list(mexp) %>% set_names(newname)

  filtered_data[[1]] %>%
    mutate_(.dots = ll)

}

do_filter_dataset_id <- function(.equation_table, .detritus_data) {
  .equation_table %>%
    by_row(filter_by_dataset_id %>%
             lift %>%
             possibly(NA_real_),
           df = .detritus_data,
           .to  = "filtered_data")
}

do_mutate_new_col <- function(.filtered_df_table){
  .filtered_df_table %>%
    # this only works because of possibly() above, and in reality should do
    # nothing -- why are functions being applied to absent data; there must be an
    # error
    filter(!is.na(filtered_data)) %>%
    select(-used_on_dataset) %>%
    by_row(mutate_new_col %>%
             lift)
}


# functions for creating, preparaing and modifying data relating to models -------------------


add_new_columns_for_prediction <- function(.detritus_data) {
  .detritus_data %>%
    mutate(detritus10_1500_2000_NA = detritus10_1500 + detritus1500_20000 + detritus20000_NA) %>%
    mutate(detritus_over_150       = detritus0_150   + detritus150_20000  + detritus20000_NA) %>%
    mutate(detritus850_20000_sum   = if_else(is.na(detritus850_20000), true = detritus1500_20000 + detritus850_1500, false = detritus850_20000))
}


create_model_table <- function(){
  frame_data(
    ~m_id, ~target_dat,        ~src_dat,                       ~xvar,                          ~yvar,                           ~.f, ~family,
    "m1",   "116",              c("131", "126", "121", "221"),  "~log(diameter)",             "~log(detritus10_1500_2000_NA)", glm, "gaussian",
    "m2",   c("186", "216"),    c("211"),                       "~log(detritus0_150)",        "~log(detritus150_20000)",       glm, "gaussian",
    "m3",   c("186", "216"),    c("211"),                       "~log(detritus0_150)",        "~log(detritus20000_NA)",        glm, "gaussian",
    "m4",   c("201"),           c("211"),                       "~log(detritus0_150)",        "~log(detritus_over_150)",       glm, "gaussian",
    "m5",   c("71", "51", "61"),c("56"),                        "~log(detritus850_20000_sum)","~log(detritus0_150)",           glm, "gaussian",
    "m6",   c("71", "51"),      c("61"),                        "~log(detritus850_20000_sum)","~log(detritus20000_NA)",        glm, "gaussian"
  ) %>%
    mutate(xvar = xvar %>% map(as.formula),
           yvar = yvar %>% map(as.formula))
}

derive_modelling_information <- function(.model_table, .detritus_data){
  # create a dataframe that holds everything we need to run the models:
  .model_table %>%
    # select the required input rows
    mutate(src_df = map(src_dat,
                        ~ .detritus_data %>%
                          # TODO: ? add select() to contain only some variables??
                          filter(dataset_id %in% .x)),
           # create modelling function
           fml = map2(.x = xvar, .y = yvar, ~ formulae(.y, .x)),
           fml = flatten(fml)) %>%
    mutate(x_symb = xvar %>% map(find_symbols),
           y_symb = yvar %>% map(find_symbols),
           x_funs = x_symb %>% map_chr("functions"),
           x_vars = x_symb %>% map_chr("variables"),
           y_funs = y_symb %>% map_chr("functions"),
           y_vars = y_symb %>% map_chr("variables"))
}


# write a function which uses all these arguements to create a model
fit_predictive_model <- function(m_id, src_df, fml, .f, family, target_dat) {
  # mod_list = fit_with(src_df)
  # browser()
  ff <- .f[[1]]

  fit_dat <- partial(fit_with, .f = ff, .formulas = fml, family = family)

  mod <- map(src_df, fit_dat)


  return(mod)
}

do_fit_predictive_model <- function(.modelling_information){
  .modelling_information %>%
    # selet the functions's arguements
    select(m_id, src_df, fml, .f, family, target_dat) %>%
    by_row(fit_predictive_model %>% lift,
           .to = "predicting_model") %>%
    mutate(predicting_model = predicting_model %>% flatten)
}


make_prediction_xs <- function(m_id, src_df, x_vars) {
  dd <- src_df[[1]]

  dd %>%
    .[[x_vars]] %>%
    seq_range(n = 30) %>%
    list(.) %>%
    set_names(x_vars) %>%
    as.data.frame
}



make_prediction_df <- function(m_id, incoming_data, predicting_model, y_vars, y_funs){

  # browser()
  dd <- incoming_data[[1]]

  y_vars <- paste0(y_vars, "_pred")

  predict_dat <- partial(add_predictions, data = dd, var = y_vars)

  # back transformation function for y axis (x not necessary because it is in the function?)
  back_trans <- switch(y_funs, log = exp, I)

  modlist <- predicting_model[[1]] # just because it is a list-column, get the first element (which is a list lol)
  out <- map_df(modlist, predict_dat)

  out[[y_vars]] <- back_trans(out[[y_vars]])

  return(out)
}



construct_plotting_information <- function(.observed_model_fit, .modelling_information) {
  .observed_model_fit %>%
    select(m_id, src_df, predicting_model) %>%
    left_join(.modelling_information %>%
                select(m_id, target_dat, x_funs, y_funs, x_vars, y_vars),
              by = "m_id")
}


plot_fn <- function(src_df, x_funs, y_funs, x_vars, y_vars, curve_data,...){
  dd <- src_df[[1]]

  # give "identity" if x_funs or y_funs is ""
  switch_trans <- function(x) switch(x, "log" = "log", "identity")
  # get correct axis transformations
  xt <- switch_trans(x_funs)
  yt <- switch_trans(y_funs)

  # browser()

  # rename variables for plotting
  plotdat <- dd %>%
    select_(xs = x_vars,
            ys = y_vars)

  # tweak the name - predicted values have "pred" on the side
  y_vars <- paste0(y_vars, "_pred")

  ld <- curve_data[[1]] %>%
    select_(xs = x_vars,
            ys = y_vars)

  plotdat %>%
    ggplot(aes(x = xs, y = ys)) +
    geom_point() +
    geom_line(data = ld) +
    labs(x = x_vars, y = y_vars) +
    coord_trans(x = xt, y = yt)

}
# TODO perhaps a color map to show the site(s)??



# plotting models ---------------------------------------------------------

plot_model_and_supporting_data <- function(.plotting_information, .modelling_information) {

  # create the x range over which all the models should be predicted.
  data_for_drawing_line <- .plotting_information %>%
    select(m_id, src_df, x_vars) %>%
    by_row(make_prediction_xs %>% lift, .to = "xs_range") %>%
    select(-src_df, -x_vars)

  # join back to original and run another function -- this time, to add predictions
  plotting_info_pred <- .plotting_information %>%
    left_join(data_for_drawing_line, by = "m_id") %>%
    select(m_id, incoming_data = xs_range, predicting_model, y_vars, y_funs) %>%
    # mutate(predicting_model = flatten(predicting_model)) %>%
    by_row(make_prediction_df %>% lift, .to = "curve_data")

  # TODO another pipeline here, that takes a list of models (derived from
  # bootstraps) which will be part of plotting_information. Then applies this in
  # a process similar to the above, then generates a list of predictions. Then
  # applies quantiles to this list.

  plots <- plotting_info_pred %>%
    left_join(.modelling_information %>% select(m_id, src_df, x_funs, x_vars)) %>%
    by_row(plot_fn %>% lift, .to = "model_fit_plot")


}



# Coup de Grace: use the model fit to add the missing values to observed data from another site
estimate_missing_detritus_new_site <- function(.observed_model_fit, .modelling_information,
                                               .detritus_data){
  # extract the fit data that we need
  fit_data_needed <- .observed_model_fit %>%
    select(m_id, predicting_model, target_dat)

  # there are some "model-level" info that we also need -- specifically info
  # about the y variable and transformations if any
  model_info <- .modelling_information %>%
    select(m_id, y_vars, y_funs)

  # join these, add filtered dataset to model
  fit_data_needed %>%
    left_join(model_info) %>%
    mutate(target_df = target_dat %>%
             map(~ .detritus_data %>%
                   # TODO: ? add select() to contain only some variables??
                   filter(dataset_id %in% .x))
    ) %>%
    # get the precise variables we need to add predictions
    select(m_id, incoming_data = target_df, predicting_model, y_vars, y_funs) %>%
    by_row(make_prediction_df %>% lift, .to = "pred_data")

}
