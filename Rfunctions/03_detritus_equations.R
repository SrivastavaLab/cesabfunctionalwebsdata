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
    select(xs = xvar, ys = yvar, matches("dataset_name")) %>%
    filter(complete.cases(.))
  # browser()

  assert_that(nrow(dat) > 0)

  # extract the function: TODO make sure it is length 1 before doing this
  f <- est_f[[1]]

  curve_dat <- tibble(xs     = seq_range(dat$xs, n = 40, expand = FALSE),
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
  tribble(
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

  ds_id <- used_on_dataset

  df %>%
    filter(dataset_id %in% ds_id)# %>%
    # check to make sure predictors actually exist
    # assert_(not_na, xvar) %>%
    # verify(all(!is.na(xvar))) |>
    # and that dataset is not 0 from the filter above
    # verify(nrow(.) > 0)
  message("data NOT checked before modelling")
}

# make a new column to hold the
mutate_new_col <- function(xvar, yvar, est_f, filtered_data){
  # predicted flag as column or different header?
  # test for all(is.na()) in target
  # newname <- paste0(yvar, "_function")

  assertthat::assert_that(length(est_f) == 1)

  # unlist function(it is in a list column)
  # ff <- est_f

  # mexp <- lazyeval::interp(~ ff(x), x = as.name(xvar))

  # ll <-  list(mexp) %>% set_names(newname)

  filtered_data %>%
    mutate("{yvar}_function" := est_f({{xvar}})
      )

}

do_filter_dataset_id <- function(.equation_table, .detritus_data) {
  .equation_table %>%
    rowwise() |>
    mutate(filtered_data =
             list(
               .detritus_data |>
                 filter(dataset_id %in% used_on_dataset)
             )
           )
  # by_row(filter_by_dataset_id %>%
    #          lift %>%
    #          possibly(NA_real_),
    #        df = .detritus_data,
    #        .to  = "filtered_data")
}

do_mutate_new_col <- function(.filtered_df_table){
  .filtered_df_table %>%
    # this only works because of possibly() above, and in reality should do
    # nothing -- why are functions being applied to absent data; there must be an
    # error
    # filter(!is.na(filtered_data)) %>%
    select(-used_on_dataset) %>%
    mutate(equation_applied = list(
      mutate_new_col(xvar = xvar,
                     yvar = yvar,
                     est_f = est_f,
                     filtered_data = filtered_data)
    )
    )
}


# functions for creating, preparaing and modifying data relating to models -------------------


add_new_columns_for_prediction <- function(.detritus_data) {

  message("changing detritus20000_NA column type to double")
  .detritus_data$detritus20000_NA <- readr::parse_double(.detritus_data$detritus20000_NA)

  .detritus_data %>%
    mutate(detritus10_1500_2000_NA = detritus10_1500 + detritus1500_20000 + detritus20000_NA) %>%
    mutate(detritus_over_150       = detritus0_150   + detritus150_20000  + detritus20000_NA) %>%
    mutate(detritus850_20000_sum   = if_else(is.na(detritus850_20000),
                                             true = detritus1500_20000 + detritus850_1500,
                                             false = detritus850_20000)) %>%
    mutate(detritus0_NA_sum = detritus0_150 + detritus150_850 + detritus850_20000_sum + detritus20000_NA) %>%
    mutate(detritus150_NA_sum = detritus150_850 + detritus850_20000_sum + detritus20000_NA) %>%
    mutate(detritus0_20000_sum = detritus0_150 + detritus150_850 + detritus850_20000_sum,
           detritus20000_NA_na0 = if_else(detritus20000_NA < 0.001, true = NA_real_, false = detritus20000_NA)) %>%
    mutate(detritus150_1500 = detritus150_850 + detritus850_1500,
           detritus150_1500_plus = if_else(is.na(detritus150_1500),
                                           true = detritus150_NA,
                                           false = detritus150_1500)) %>%
    mutate(detritus150_NA_sum_Japi = detritus150_850 + detritus850_1500 + detritus1500_20000 + detritus20000_NA,
           detritus150_NA_sum_Japi = if_else(is.na(detritus150_NA_sum_Japi),
                                             true = detritus125_NA,
                                             false = detritus150_NA_sum_Japi)
)
}


create_model_table <- function(){
  tribble(
    ~m_id, ~target_dat,        ~src_dat,                       ~xvar,                            ~yvar,                           ~.f, ~family,
    "m01",   "116",              c("131", "126", "121", "221"),  "~log(diameter)",               "~log(detritus10_1500_2000_NA)", glm, "gaussian",
    "m02",   c("186", "216"),    c("211"),                       "~log(detritus0_150)",    "~log(detritus150_20000)",       glm, "gaussian",
    "m03",   c("186", "216"),    c("211"),                       "~log(detritus0_150)",    "~log(detritus20000_NA)",        glm, "gaussian",
    "m04",   c("201"),           c("211"),                       "~log(detritus0_150)",    "~log(detritus_over_150)",       glm, "gaussian",
    "m05",   c("71", "51", "61"),c("56"),                        "~log(detritus850_20000_sum)",  "~log(detritus0_150)",     glm, "gaussian",
    "m06",   c("71", "51"),      c("61"),                        "~log(detritus850_20000_sum)",  "~log(detritus20000_NA)",        glm, "gaussian",
    "m07",   c("66"),            c("56"),                        "~diameter",                    "~detritus0_NA_sum",             glm, "gaussian",
    "m08",   c("76", "81", "91"),c("56"),                        "~detritus150_NA_sum",          "~detritus0_150",          glm, "gaussian",
    "m09",   c("86"),            c("91"),                        "~num_leaf",                    "~detritus150_NA",               glm, "gaussian",   # too weak to use??
    "m10",   c("101", "106"),    c("56"),                        "~log(detritus0_20000_sum)",    "~log(detritus20000_NA_na0)",    glm, "gaussian",   # can't get coefficients or r2 to match Diane's notes
    "m11",   c("146"),           c("56"),                        "~log(detritus150_1500_plus)",  "~log(detritus0_150)",           glm, "gaussian",
    "m12",   c("161"),           c("56"),                        "~log(detritus150_NA_sum_Japi)","~log(detritus0_150)",           glm, "gaussian"
  ) %>%
    rowwise() |>
    mutate(
      fml_string = paste0(
        stringr::str_replace(string = yvar, pattern = "~", replacement=""),
        # " ~ ",
        xvar),
      xvar = list(as.formula(xvar)),
      yvar = list(as.formula(yvar)),
      fml = list(as.formula(fml_string))
    )
}

derive_modelling_information <- function(.model_table, .detritus_data){
  # create a dataframe that holds everything we need to run the models:
# browser()
  model_info <- .model_table %>%
    rowwise() |>
    # select the required input rows
    mutate(
      src_df = list(
        .detritus_data %>%
          # TODO: ? add select() to contain only some variables??
          filter(dataset_id %in% src_dat)
        )
    ) |>
    # create modelling function
    # fml = map2(.x = xvar, .y = yvar, ~ formulae(.y, .x)),
    # fml = flatten(fml)) %>%
    mutate(x_symb = list(find_symbols(xvar)),
           y_symb = list(find_symbols(yvar)),
           x_funs = list(x_symb$functions),# %>% map_if(is_empty, ~ "") %>% flatten_chr(),
           x_vars = x_symb$variables, # %>% map_chr("variables"),
           y_funs = list(y_symb$functions),# %>% map("functions") %>% map_if(is_empty, ~ "") %>% flatten_chr(),
           y_vars = y_symb$variables# %>% map_chr("variables")
           )
# needs to be rewritten to work with rowwise .. but is it necessary? what is this for!

  # make sure that there are actually some data to use to fit the model!
  nrows_src_df <- map_dbl(model_info$src_df, nrow)
  if(any(nrows_src_df == 0)) stop("there are 0 rows in some input data for the imputation models")

  return(model_info)
}


## NOT useful anymore!
# write a function which uses all these arguements to create a model
fit_predictive_model <- function(m_id, src_df, fml, .f, family, target_dat) {
  # mod_list = fit_with(src_df)
  # browser()
  ff <- .f#[[1]]

  fit_dat <- partial(fit_with, .f = ff, .formulas = fml, family = family)

  mod <- map(src_df, fit_dat)


  return(mod)
}

do_fit_predictive_model <- function(.modelling_information){
  .modelling_information %>%
    # selet the functions's arguements
    select(m_id, src_df, fml_string, family, target_dat) |>  #%>%
    # by_row(fit_predictive_model %>% lift,
    #        .to = "predicting_model") %>%
    mutate(
      safe_model = list(
        # perform the GLMs but do it "safely" this still produces a result even if it errors
        # errors are caused by data being gone when i think it should be.
        purrr:::safely(glm)(as.formula(fml_string), family = family, data = src_df)
      ),
      predicting_model = if_else(is.null(safe_model$result), list(NA), list(safe_model$result) )
    )
}


#' make the predictor range (and name it appropriately)
#'
#' given a data.frame and a variable name, find that variable name and use
#' seq_range to make 30 points in its range
#'
#' @param m_id model.id, for housekeeping
#' @param src_df source data.frame
#' @param x_vars x variable name
#'
#' @return a data.frame with one column only (the new sequence)
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

  dd <- incoming_data[[1]]

  # type response means that we will get back the answer on the scale of the
  # response (which might have been transformed!!)
  # NOTE PLEASE: that `prediction()` gives these **BY DEFAULT**. see ?prediction

  modlist <- predicting_model[[1]] # just because it is a list-column, get the first element (which is a list lol)
  # note that this list will normall be length 1, but I'm doing it this way just
  # in case multiple models are ever fit to the data
  out <- map_df(modlist, ~ prediction(.x, data = dd))

  names(out) <- paste0(y_vars, "_", names(out))

  # back transformation function for y axis (x not necessary because it is in the function?) >> that's correct
  back_trans <- switch(y_funs, log = exp, I)
  # REASONABLY CERTAIN this is correct since iirc SEs are in the same units as
  # the values they are associate with
  out <- back_trans(out)

  obs_pred <- bind_cols(dd, out)

  return(obs_pred)
}



construct_plotting_information <- function(.observed_model_fit, .modelling_information) {
  .observed_model_fit %>%
    # select(-.f) %>%
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
    select(xs = matches(x_vars),
            ys = matches(y_vars))

  # tweak the name - predicted values have "_fitted" on the side
  y_vars <- paste0(y_vars, "_fitted")

  ld <- curve_data[[1]] %>%
    select(xs = matches(x_vars),
            ys = matches(y_vars))

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

  # browser()
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
estimate_missing_detritus_new_site <- function(.observed_model_fit,
                                               .modelling_information,
                                               .detritus_data){
  # extract the fit data that we need
  fit_data_needed <- .observed_model_fit %>%
    select(m_id, predicting_model, target_dat)

  # there are some "model-level" info that we also need -- specifically info
  # about the y variable and transformations if any
  model_info <- .modelling_information %>%
    select(m_id, y_vars, y_funs)
# browser()
  # join these, add filtered dataset to model
  prediction_raw_material <- fit_data_needed %>%
    left_join(model_info) %>%
    mutate(target_df = list(
      .detritus_data %>%
        # TODO: ? add select() to contain only some variables??
        filter(dataset_id %in% target_dat)
    )
    )
# browser()

  name_new_col <- function(nm, val){
    d <- data.frame(x = val)
    names(d) <- paste0(nm, "_fitted")
    d
  }


  output <- prediction_raw_material %>%
    # get the precise variables we need to add predictions
    select(m_id, incoming_data = target_df, predicting_model, y_vars, y_funs) %>%
    mutate(
      pred_raw = list(
        predict(
          predicting_model,
          newdata = incoming_data)
      ),
      # back transform where necessary
      # using the name "fitted" for these predicted values because
      # that is what is expected by the next functions
      fitted = list(
        if (length(y_funs) > 0 && y_funs == "log") {
          exp(pred_raw)
        } else  {
          pred_raw
        }),
      fit_df = list(name_new_col(y_vars, fitted)),
      pred_data = list(bind_cols(incoming_data, fit_df))
    )

  # not using this part anymore,
  # mutate(make_prediction_df %>% lift, .to = "pred_data")

  return(output)

}
