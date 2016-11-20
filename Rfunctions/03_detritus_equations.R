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
    by_row(plot_function_with_all_data %>%
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
    mutate(detritus10_1500_2000_NA = detritus10_1500 + detritus1500_20000 + detritus20000_NA)
}


create_model_table <- function(){
  frame_data(
    ~m_id, ~target_dat, ~src_dat,                       ~xvar,            ~yvar,                           ~.f, ~family,
    "m1",   116,         c("131", "126", "121", "221"),  "~log(diameter)", "~log(detritus10_1500_2000_NA)", glm, "gaussian"
  ) %>%
    mutate(xvar = xvar %>% map(as.formula),
           yvar = yvar %>% map(as.formula))
}
