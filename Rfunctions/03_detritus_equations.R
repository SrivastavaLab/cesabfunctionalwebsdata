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


plot_data_with_equation_table <- function(.equation_table) {
  .equation_table %>%
    select(-used_on_dataset) %>%
    by_row(plot_function_with_all_data %>%
             # use the most recent dataset for these calculations
             partial(df = detritus_wider_correct_frenchguiana) %>%
             # lift from using named arguements to using a list
             lift_dl %>%
             possibly(otherwise = NA_real_))
}
