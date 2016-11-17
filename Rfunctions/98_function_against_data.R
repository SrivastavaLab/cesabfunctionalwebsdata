## what sites?


show_function_with_all_data <- function(df, xvar, yvar, est_f){
  requireNamespace("assertthat")
  # browser()
  xname <- as.character(xvar)
  yname <- as.character(yvar)

  assert_that(has_name(detritus_wider_correct_frenchguiana, xname))
  assert_that(has_name(detritus_wider_correct_frenchguiana, yname))


  dat <- df %>%
    select_(xs = xname, ys = yname, "dataset_name") %>%
    filter(complete.cases(.))

  assert_that(nrow(dat) > 0)


  curve_dat <- data_frame(xs     = seq_range(dat$xs, n = 40, expand = FALSE),
                          ys     = est_f(xs))

  dat %>%
    ggplot(aes(x = xs, y = ys, colour = dataset_name)) +
    geom_point() +
    geom_line(aes(colour = NULL), data = curve_dat) +
    viridis::scale_color_viridis(discrete = TRUE) +
    coord_trans("log", "log") +
    xlab(xname) +
    ylab(yname)
}

estimating_equation_data <- frame_data(
  ~xvar,                  ~yvar,              ~est_f,                                                   ~used_on_dataset,
  "detritus150_20000",    "detritus0_150",    function(coarse) {exp(0.68961 * log(coarse) - 0.11363)},   c(6),
  "detritus1500_20000",   "detritus10_1500",  function(med)    {exp(0.79031 * log(med) - 0.070033)},     c(111)
)


estimating_equation_data %>%
  select(-used_on_dataset) %>%
  pmap(show_function_with_all_data, df = detritus_wider_correct_frenchguiana)

