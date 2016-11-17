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



# what if it is a model tho -----------------------------------------------

detritus_wider_correct_frenchguiana %>%
  mutate(detritus0_NA =  detritus10_1500 + detritus1500_20000 + detritus20000_NA) %>%
  glm(log(detritus0_NA)~log(diameter), data=elverde90s)

estimating_function_data <-
  frame_data(
    ~target_dat, ~src_dat,                       ~eqn,                                                              ~xvar,               ~yvar,
    116,         c("131", "126", "121", "221"),  list(~ detritus10_1500 + detritus1500_20000 + detritus20000_NA),  list(~detritus0_NA),  list(~log(detritus0_NA))
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


detritus_wider_correct_frenchguiana %>% filter(dataset_id %in% c("131", "126"))

estimating_function_data$src_dat
