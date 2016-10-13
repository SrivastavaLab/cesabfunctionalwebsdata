#' ## Predicting the French Guiana detritus _mass_ from _volume_
#'
#' first order of business is to predict the amounts of detritus based on the
#' milliteters of detritus in French Guiana. This prediction should furnish the
#' standard errors also, which will be useful if/when these values are used in
#' subsequent regressions, so that their predictions can be weighted by SE or
#' something

read_fpom_fg <- function(path) {
  path %>%
    read_csv(col_types =
               cols(
                 .default = col_character(),
                 `FPOM (ml decanted)` = col_double(),
                 `FPOM (mm3)` = col_integer(),
                 `dry weight (g)` = col_double(),
                 `dry weight (mg)` = col_integer()
               )) %>%
    select(-starts_with("X"))
}
