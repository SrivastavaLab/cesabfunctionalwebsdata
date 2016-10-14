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
    select(-starts_with("X")) %>%
    rename(fpom_ml = `FPOM (ml decanted)`,
           fpom_g = `dry weight (g)`) %>%
    filter(!is.na(fpom_ml))
}

#' Fit a model to predict the mass (in ***GRAMS*** is that correct) from the
#' volumen data. Right now we are using the very same form of the model that was
#' present in the original excel file from Regis et al. note that stat_smooth
#' shows a linear relationship.
fit_fpom_g_ml <- function(fpom_fg_data) {
  lm(fpom_g ~ I(fpom_ml^2) + fpom_ml, data = fpom_fg_data)

}


#' convenience function. This takes a model object and returns a function, one
#' that takes a vector of new x values and returns a list of predictions
predict_function <- function(mod) {

  respname <- mod$model %>%
    attr("terms") %>%
    formula() %>%
    all.vars(.) %>%
    .[2]

  function(xs) {
    force(respname)
    force(mod)

    xs %>%
      list %>%
      set_names(respname) %>%
      predict(mod, se.fit = TRUE, .)
  }
}


#' function to update the dataset with the new and correct fpom values for
#' French Guiana. This uses the convenience function `predict_function()` to
#' create the workhorse function that does the actual work

estimate_fpom_g_from_ml <- function(.detritus_wider_cardoso_corrected, .model_fpom_g_ml) {

  ## create function to estimate g from ml
  fpom_convert_ml_into_g <- predict_function(.model_fpom_g_ml)

  .detritus_wider_cardoso_corrected %>%
    tbl_df %>%
    select(dataset_id, bromeliad_id, dataset_name, fpom_ml) %>%
    nest(bromeliad_id, fpom_ml) %>%
    ## should there perhaps be a check that i'm not writing over data?
    mutate(detritus0_150_prediction = map_if(data,
                                             ## UGH is there a better way to say "wherever that column is not all NA"
                                             ~ .x$fpom_ml %>% is.na %>% all %>% `!`(.),
                                             ~ fpom_convert_ml_into_g(.x$fpom_ml)))

  ## here it could also spit out a message

}


predict_fpom_g <- function(.detritus_wider_FG_g) {
  .detritus_wider_FG_g %>%
    mutate(detritus0_150_pred = map_if(detritus0_150_prediction,
                                       negate(is.data.frame),
                                       ~ data_frame(detritus0_150_pv = .x$fit,
                                                    detritus0_150_se = .x$se.fit))) %>%
    unnest(detritus0_150_pred)

}
##


## nest the list where there is the same dataset id and then, then, you should
## do the function wherever there is data. Thin ein a separate step, why don't
## you just move the predictions (and the SEs) over where there is NOT data?


## put the predicted values with the originals. Combine them side-by-side and
## then run simple check -- there should be no predicted detritus where there is
## also observed detritus
