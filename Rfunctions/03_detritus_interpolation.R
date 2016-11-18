#' ## Predicting the French Guiana detritus _mass_ from _volume_
#'
#' first order of business is to predict the amounts of detritus based on the
#' milliteters of detritus in French Guiana. This prediction should furnish the
#' standard errors also, which will be useful if/when these values are used in
#' subsequent regressions, so that their predictions can be weighted by SE or
#' something

#' read in requisite data.
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
      predict(mod, se.fit = TRUE, .) %>%
      ## returns a list. Add in the original values:
      splice(xs = xs, .)
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
                                             ~ .x$fpom_ml %>% negate(is.na)(.) %>% all,
                                             ## if it is not all NA -- in other
                                             ## words there are some values --
                                             ## then run the prediction function
                                             ## and spit out a list that contains the ids of the prediction
                                             ~ splice(bromeliad_id = .x$bromeliad_id,
                                                      fpom_convert_ml_into_g(.x$fpom_ml))))

  ## here it could also spit out a message

}

## this function takes a data.frame that has a mix of observed data and
## prediction objects in the same list-column. this column is "heterogeneous" --
## some elements are lists, others data.frames
predict_fpom_g <- function(.detritus_wider_FG_g) {

  ## convenience function for creating a data.frame out fo the predictions and standard errors
  tidy_prediction <- function(pred_list){
    data_frame(detritus0_150_pv = pred_list$fit,
               detritus0_150_se = pred_list$se.fit)
  }

  det_pred <- .detritus_wider_FG_g %>%
    mutate(detritus0_150_just_brom = map(detritus0_150_prediction,
                                         ## everyone should have "bromeliad_id"
                                         ~ data_frame(bromeliad_id = .x$bromeliad_id)),
           detritus0_150_just_pred = map(detritus0_150_prediction,
                                         ## if you don't have prediction variables, u are not a prediction
                                         possibly(tidy_prediction, data_frame(detritus0_150_pv = NA_real_,
                                                                              detritus0_150_se = NA_real_))),
           ## stick em together (use cbind for the recycling)
           detritus0_150_df        = map2(detritus0_150_just_brom, detritus0_150_just_pred, cbind))

  ## This approach with `safely` is OK. it would be even simpler with
  ## `possibly`, but what if we need those errors someday?? they would go here.

  det_pred %>%
    unnest(detritus0_150_df)
}
##


## nest the list where there is the same dataset id and then, then, you should
## do the function wherever there is data. Thin ein a separate step, why don't
## you just move the predictions (and the SEs) over where there is NOT data?


## put the predicted values with the originals. Combine them side-by-side and
## then run simple check -- there should be no predicted detritus where there is
## also observed detritus
combine_observed_predicted_0_150_det <- function(.detritus_wider_correct_frenchguiana, .detritus_wider_fpom_g_predicted) {

# browser()
  combined_cols <- .detritus_wider_correct_frenchguiana %>%
    # rename for clarity
    rename(detritus0_150_orig = detritus0_150) %>%
    left_join(.detritus_wider_fpom_g_predicted, by = c("dataset_id", "dataset_name", "bromeliad_id")) %>%
    # there should be no value present in both columns -- this is a horrible way to do it!
    verify(rowSums(cbind(!is.na(detritus0_150_orig), !is.na(detritus0_150_pv))) < 2) %>%
    # combine the two columns -- using ifelse should now be safe if the above check passed :D
    mutate(detritus0_150 = if_else(is.na(detritus0_150_orig), detritus0_150_pv, detritus0_150_orig))

  # then check with (either) of the fpom columns
  combined_cols %>%
    check_data_source_target("fpom_ml", "detritus0_150")

  # then clean it out if it works
  combined_cols %>%
    select(-fpom_ml, -detritus0_150_orig, -detritus0_150_pv)
  ## note that we keep detritus0_150_se which is now useful for prediction etc
  ## etc, could also be used to identify predicted values and reg values.

}

## OK an implicit assumption in the above is that all sites can and should
## receive the new data -- whether it is observed, measured, or whatever. In
## other words, even bromeliads which have no predictions, or for which the
## bromeliads in question were actually measured for detritus in that range, are
## given a brand new column.
