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
               cols_only(
                 # .default = col_character(),
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
get_response_from_model <- function(mod) {

  respname <- mod$model %>%
    attr("terms") %>%
    formula() %>%
    all.vars(.) %>%
    .[1]

  return(respname)
}


#' using broom::augment to add new values here.

add_predictions_to_data <- function(.df, .model) {

  if (TRUE) {
    warning("skipping detritus prediction")
  }else{
  respname <- get_response_from_model(.model)

  # get predictions
  # model_predictions <- prediction(data = .df, model = .model)
  model_predictions <- broom::augment(x = .model)


  # names(model_predictions_fulldata) <- paste(
  #   respname, names(model_predictions_fulldata), sep = "_")

  # export list of prediciton object -- just in case i want it later! and also
  # the colbound data.fram
  outlist <- list(model_predictions = model_predictions,
                  data_with_prediction = left_join(.df, model_predictions, by = join_by(bromeliad_id))
  )



  return(outlist)
  }

  return(.df)
}

## you just move the predictions (and the SEs) over where there is NOT data?


## put the predicted values with the originals. Combine them side-by-side and
## then run simple check -- there should be no predicted detritus where there is
## also observed detritus
combine_observed_predicted_0_150_det <- function(.detritus_wider_fpom_g_pred) {

  if(TRUE){
    warning("FPOM not inferred from a model")
    ## don't be confused by the object name. this passes the same information
    ## unchanged from one step to the next
  } else {

  outdf <- .detritus_wider_fpom_g_pred[["data_with_prediction"]]

  output <- outdf %>%
    # mutate_at(vars(ends_with("fitted")), as.numeric) %>%
    # not sure what to call this -- a combination of predictions and real values!
    mutate(detritus0_150_combo = if_else(is.na(detritus0_150), .fitted, detritus0_150),
           detritus0_150_src   = if_else(!is.na(detritus0_150),
                                         "observed",
                                         if_else(!is.na(.fitted),
                                                 "estimated",
                                                 NA_character_)))

  # TODO tests? it might make sense to keep the name of the variable the same --
  # not to track metadata in a column name like that

  output <- dplyr::select(output,-starts_with("."))

  return(output)

  }
  return(.detritus_wider_fpom_g_pred)
}

change_name_150_combo <- function(.detritus_wider_0_150_added){

  if(TRUE){
  warning("not renaming a detritus0_150_combo as missing values were never predicted")
  }else{
  .detritus_wider_0_150_added %>%
    select(-detritus0_150) %>%
    rename(detritus0_150 = detritus0_150_combo)
  }
  return(.detritus_wider_0_150_added)
}
