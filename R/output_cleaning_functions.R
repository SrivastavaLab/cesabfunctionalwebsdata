
get_bromid_nonNA <- function(data_matrix, varname){

  # assertthat::assert_that(all(names(data_matrix) %in% c("bromeliad_id", varname)))

  where_exists <- !is.na(data_matrix[[varname]])

  data_matrix[['bromeliad_id']][where_exists]

}


# Filter out NAs

any_NA_or_0 <- function(vector){
  any(c(all(is.na(vector)), sum(vector, na.rm = TRUE) == 0))
}

all_NA <- function(vector){
  all(is.na(vector))
}



#' Title
#'
#' Input is either W or B matrix (or E, when all_NA)
#'
#' @param data_matrix data matrix to check for variables
#' @param response vector of responses
#' @param FUN function that is TRUE when thigs are bad -- all_NA, or all_NA_all_zero
#'
#' @return
#' @export
drop_row_col <- function(data_matrix, response, FUN){

  assertthat::assert_that(all(response %in% names(data_matrix)))

  row_all_match_FUN <- data_matrix %>%
    .[,response] %>%
    apply(1, FUN)

  ## remove rows that match the funtion
  matrix_complete_row <- data_matrix %>%
    .[!row_all_match_FUN, ]

  ## remove the columns
  col_all_match_FUN <- matrix_complete_row %>%
    .[,response] %>%
    apply(2, FUN)

  ## remove rows that match the funtion
  matrix_complete_row_col <- matrix_complete_row %>%
    .[,!col_all_match_FUN]

  return(matrix_complete_row_col)

}
