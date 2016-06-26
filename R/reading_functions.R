

read_csv_correct_cols <- function(filename){
  first_attempt <- readr::read_csv(filename)
  is_number <- sapply(first_attempt, function(x) any(str_detect(x, "[0123456789]"), na.rm = TRUE))
  correct_cols <- paste(c("c", "n")[is_number + 1], collapse = "")
  read_csv(filename, col_types = correct_cols)
}
