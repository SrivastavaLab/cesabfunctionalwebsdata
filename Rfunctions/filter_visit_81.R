# visit 81 is not a complete sample so remove it from the data release.


# step zero filter visits

filter_visit_81 <- function(.visits_date) {
  .visits_date %>%
    filter(visit_id != "81")
}


filter_bromeliads_visit81 <- function(.bromeliad_correctnames, .visit_no_81) {
  semi_join(.bromeliad_correctnames,
            .visit_no_81,
            by = c("visit_id"))
}


filter_abundance_81 <- function(.abundance_no_zero, .bromeliads_visit_no_81){

  .abundance_no_zero %>%
    semi_join(.bromeliads_visit_no_81, by = c("dataset_id", "bromeliad_id"))

}
