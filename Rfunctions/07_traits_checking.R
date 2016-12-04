
### output -- a matrix with traits named in the proper way (XXN, where X is a
### letter and N a number) and in which all traits are unique

rename_check_traits <- function(.trts_all_filtered){
  ## in order to find out where the traits between the different columsn are
  ## similar, we spread the two categories ("traits" and "tachet").We then stick
  ## them together.
  traits_all_combined <- .trts_all_filtered %>%
    gather(trtname, value, starts_with("tachet"), starts_with("traits")) %>%
    separate(trtname, into = c("tach_trait", "traitmode")) %>%
    spread(tach_trait, value) %>%
    replace_na(list(tachet = "", traits = "")) %>%
    mutate(combotrait = paste0(tachet, traits))

  ## check for problems -- if there is more than one character in the traits (all
  ## shold be single digits) then this indicates that the two tables had different
  ## values
  traits_all_combined %>%
    filter(nchar(combotrait) > 1) %>%
    assertr::verify(nrow(.) == 0)

  ## if there are no problems -- convert the combined trait column to numeric,
  ## delete all, make traits columns.
  traits_all_combined %>%
    mutate(combotrait2 = as.numeric(combotrait)) %>%
    select(-tachet, -traits, -combotrait) %>%
    spread(traitmode, combotrait2) %>%
    assertr::verify(nrow(.) == nrow(rename_check_traits))
}
