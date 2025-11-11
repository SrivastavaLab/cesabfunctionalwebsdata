check_brom_data <- function(d) {
  return(d)
  # assertr::assert(d,
  #        assertr::within_bounds(lower.bound = 0,
  #                      upper.bound = 150))
}


read_size_aquilega <- function(filenm) {
  d <- filenm %>%
    read_delim(delim = ";") %>%
    rename(plant_id = ID) %>%
    mutate(plant_id = as.character(plant_id))

  # browser()

  check_brom_data(d)

  return(d)
}

read_size_Guzmania_mertensii <- function(filenm){
  filenm %>%
    read_delim(delim = ";") %>%
    rename(plant_id = plante) %>%
    mutate(plant_id = as.character(plant_id)) %>%
    check_brom_data
}

read_size_vriesea <- function(filenm){
  filenm %>%
    read_delim(delim = ";") %>%
    rename(plant_id = BromeliadID) %>%
    mutate(plant_id = as.character(plant_id)) %>%
    check_brom_data
}

combine_check_supp_size <- function(.aquilega_biog, .aquilegaKT, .guzmania, .mertensii, .vriesea, .vriesea_prod){

}

create_all_size_data <- function(.size_data) {
  .size_data %>%
    # the first column is called many things
    map(~ set_names(.x, c("plant_id", names(.x)[-1]))) %>%
    map(~ mutate(.x, plant_id = as.character(plant_id))) %>%
    bind_rows(.id = "filename") %>%
    mutate(filename = filename %>% str_replace("size_", "") %>% str_replace("\\.csv", ""))
}

# this could add more info at the level of the file: who collected it, etc.
more_information <- function() {
  tribble(
    ~ filename,       ~species,              # collected_year
    "aquilega_biog",  "Aechmaea_aquilega",
    "aquilegaKT",     "Aechmaea_aquilega",
    "guzmania",    "Guzmania_sp",
    "mertensii",      "Aechmaea_mertensii",
    "vriesea_prod",   "Vrisea_splendens",
    "vriesea",        "Vrisea_splendens"
)
}

#  combine data

add_more_info_to_supp <- function(.all_size_data){
  .all_size_data %>%
    left_join(more_information())
}

#   write_csv("data-intermediate/size_all_data.csv")

