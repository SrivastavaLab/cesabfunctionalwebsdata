
fiter_test <- function(x){
  x %>%
    filter(!grepl("^Test", name))
}


no_attrib_unnest_det <- function(.broms){
  names(.broms) <- str_replace(names(.broms), "attributes\\.", "")

  ### the detritus should be unnested (for raw. summarize later)


  .broms %>%
    ## supply a default data.frame
    mutate(detritus = map_if(detritus,
                             is_null,
                             ~ data_frame(min = NA,
                                          max = NA,
                                          mass = NA))) %>%
    unnest(detritus)
}

combine_multi_names <- function(.trts_all){
  ### names are a nested list. Convert them to a vector of length one.
  is_long <- function(x) length(x) > 1

  .trts_all %>%
    mutate(names = names %>%
             map_if(is_null, ~"") %>%
             map_if(is_long, paste, collapse = ";")) %>%
    unnest(names)

}
