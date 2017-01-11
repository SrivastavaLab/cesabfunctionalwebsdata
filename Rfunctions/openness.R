
add_in_extra_columns <- function(.detritus_wide, .detritus_wider, .bromeliad_detritus){
  optional_cols <- reduce(list(names(.detritus_wide),
                               names(.detritus_wider),
                               names(.bromeliad_detritus)), setdiff)


  bromeliad_optional_info <- distinct(.detritus_wide[c("bromeliad_id", optional_cols)])
  # TODO check these rows

  bromeliad_detritus_opts <- left_join(.bromeliad_detritus, bromeliad_optional_info)

  return(bromeliad_detritus_opts)
}

# investigating weird spot changes ----------------------------------------

# there is one value where the input was entered with a number that was too high
# .. correct this in a way that is not sensitive to the position of this
# incorrect value bromeliad_wide$incident_radiation_percentage[517]<-14.59

# bromeliad_detritus_opts$incident_radiation_percentage %>% max(na.rm = TRUE)

correct_incident_rad_percent <- function(.bromeliad_detritus_opts) {
  .bromeliad_detritus_opts %>%
    mutate(incident_radiation_percentage = if_else(
      bromeliad_id == "3846" & incident_radiation_percentage > 100,
      true = incident_radiation_percentage / 10,
      false = incident_radiation_percentage
    ))

}

# converting other existing columns to “open.canopy" ----------------------


#' create a conversion table for adding open.canopy
#'
#' Some values of either "visit_id" or "canopy_openess_chr" can be converted
#' into "open.canopy". In the case of canopy_openess_chr, we map the words to
#' either 1 (open) or 0(closed)
#'
#'converts edge to closed in Las gamas visits
#converts two visits 331, 311 (Nouragues inselberg) to value 1 (open), and visits 326, 316, 306 (Nouragues forest sites) to value 0(closed)
#converts cardoso open restinga to value 1 (open), and cardoso closed to value 0 (closed)

#open:211, 231, (311), (331), 336, 456, 471, 476, 481, 501,

#closed; 106, 111, 116, 121, 126, 131,136,141,146,151,156,161,166,171,176,181,191, 196, 201, (21), 241
# 246, 251, (306), (316), (326), 341, 346, 351, 356, 361, 366, 371, 376,391, 396,
# 401, 406, 41, 411, 416, 421, 426, 431, 436, 441, 446, 451, 46, 486, 51, 56, 61, 81, 91, 96

# Las gamas was handled by function 281,266,271...but check 491, 496, 506, 86
#asterisked visits: *286, *291, *296, *301 vriesea in PetitSaut is closed, aechmea mertensii is edge (variable, perhaps closer to edge) in kaw
#note that visit 286 from petitsaut
#note exclude visit 101 from entire matrix,can filter out collection method = NA
#note that visit id 276 is empty (andrews phd data)

#' @return A table which we can left_join with bromeliad_detritus
create_openness_conversion_table <- function(){
  convert_words_binary <- frame_data(
    ~canopy_openess_chr, ~open.canopy,
    "open",              1,
    "closed",            0,
    "edge",              0
  )

  convert_visitid <- bind_rows(data_frame(visit_id = c("211", "331", "311", "231", "336", "456", "471", "476", "481", "501"), open.canopy = 1),
                               data_frame(visit_id = c("326", "316", "306",  "21", "106", "111", "116", "121", "126", "131"), open.canopy = 0),
                               data_frame(visit_id = c("136", "141", "146", "151", "156", "161", "166", "171", "176", "181"), open.canopy = 0),
                               data_frame(visit_id = c("191", "196", "201", "241", "246", "251", "291", "341", "346", "351", "356"), open.canopy = 0),
                               data_frame(visit_id = c("361", "366", "371", "376", "391", "396", "401", "406",  "41", "411"), open.canopy = 0),
                               data_frame(visit_id = c("416", "421", "426", "431", "436", "441", "446", "451", "46", "486", "51", "56", "61", "81", "91", "96"), open.canopy = 0))


  new_openness_values <- expand.grid(visit_id = c("281","266","271"),
                                     canopy_openess_chr = c("open", "closed", "edge"),
                                     stringsAsFactors = FALSE) %>%
    left_join(convert_words_binary) %>%
    bind_rows(convert_visitid) %>%
    arrange(open.canopy)

  return(new_openness_values)
}

convert_incident_to_openness <- function(.bromeliad_detritus_open) {

  incident_convert<-function(a){
    ifelse(a>=50,1,(ifelse(a<50,0,NA)))
  }

  .bromeliad_detritus_open %>%
    mutate(open.canopy = ifelse(visit_id %in%c("296", "301"),incident_convert(incident_radiation_above_ground_percentage), open.canopy))%>%
    mutate(open.canopy = ifelse(visit_id %in%c("331"),incident_convert(incident_radiation_percentage), open.canopy))#%>%
    # mutate(open.canopy = ifelse(visit_id %in%c("491", "496","506"),`Canopy openess`, open.canopy))



}


# “correcting a value… that was correct already?? -------------------------

#  Apparently we first change this particular, single bromeliad.. but why?

# apparently we are supposed to turn its value of canopy.openess[281]<-"open"
# bromeliad_detritus_opts %>% filter(bromeliad_id == "2431")

#  what's around there?
# target_brom <- which(bromeliad_detritus_opts$bromeliad_id == "2431")
#
# bromeliad_detritus_opts %>%
#   slice((target_brom - 5):(target_brom + 5)) %>%
#   select(bromeliad_id, dataset_name, dplyr::contains("canopy"))


