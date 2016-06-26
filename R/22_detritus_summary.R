### summarizing detritus data.
### Andrew MacDonald June 2016


# pakages -----------------------------------------------------------------

require(dplyr)
require(readr)
require(tidyr)
library(stringr)



# read in data ------------------------------------------------------------

broms_detritus <- read_csv("data-raw/03_broms.csv",
                           col_types = cols(dead_leaves = "n"))
