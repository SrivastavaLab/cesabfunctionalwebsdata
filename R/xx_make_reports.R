## a quick example of how to create reports with R

## make sure your working directory is set appropriately (ie to the project location).

require(knitr)
spin("R/01_accessing_data.R", format = "Rmd")
