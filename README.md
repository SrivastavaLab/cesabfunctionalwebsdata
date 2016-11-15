Data preparation for FUNCTIONALWEBS
===================================

Data cleaning and organization for CESAB FUNCTIONALWEBS papers

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    dir(pattern = "^data.*html") %>% paste0("http://aammd.info/data_quality/", .)

    ## [1] "http://aammd.info/data_quality/data_quality_Argentina.html"           
    ## [2] "http://aammd.info/data_quality/data_quality_Brazil.html"              
    ## [3] "http://aammd.info/data_quality/data_quality_Colombia.html"            
    ## [4] "http://aammd.info/data_quality/data_quality_Costa_Rica.html"          
    ## [5] "http://aammd.info/data_quality/data_quality_Dominica.html"            
    ## [6] "http://aammd.info/data_quality/data_quality_French_Guiana.html"       
    ## [7] "http://aammd.info/data_quality/data_quality_Honduras.html"            
    ## [8] "http://aammd.info/data_quality/data_quality_Netherlands_Antilles.html"
    ## [9] "http://aammd.info/data_quality/data_quality_Puerto_Rico.html"
