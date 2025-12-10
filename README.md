Data preparation for Bromeliad Working Group datasets
===================================

This repository contains code that downloads and organizes all the data from the Bromeliad Working Group. 
This work was begun during the CESAB working group FunctionalWebs, which is what gives this repository its name. 

The updated workflow uses [targets](https://docs.ropensci.org/targets/). 
Targets is a package for creating and managing "make-like" workflows in R.
Workflows like this track how the various steps of the process are related, 
d also tracks which of these have become out of date.
A step might become out of date if the input has changed (for example, 
new data is added to the database) or if the process has changed (for example,
the function that corrects the name of a morphospecies is updated to fix a spelling error).

Targets is well-established, stable, and has excellent documentation. 
Be sure to [read the manual](https://books.ropensci.org/targets/) while maintaining and editing this repository!

Important files to note:

* `00_run_scripts.R` -- the "main" document, and the one you should open if you are working -- for example, if you are making a new data release. 
By default it does NOT download new data, but this is easy to toggle on and off. 
The assumption here is that the database itself changes slowly, but the code to clean those data and add new information to them changes more rapidly.
* `_targets.yaml` -- contains info that makes `00_run_scripts.R` work as it should. 
To understand the process here (and see the example code on which this is all based)
read [the chapter on Projects](https://books.ropensci.org/targets/projects.html) in the user manual.
* `01_download_data.R` -- downloads the BWG working group data. 
This requires the user's BWG password. Previously, `bwgdata` (the R package that accesses the database API)
prompted users to enter a username and password interactively. This won't work with the `targets` workflow, which is not interactive.
This is why bwgdata 0.4.0 uses environment variables to store usernames and passwords. 
Look at the [bwgdata Readme](https://github.com/SrivastavaLab/bwgdata) to know more about that! 
This workflow will **NOT WORK** unless `bwgdata` is at version 0.4.0, which dates from October 2025.
* `02_process_data` UNDER CONSTRUCTION. corrects, combines, and adds additional information to the dataset.

## Authenticating with the OSF

some data files associated with this project are now hosted at the OSF.
To download them you need an osf PAT.
See this [documentation](https://docs.ropensci.org/osfr/articles/auth)
which also links to [this book chapter](https://rstats.wtf/r-startup.html)

