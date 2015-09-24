#!/bin/bash

Rscript R/make_plants_table.R 
Rscript R/make_sites_table.R
Rscript R/2013_spring_status_update.R
Rscript R/2013_summer_status_update.R 
Rscript R/2013_fall_status_update.R
Rscript R/2014_early_spring_status_update.R
Rscript R/2014_spring_transplants_status_update.R 
Rscript R/2014_spring_status_update.R
Rscript R/2014_fall_status_update.R
Rscript R/2015_spring_status_update.R
Rscript R/makeFlowersTable.R
Rscript R/UpdateFlowersTable.R
Rscript R/add_snow_mold_transplants.R
