rm(list = ls())
library(RSQLite)

source( 'check_db_functions.R')

plants = read.csv('MasterPlantsTable.csv')

#### change numeric to date 
origin = '1904-01-01' ### use correct origin for microsoft excel 

plants$tag_switched = strftime(as.Date(plants$tag_switched, origin = origin))
plants$start_date = strftime(as.Date(plants$start_date, origin = origin))
plants$end_date = strftime(as.Date(plants$end_date, origin = origin))
plants$date_treated = strftime(as.Date(plants$date_treated, origin = origin))
plants$treatment[ which( !plants$treatment %in% c('control', 'remove') )  ]  <- NA  #### re-format blank treatments 
plants$treatment = factor(plants$treatment)

plantsTypes = sapply(plants, class) 

plantsTypes[1:length(plantsTypes)] <- c('character', "int", "real", "real", "character", 
                                        "int", "int", "int", "text", "character",
                                        "text", "text", "int", "int", "character",
                                        "character")

#### Run Checks 
see_if( checkPlantID ( plants$ID ))
see_if( checkSiteLabels( plants$site))
see_if( checkTags ( plants$tag1, na.rm = FALSE))
see_if( checkTags ( plants$tag2, na.rm= TRUE)) 
see_if( checkDate( plants$start_date))
see_if( checkDate( plants$end_date, na.rm = TRUE ) )
see_if( checkStatus( plants$active ))
see_if( checkDate( plants$date_treated))
see_if( checkSpecies ( plants$species))
see_if( checkClasses( plants$class))
see_if( checkTreatment( plants$treatment))


db = dbConnect(SQLite(), dbname = 'sage.sqlite') ### create database 

dbWriteTable(db, name = 'plants', field.types = as.list(plantsTypes), 
             value = plants, uniq = 'ID', row.names = FALSE, overwrite = TRUE)

res = dbSendQuery( db, 'UPDATE plants SET treatment = Null WHERE treatment != "control" AND treatment != "remove" ')
dbClearResult(res)

dbDisconnect(db)            # Close connection
