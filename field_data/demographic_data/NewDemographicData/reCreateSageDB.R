rm(list = ls())
library(xlsx)
library(RSQLite)

setwd('~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/')

plantsCols = c('character', 'integer', 'numeric', 'numeric', 'character', 
               rep('integer', 4), 'character', 'integer', 'integer', 'integer', 
               'integer', 'integer', 'character')
firstStatusCols = c('integer', 'integer', 'integer', rep('numeric', 9), 'integer', 'character', 'integer')

plants = read.xlsx2('2014_summer_plants_table.xlsx', sheetIndex= 1, startRow=3, 
                    colClasses = plantsCols)

firstStatus = read.xlsx2('2012_Fall_to_2013_SpringStatus.xlsx', sheetIndex=1, 
                         colClasses = firstStatusCols)

sites = read.csv('../../siteCharacteristics/site_positions.csv')

#### change numeric to date 
plants$tag_switched = strftime(as.Date(plants$tag_switched, origin = '1904-01-01'))
plants$start_date = strftime(as.Date(plants$start_date, origin = '1904-01-01'))
plants$end_date = strftime(as.Date(plants$end_date, origin = '1904-01-01'))
plants$date_treated = strftime(as.Date(plants$date_treated, origin = '1904-01-01'))

firstStatus$date = strftime(as.Date(firstStatus$date, origin = '1904-01-01'))


### round measurements less than one up to one
firstStatus [  which(firstStatus$c1 < 1) , 'c1' ]  <- 1
firstStatus [  which(firstStatus$c2 < 1) , 'c2' ]  <- 1
firstStatus [  which(firstStatus$ch < 1) , 'ch' ]  <- 1

### round small measurements 
### round 1/2 cm down 
firstStatus$c1 <- round(firstStatus$c1 - 0.01)
firstStatus$c2 <- round(firstStatus$c2 - 0.01)
firstStatus$ch <- round(firstStatus$ch - 0.01)


plantsTypes = sapply(plants, class) 
statusTypes = sapply(firstStatus, class)
sitesTypes = sapply(sites, class)

plantsTypes[1:length(plantsTypes)] <- c('character', "int", "real", "real", "character", 
                                        "int", "int", "int", "text", "character",
                                        "text", "text", "int", "int", "character",
                                        "character")

statusTypes[1:length(statusTypes)] <- c('int', 'text', 'int', rep('real', 9), 
                                        'int', 'character', 'int')

###### create the database 
db = dbConnect(SQLite(), dbname = '../sage.sqlite')

dbWriteTable(db, name = 'plants', field.types = as.list(plantsTypes), 
             value = plants, uniq = 'ID', row.names = FALSE, overwrite = TRUE)

dbWriteTable(db, name = 'status', field.types = as.list(statusTypes), 
             value = firstStatus, row.names = FALSE, overwrite = TRUE)

dbWriteTable(db, name = 'sites', value = sites, row.names = FALSE, 
             overwrite = TRUE)

dbListFields(db, 'plants')
dbListFields(db, 'status')
dbListFields(db, 'sites')

res = dbSendQuery( db, 'UPDATE plants SET treatment = Null WHERE treatment != "control" AND treatment != "remove" ')
dbClearResult(res)

dbDisconnect(db)            # Close connection

