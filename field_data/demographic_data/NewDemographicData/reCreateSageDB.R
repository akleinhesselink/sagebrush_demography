rm(list = ls())
library(RSQLite)

source( 'check_db_functions.R')

setwd('~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/')


plants = read.csv('MasterPlantsTable.csv')
firstStatus = read.csv('2012_Fall_to_2013_SpringStatus.csv')
sites = read.csv('../../siteCharacteristics/site_positions.csv')


#### change numeric to date 

origin = '1904-01-01' ### use correct origin for microsoft excel 
plants$tag_switched = strftime(as.Date(plants$tag_switched, origin = origin))

plants$start_date = strftime(as.Date(plants$start_date, origin = origin))
plants$end_date = strftime(as.Date(plants$end_date, origin = origin))
plants$date_treated = strftime(as.Date(plants$date_treated, origin = origin))

firstStatus$date = strftime(as.Date(firstStatus$date, origin = origin))
max(firstStatus$date, na.rm= TRUE)
min(firstStatus$date, na.rm = TRUE)

firstStatus <- firstStatus [ !is.na( firstStatus$ID), ]  #### drop where ID == NA's
print( firstStatus[ is.na( firstStatus$date)]) #### print missing dates 

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

firstStatus$canopy <- as.numeric(firstStatus$canopy)
firstStatus$herbivory[ is.na( firstStatus$herbivory) ] <- 0 
firstStatus$infls <- as.numeric(firstStatus$infls)
firstStatus[ is.na( firstStatus$infls ) , 'infls'] <- 0 

###### Run the checks 
see_if( checkSiteLabels( sites$site))
see_if( checkPlantID ( plants$ID ))
see_if( checkSiteLabels( plants$site))
see_if( checkTags ( plants$tag1, na.rm = FALSE))
see_if( checkTags ( plants$tag2, na.rm= TRUE)) 
see_if( checkDate( plants$start_date))
see_if( checkDate( plants$end_date, na.rm = TRUE ) )
see_if( checkStatus( plants$active ))
see_if( checkDate( plants$date_treated))


see_if( checkStatus( firstStatus$status))
see_if( checkTags( firstStatus$ID, na.rm = FALSE))
see_if( checkTags( firstStatus$field_tag, na.rm = FALSE))
see_if( checkDate( firstStatus$date, na.rm= FALSE ))
see_if( checkHerbivory ( firstStatus$herbivory ))
see_if( checkPositiveRange ( firstStatus$ch, upper.limit= 200))
see_if( checkPositiveRange ( firstStatus$c1, upper.limit = 200))
see_if( checkPositiveRange( firstStatus$c2, upper.limit = 200))
see_if( checkPositiveRange( firstStatus$stem_d1, upper.limit = 100))
see_if( checkPositiveRange( firstStatus$stem_d2, upper.limit = 100))
see_if( checkPositiveRange( firstStatus$canopy, upper.limit = 150))
see_if( checkPositiveRange( firstStatus$infls, upper.limit = 900))
see_if( checkAllMonths( firstStatus$date[ which( firstStatus$infls > 0 )], early= 9, late = 11))

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

res = dbSendQuery( db, "SELECT ID, field_tag, date FROM status WHERE date(date) > date('2013-04-01') AND 
                   date(date) < date('2013-08-01') AND status = 0")
springDeadUpdate = fetch(res)
dbClearResult(res)

names(springDeadUpdate) 

for(i in 1:nrow(springDeadUpdate)){ 
  ID = springDeadUpdate[i, 'ID']
  date = springDeadUpdate[i, 'date']
  
  res = dbSendQuery( db, "UPDATE plants SET active = 0, end_date = ? WHERE ID = ? AND active = 1", 
                     list(date, ID))
  dbClearResult(res)
}

dbDisconnect(db)            # Close connection