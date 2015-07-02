#### update Plants table and status table 
#### open sage.sqlite
#### read in 2013_Fall_update
#### update plants table active and end_date columns
#### tricky to match plants in plants table with the
#### current tag number.  New tags do not correspond to IDs
#### and sometimes plants switch to a new  tag #

rm(list = ls())
library(RSQLite)
source('check_db_functions.R')

fallStatus = read.csv('2013_Fall_update.csv')
  
fallTransplants = read.csv('2013_NewFallTransplantsStatus.csv')

origin = '1899-12-30'
fallStatus$date = as.Date(fallStatus$date, origin = origin)
fallTransplants$date = as.Date(fallTransplants$date, origin = origin)

fallStatus$date = strftime(fallStatus$date)
fallTransplants$date = strftime(fallTransplants$date)

fallStatusUpdate = fallStatus[, c('ID', 'date', 'field_tag', 
                                  'c1', 'c2', 'ch', 'canopy', 'infls', 
                                  'lv_stems', 'dd_stems', 'stem_d1', 
                                  'stem_d2', 'status','notes', 'herbivory')]

fallTransplantsUpdate = fallTransplants[, c('date', 'field_tag', 'c1', 'c2', 'ch', 
                                            'canopy', 'infls', 'lv_stems', 'dd_stems', 'stem_d1', 
                                            'stem_d2', 'status', 'notes', 'herbivory')]

db = dbConnect(SQLite(), dbname = 'sage.sqlite')

fallStatusUpdate$herbivory[ is.na( fallStatusUpdate$herbivory )  ] <- 0 
fallStatusUpdate$stem_d2 = as.numeric( fallStatusUpdate$stem_d2) 


lastDate = max(fallStatusUpdate$date)
res = dbSendQuery( db, "SELECT * FROM plants WHERE active = 1 AND date( start_date) <= date( ? )" , list( lastDate))
active = fetch( res, -1)
dbClearResult( res )

#### run Checks 
see_if( checkPlantID( fallStatusUpdate$ID ))
see_if( checkStatus( fallStatusUpdate$status))
see_if( checkTags( fallStatusUpdate$field_tag, na.rm = FALSE))
see_if( checkDate( fallStatusUpdate$date, na.rm= FALSE ))
see_if( checkHerbivory ( fallStatusUpdate$herbivory ))
see_if( checkPositiveRange ( fallStatusUpdate$ch, upper.limit= 200))
see_if( checkPositiveRange ( fallStatusUpdate$c1, upper.limit = 200))
see_if( checkPositiveRange( fallStatusUpdate$c2, upper.limit = 200))
see_if( checkPositiveRange( fallStatusUpdate$stem_d1, upper.limit = 100))
see_if( checkPositiveRange( fallStatusUpdate$stem_d2, upper.limit = 100))
see_if( checkPositiveRange( fallStatusUpdate$canopy, upper.limit = 150))
see_if( checkPositiveRange( fallStatusUpdate$infls, upper.limit = 900))
see_if( checkAllMonths( fallStatusUpdate$date[ which( fallStatusUpdate$infls > 0 )], early= 9, late = 11))

checkActive( x=fallStatusUpdate$ID , active= active$ID)
checkForMissing( fallStatusUpdate$ID, active = active$ID)

dbWriteTable(db, name = 'status', value = fallStatusUpdate, 
             append = TRUE, row.names = FALSE)

##### assign ID's to new fall transplants in STATUS table 
res = dbSendQuery( db, "SELECT ID, tag1 FROM plants WHERE class = 5")
class5s = fetch(res, -1)
dbClearResult(res)

fallTransplantsUpdate = merge(class5s, fallTransplantsUpdate, by.x = 'tag1', by.y = 'field_tag')
names(fallTransplantsUpdate)[1] <- 'field_tag'

fallTransplantsUpdate = cbind(ID = fallTransplantsUpdate$ID, date = fallTransplantsUpdate$date, 
                              field_tag = fallTransplantsUpdate$field_tag, fallTransplantsUpdate[, -c(1:3)])

fallTransplantsUpdate$date <- as.character(fallTransplantsUpdate$date ) 

fallTransplantsUpdate$herbivory <- 0 
fallTransplantsUpdate$stem_d2 <- as.numeric(fallTransplantsUpdate$stem_d2)
fallTransplantsUpdate$canopy <- as.numeric( fallTransplantsUpdate$canopy)
fallTransplantsUpdate$infls <- 0

#### run checks 
see_if( checkPlantID( fallTransplantsUpdate$ID))
see_if( checkStatus( fallTransplantsUpdate$status))
see_if( checkTags( fallTransplantsUpdate$field_tag, na.rm = FALSE))
see_if( checkDate( fallTransplantsUpdate$date, na.rm= FALSE ))
see_if( checkHerbivory ( fallTransplantsUpdate$herbivory ))
see_if( checkPositiveRange ( fallTransplantsUpdate$ch, upper.limit= 200))
see_if( checkPositiveRange ( fallTransplantsUpdate$c1, upper.limit = 200))
see_if( checkPositiveRange( fallTransplantsUpdate$c2, upper.limit = 200))
see_if( checkPositiveRange( fallTransplantsUpdate$stem_d1, upper.limit = 100))
see_if( checkPositiveRange( fallTransplantsUpdate$stem_d2, upper.limit = 100))
see_if( checkPositiveRange( fallTransplantsUpdate$canopy, upper.limit = 150))
see_if( checkPositiveRange( fallTransplantsUpdate$infls, upper.limit = 900))
see_if( checkAllMonths( fallTransplantsUpdate$date[ which( fallTransplantsUpdate$infls > 0 )], early= 9, late = 11))

lastDate = max(fallTransplantsUpdate$date)
res = dbSendQuery( db, "SELECT * FROM plants WHERE active = 1 AND date( start_date) <= date( ? )" , list( lastDate))
active = fetch( res, -1)
dbClearResult( res )

checkActive( fallTransplantsUpdate$ID, active= active$ID ) 
checkForMissing( c(fallStatusUpdate$ID, fallTransplantsUpdate$ID), active = active$ID) #### check both for fallUpdate and transplantsUpdate 

dbWriteTable(db, name = 'status', value = fallTransplantsUpdate, 
             append = TRUE, row.names = FALSE)

res = dbSendQuery( db, "SELECT ID, field_tag, date FROM status WHERE date(date) > date('2013-08-30') AND 
                   date(date) < date('2014-01-01') AND (status = 0 OR status = 2)")

fallDeadUpdate = fetch(res)
dbClearResult(res)

for(i in 1:nrow(fallDeadUpdate)){ 
  ID = fallDeadUpdate[i, 'ID']
  date = fallDeadUpdate[i, 'date']
  
  res = dbSendQuery( db, "UPDATE plants SET active = 0, end_date = ? WHERE ID = ? AND active = 1", 
                     list(date, ID))
  dbClearResult(res)
}


dbDisconnect(db)            # Close connection

