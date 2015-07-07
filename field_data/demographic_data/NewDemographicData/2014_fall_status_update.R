#### update Plants table and status table from Fall 2014 Data
#### 
####
rm(list = ls())
library(RSQLite)
source( 'check_db_functions.R')

fallStatus = read.csv('2014_FallData.csv')

head( fallStatus[, 1:15])
head( fallStatus[ is.na(fallStatus$date), ] ) 

origin = '1899-12-30'
fallStatus$date = as.Date(fallStatus$date, origin = origin)

fallStatus$TAG = fallStatus$tag2
fallStatus$TAG[ is.na( fallStatus$TAG ) ] <- fallStatus$tag1[ is.na( fallStatus$TAG ) ] 

fallStatusUpdate = data.frame( fallStatus[, c('ID', 'date', 'TAG', 'c1', 'c2' , 'ch' , 'canopy', 'infls')],  
                                 lv_stems = NA, dd_stems = NA, stem_d1 = fallStatus$stemd1, stem_d2 = NA, 
                                 fallStatus[, c('status', 'notes', 'herbivory')])

fallStatusUpdate$date = strftime(fallStatusUpdate$date)

fallStatusUpdate$status[ fallStatusUpdate$ID == 1052 ] <- 0 #### Was dead in the spring so assigning it dead here 

fallDeadUpdate = subset( fallStatusUpdate, status == 0)

fallStatusUpdate$herbivory[  is.na( fallStatusUpdate$herbivory  ) ]  <- 0

fallStatusUpdate = fallStatusUpdate[ !is.na( fallStatusUpdate$status ),  ] #### drop those with NA for status



names(fallStatusUpdate)[ which( names( fallStatusUpdate) == 'TAG' ) ]  <- 'field_tag'  #### standardize tag name 
fallStatusUpdate[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] <- as.numeric( unlist( fallStatusUpdate[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] ))


db = dbConnect(SQLite(), 'sage.sqlite')

lastDate = max(fallStatusUpdate$date)
res = dbSendQuery( db, "SELECT *, max(date) FROM plants 
                   JOIN status USING (ID) 
                   WHERE active = 1 AND start_date <= ? AND date <= ?
                   GROUP BY ID", list(lastDate, lastDate))

active = fetch( res, -1)
dbClearResult( res )


#### run checks 
see_if( checkStatus (fallStatusUpdate$status))
see_if( checkPlantID ( fallStatusUpdate$ID))
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

Bad = checkActive( fallStatusUpdate$ID, active$ID )
Bad

fallStatusUpdate[ fallStatusUpdate$ID %in% Bad & fallStatusUpdate$status == 1, ] #### should all be zeros
missing = checkForMissing( fallStatusUpdate$ID, active$ID)
missing 

dbWriteTable(db, name = 'status', value = fallStatusUpdate, append = TRUE, row.names = FALSE)



for(i in 1:nrow(fallDeadUpdate)){ 
  ID = fallDeadUpdate[i, 'ID']
  date = fallDeadUpdate[i, 'date']
  res = dbSendQuery( db, "UPDATE plants SET active = 0, end_date = ? 
                     WHERE ID = ? AND active = 1", list(date, ID)) 
  dbClearResult(res)
}

res = dbSendQuery(db, "UPDATE status SET stem_d1 = NULL WHERE (status != 1 OR stem_d1 = 0)")
dbClearResult(res)

res = dbSendQuery(db, "UPDATE status SET ch = NULL WHERE (status != 1 OR ch = 0)")
dbClearResult(res)

res = dbSendQuery(db, "UPDATE status SET c1 = NULL WHERE (status != 1 OR c1 = 0)")
dbClearResult(res)

res = dbSendQuery(db, "UPDATE status SET c2 = NULL WHERE (status != 1 OR c1 = 0)")
dbClearResult(res)

res = dbSendQuery(db, "UPDATE status SET infls = NULL WHERE (status != 1 OR c1 = 0)")
dbClearResult(res)

dbDisconnect(db)            # Close connection
