#### update Plants table and status table from early Spring Data
#### open sage.sqlite
#### read in 2014_EarlySpring_update
#### update plants table active and end_date columns
#### trick here will be going back and changing status of 
#### fall plants with status 2 or 3.  These will be changed
#### to 0 if the plant is a 0 in the spring of 2014.


rm(list = ls())
library(RSQLite)
source('check_db_functions.R')
source('dbQueryTools.R')

earlySpringStatus = read.csv('2014_Early_Spring_Update.csv')

earlySpringStatus$date2 = as.Date(earlySpringStatus$date2, origin = '1899-12-30')

earlySpringUpdate = data.frame( earlySpringStatus[, c('ID', 'date2', 'TAG')], c1 = NA, c2 = NA, ch = NA, 
                                canopy = NA, infls = NA, lv_stems = NA, dd_stems = NA, stem_d1 = NA, stem_d2 = NA, 
                                earlySpringStatus[, c('Spring.Status', 'Spring.Notes', 'herbivory')])

names(earlySpringUpdate)[ c(2, 3, 13, 14)] <- c('date','field_tag', 'status', 'notes') #### standard names for headers 

earlySpringUpdate$date = strftime(earlySpringUpdate$date)

earlySpringUpdate$herbivory[  is.na( earlySpringUpdate$herbivory  ) ]  <- 0
earlySpringUpdate = earlySpringUpdate[ !is.na(earlySpringUpdate$status),  ] ### drop status NA

earlySpringUpdate[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] <- as.numeric( unlist( earlySpringUpdate[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] ))

db = dbConnect(SQLite(), dbname = 'sage.sqlite')

lastDate = max(earlySpringUpdate$date)
active = dbGetQuery( db, "SELECT *, max(date) FROM plants 
                   JOIN status USING (ID) 
                   WHERE active = 1 AND start_date <= ? AND date <= ?
                   GROUP BY ID", list(lastDate, lastDate))
#### run checks 
see_if( checkStatus (earlySpringUpdate$status))
see_if( checkPlantID ( earlySpringUpdate$ID))
see_if( checkTags( earlySpringUpdate$field_tag, na.rm = FALSE))
see_if( checkDate( earlySpringUpdate$date, na.rm= FALSE ))
see_if( checkHerbivory ( earlySpringUpdate$herbivory ))

see_if( checkPositiveRange ( earlySpringUpdate$ch, upper.limit= 200))
see_if( checkPositiveRange ( earlySpringUpdate$c1, upper.limit = 200))
see_if( checkPositiveRange( earlySpringUpdate$c2, upper.limit = 200))
see_if( checkPositiveRange( earlySpringUpdate$stem_d1, upper.limit = 100))
see_if( checkPositiveRange( earlySpringUpdate$stem_d2, upper.limit = 100))
see_if( checkPositiveRange( earlySpringUpdate$canopy, upper.limit = 150))
see_if( checkPositiveRange( earlySpringUpdate$infls, upper.limit = 900))
see_if( checkAllMonths( earlySpringUpdate$date[ which( earlySpringUpdate$infls > 0 )], early= 9, late = 11))

Bad = checkActive( earlySpringUpdate$ID, active$ID)
Bad

earlySpringUpdate[ earlySpringUpdate$ID %in% Bad , ] #### as long as the spring status is 0 it should be ok, just confirming ones that died in the fall

missing = checkForMissing( earlySpringUpdate$ID, active$ID ) 
sort(missing)

missingInfo = dbGetQuery( db, paste( "SELECT ID, start_date, end_date, class, site 
                                      FROM plants  
                                      WHERE ID IN (", questionMarks( missing ), ") 
                                      ORDER BY ID, site;"), missing) 

missingInfo #### mostly class 5s spring Transplant update catches the 5's later 
                                      ##### 771 is a new seedling is a class 6 and needs to be added to the datasheet 
                                      ###### watch for 678 a class 4 in later updates 

newSeedling = earlySpringUpdate[ earlySpringUpdate$field_tag == 771, ] ### make new record to append to status update 
newSeedling$ID = 1087
newSeedling$status = 1 
earlySpringUpdate = rbind( earlySpringUpdate, newSeedling)

statusChangeReport( old = active, new = earlySpringUpdate )

dbWriteTable(db, name = 'status', value = earlySpringUpdate, 
             append = TRUE, row.names = FALSE)

early_date = strftime( as.Date(min(earlySpringUpdate$date)) - 1 ) 

reborn = dbGetQuery( db, q.reborn) #### find status changes from anything back to one 
reborn ##### These need to be reset to status 1 

dbGetQuery( db, q.now.dead ) ##### find status going from 3 to 0 

dbGetQuery( db, q.update.now.dead ) #### update status to 0 when they go from 3 to 0 

dbGetQuery( db, q.now.dead ) ##### find status going from 3 to 0 

dbGetQuery( db, q.update.end_date ) #### , rep(early_date, 2)) 

dbGetQuery( db, q.update.active )

dbDisconnect(db)            # Close connection
