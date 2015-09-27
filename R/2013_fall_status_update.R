source('R/2013_summer_status_update.R')

fallStatus = read.csv('field_data/demographic_data/2013_Fall_update.csv')
  
fallTransplants = read.csv('field_data/demographic_data/2013_NewFallTransplantsStatus.csv')

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
active = dbGetPreparedQuery( db, "SELECT *, max(date) FROM plants 
                   JOIN status USING (ID) 
                   WHERE active = 1 AND start_date <= ? AND date <= ?
                   GROUP BY ID", data.frame(lastDate, lastDate))

active.size = dbGetPreparedQuery( db, "SELECT *, max(date) FROM plants 
                   JOIN status USING (ID) 
                   WHERE active = 1 AND start_date <= ? AND date <= ? AND ch IS NOT NULL
                   GROUP BY ID", data.frame(lastDate, lastDate))

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

Bad = checkActive( x=fallStatusUpdate$ID , active= active$ID)
Bad

missing = checkForMissing( fallStatusUpdate$ID, active = active$ID)
missing 

statusChangeReport( old= active, new = fallStatusUpdate)

active.size$area <- (active.size$c1/2)*(active.size$c2/2)*pi 
fallStatusUpdate$area <- (fallStatusUpdate$c1/2)*(fallStatusUpdate$c2/2)*pi

showSizeDiff( old= active.size, new = fallStatusUpdate, measure= 'ch')
showSizeDiff( old = active.size, new = fallStatusUpdate, measure = 'area')
showSizeDiff( old = active.size, new = fallStatusUpdate, measure = 'stem_d1')
showSizeDiff( old = active.size, new = fallStatusUpdate, measure = 'canopy')

##### if status 0 --> 1 then change old zero to 1 
##### if status 3 --> 1 then change old 3 to 1 
##### if status 3 --> 0 then change old 3 to 0 

fallStatusUpdate[ fallStatusUpdate$ID == 10 , ]
fallStatusUpdate$status[ fallStatusUpdate$ID == 10 ] <- 0 ##### 10 is a 2 here but shows up as dead in the spring 2014 so I assign it a 0 here.

fallStatusUpdate = fallStatusUpdate [ , -which( names( fallStatusUpdate) == 'area' ) ] ## drop area 

dbWriteTable(db, name = 'status', value = fallStatusUpdate, 
             append = TRUE, row.names = FALSE)

##### assign ID's to new fall transplants in STATUS table 
class5s = dbGetQuery( db, "SELECT ID, tag1 FROM plants WHERE class = 5")

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

active_transplants <- dbGetPreparedQuery( db, "SELECT * FROM plants WHERE active = 1 AND date( start_date) <= date( ? )" , data.frame(date = lastDate))

Bad = checkActive( fallTransplantsUpdate$ID, active= active_transplants$ID ) 
Bad

missing = checkForMissing( c(fallStatusUpdate$ID, fallTransplantsUpdate$ID), active = active_transplants$ID) #### check both for fallUpdate and transplantsUpdate 
missing 

dbWriteTable(db, name = 'status', value = fallTransplantsUpdate, 
             append = TRUE, row.names = FALSE)


early_date = strftime( as.Date(min(c(fallStatusUpdate$date, fallTransplantsUpdate$date))) - 1 ) 

exceptions = 261 
fallStatusUpdate[ fallStatusUpdate$ID == 248, ] 

reborn = dbGetQuery( db, q.reborn) #### find status changes from anything back to one 
reborn ##### These need to be reset status to 1 in the summer 

dbGetQuery( db, q.update.reborn)
dbGetQuery( db, "SELECT * FROM status WHERE ID IN (647, 608, 596)")

now_dead = dbGetQuery( db, q.now.dead ) ##### find status going from 3 to 0 
dbGetQuery( db, q.update.now.dead ) #### update status to 0 when they go from 3 to 0 

#### Update plants table: 
dbGetQuery( db, q.update.end_date) # rep(early_date, 2)) 

dbGetPreparedQuery( db, makeExceptionalUpdateQuery( exceptions ), as.data.frame(matrix(rep(exceptions, 2), nrow = 1)))

dbGetQuery( db, q.update.active )

dbGetQuery( db, "SELECT plants.ID, end_date, date, status FROM plants JOIN status USING (ID) WHERE NOT status = 1 AND end_date IS NULL ORDER BY ID, date")

dbDisconnect(db)            # Close connection
graphics.off()

