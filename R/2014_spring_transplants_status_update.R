
source('R/2014_early_spring_status_update.R')

earlySpringTransplants = read.csv('field_data/demographic_data/2014_Early_Spring_Fall_Transplants_Update.csv')

origin = '1899-12-30'
earlySpringTransplants$date = as.Date(earlySpringTransplants$date, origin = origin)
earlySpringTransplants$spring = as.Date(earlySpringTransplants$spring, origin = origin)

fallTransplantsUpdate = data.frame(earlySpringTransplants[, c('spring', 'tag')], c1 = NA, c2 = NA, ch = NA, 
                                canopy = NA, infls = NA, lv_stems = NA, dd_stems = NA, stem_d1 = NA, stem_d2 = NA, 
                                earlySpringTransplants[, c('status', 'notes', 'herbivory')])

names(fallTransplantsUpdate)[ c(1, 2)] <- c('date','field_tag') #### standard names for headers 

fallTransplantsUpdate$date = strftime(fallTransplantsUpdate$date)

db = dbConnect(SQLite(), dbname = 'sage.sqlite')

res = dbSendQuery( db, "SELECT ID, tag1 FROM plants WHERE class = 5")
class5s = fetch(res, -1)
dbClearResult(res)

fallTransplantsUpdate = merge(class5s, fallTransplantsUpdate, by.x = 'tag1', by.y = 'field_tag')
names(fallTransplantsUpdate)[1] <- 'field_tag'

fallTransplantsUpdate = cbind(ID = fallTransplantsUpdate$ID, date = fallTransplantsUpdate$date, 
                              field_tag = fallTransplantsUpdate$field_tag, fallTransplantsUpdate[, -c(1:3)])

fallTransplantsUpdate$herbivory[  is.na( fallTransplantsUpdate$herbivory  ) ]  <- 0
fallTransplantsUpdate = fallTransplantsUpdate[ !is.na(fallTransplantsUpdate$status),  ] ### drop status NA
fallTransplantsUpdate[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] <- as.numeric( unlist( fallTransplantsUpdate[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] ))

fallTransplantsUpdate$date = as.character( fallTransplantsUpdate$date )

lastDate = max(fallTransplantsUpdate$date)
active = dbGetPreparedQuery( db, "SELECT *, max(date) FROM plants 
                   JOIN status USING (ID) 
                   WHERE active = 1 AND start_date <= ? AND date <= ?
                   GROUP BY ID", data.frame(lastDate, lastDate))

#### run checks 
see_if( checkStatus (fallTransplantsUpdate$status))
see_if( checkPlantID ( fallTransplantsUpdate$ID))
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

Bad = checkActive(fallTransplantsUpdate$ID, active$ID )
Bad

missing = checkForMissing( fallTransplantsUpdate$ID, active$ID ) #### 
active[ active$ID %in% missing, c('site', 'class')] #### missing all the non-class 5s 

statusChangeReport( old = active, new= fallTransplantsUpdate )

dbWriteTable(db, name = 'status', value = fallTransplantsUpdate, 
             append = TRUE, row.names = FALSE)

reborn = dbGetQuery( db, q.reborn) #### find status changes from anything back to one 
reborn ##### These need to be reset to status 1 

now_dead = dbGetQuery( db, q.now.dead ) ##### find status going from 3 to 0 
now_dead

dbGetQuery( db, q.update.now.dead ) #### update status to 0 when they go from 3 to 0 

now_dead = dbGetQuery( db, q.now.dead ) ##### find status going from 3 to 0 
now_dead$ID

dbGetQuery( db, q.update.end_date ) #### , rep(early_date, 2)) 

dbGetQuery( db, q.update.active )


dbDisconnect(db)            # Close connection
