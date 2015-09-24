#### update Plants table and status table from Spring 2014 Data
#### 

rm(list = ls())

source( 'R/check_db_functions.R')
source( 'R/dbQueryTools.R' )

springStatus = read.csv('field_data/demographic_data/2014_spring_size_update_data.csv')

origin = '1899-12-30'
springStatus$date = as.Date(springStatus$date, origin = origin)
springStatus[ springStatus$date == as.Date('2014-06-01'), ] 

reborn = springStatus [ springStatus$earlySpringStatus %in% c(0, 2, 3) & springStatus$status == 1, ] 
reborn

newlyDeads = springStatus [ springStatus$earlySpringStatus == 1 & springStatus$status == 0,  ]
newlyDeads

uncertainDead = springStatus[ springStatus$earlySpringStatus == 1 & springStatus$status == 3, ]
uncertainDead

nowDead = springStatus[ springStatus$earlySpringStatus == 3 & springStatus$status == 0, ]
nowDead

allDead = springStatus[ springStatus$status == 0, ]
allDead  #### many recorded dead in early spring and again in late spring 

springStatusUpdate = data.frame( springStatus[, c('ID', 'date', 'TAG', 'c1', 'c2' , 'ch' , 'canopy')],  
                                 infls = NA, lv_stems = NA, dd_stems = NA, stem_d1 = springStatus$stem_d1, stem_d2 = NA, 
                                 springStatus[, c('status', 'notes', 'herbivory')])

springStatusUpdate$date = strftime(springStatusUpdate$date)

db = dbConnect(SQLite(), 'sage.sqlite')

springStatusUpdate[ springStatusUpdate$ID == 81, 'ch' ] <- NA
springStatusUpdate[ springStatusUpdate$ID == 81,  ] 

springStatusUpdate$herbivory[  is.na( springStatusUpdate$herbivory  ) ]  <- 0

springStatusUpdate$status[ springStatusUpdate$ID ==676 ] <- 0 #### plant 676 was gone, change status to "0" 

names(springStatusUpdate)[ which( names( springStatusUpdate) == 'TAG' ) ]  <- 'field_tag'  #### standardize tag name 
springStatusUpdate[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] <- as.numeric( unlist( springStatusUpdate[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] ))

springStatusUpdate[ springStatusUpdate$ID == 1103 , ] 
springStatusUpdate[ which( springStatusUpdate$ID == 411 ),  ] ##### dead plant 

lastDate = max(springStatusUpdate$date)
firstDate = min( springStatusUpdate$date)

active = dbGetQuery( db, "SELECT *, max(date) 
                          FROM plants 
                          JOIN status USING (ID) 
                          WHERE active = 1 AND start_date <= ? AND date <= ?
                          GROUP BY ID", list(lastDate, lastDate))

active.size = dbGetQuery( db, "SELECT *, max(date) 
                          FROM plants 
                          JOIN status USING (ID) 
                          WHERE active = 1 
                          AND start_date <= ? 
                          AND date <= ?
                          AND c1 IS NOT NULL
                          GROUP BY ID", list(lastDate, lastDate))


new = dbGetQuery( db, "SELECT * FROM plants 
                        WHERE active = 1 
                        AND start_date <= ? 
                        AND start_date >= ?", list( lastDate, firstDate))

#### run checks 
see_if( checkStatus (springStatusUpdate$status))
see_if( checkPlantID ( springStatusUpdate$ID))
see_if( checkTags( springStatusUpdate$field_tag, na.rm = FALSE))
see_if( checkDate( springStatusUpdate$date, na.rm= FALSE ))
see_if( checkHerbivory ( springStatusUpdate$herbivory ))
see_if( checkPositiveRange ( springStatusUpdate$ch, upper.limit= 200))
see_if( checkPositiveRange ( springStatusUpdate$c1, upper.limit = 200))
see_if( checkPositiveRange( springStatusUpdate$c2, upper.limit = 200))
see_if( checkPositiveRange( springStatusUpdate$stem_d1, upper.limit = 100))
see_if( checkPositiveRange( springStatusUpdate$stem_d2, upper.limit = 100))
see_if( checkPositiveRange( springStatusUpdate$canopy, upper.limit = 150))
see_if( checkPositiveRange( springStatusUpdate$infls, upper.limit = 900))
see_if( checkAllMonths( springStatusUpdate$date[ which( springStatusUpdate$infls > 0 )], early= 9, late = 11))

Bad <- checkActive( springStatusUpdate$ID, active$ID)
Bad
springStatusUpdate[ springStatusUpdate$ID %in% Bad, ] 
springStatusUpdate[ springStatusUpdate$ID %in% new$ID, 'ID']

missing <- checkForMissing( springStatusUpdate$ID, active$ID )
missing 

statusChangeReport( old = active, new = springStatusUpdate)

springStatusUpdate[ which( springStatusUpdate$ID %in% Bad & springStatusUpdate$status == 1), c('ID') ] ##### no live plants 
springStatusUpdate[ springStatusUpdate$ID %in% new$ID, "ID"] ### new plants
springStatusUpdate[ which( springStatusUpdate$ID %in% Bad & springStatusUpdate$status == 3), ] ##### uncertain plants 

springStatusUpdate$area = (springStatusUpdate$c1/2) * (springStatusUpdate$c2/2 ) * pi
active.size$area = (active.size$c1/2)*(active.size$c2/2)*pi

showSizeDiff( old= subset( active.size, area < 20), new = springStatusUpdate, measure= 'area')
showSizeDiff( old = active.size, new = springStatusUpdate, measure= 'area')
showSizeDiff( old = active.size, new = springStatusUpdate, measure = 'ch' ) ## check 172 and 211 
showSizeDiff( old = active.size, new = springStatusUpdate, measure = 'canopy') ## 
showSizeDiff( old = active.size, new = springStatusUpdate, measure = 'stem_d1') ## check 377
graphics.off()

springStatusUpdate[ springStatusUpdate$ID %in% c(172, 211), ]
dbGetQuery( db, "SELECT site, ID, ch, date FROM plants JOIN status USING (ID) WHERE ID IN (172, 211) ORDER BY ID, date")

springStatusUpdate = springStatusUpdate [ , -which( names( springStatusUpdate ) == 'area' )] #### remove area column

dbWriteTable(db, name = 'status', value = springStatusUpdate, append = TRUE, row.names = FALSE)

dbGetQuery(db, "UPDATE status SET stem_d1 = NULL WHERE stem_d1 = 0")

dbGetQuery(db, "UPDATE status SET ch = NULL WHERE (status != 1 OR ch = 0)")

dbGetQuery(db, "UPDATE status SET c1 = NULL WHERE (status != 1 OR c1 = 0)")

dbGetQuery(db, "UPDATE status SET c2 = NULL WHERE (status != 1 OR c1 = 0)")

reborn = dbGetQuery( db, q.reborn) #### find status changes from anything back to one 
reborn ##### These need to be reset to status 1 

now_dead = dbGetQuery( db, q.now.dead ) ##### find status going from 3 to 0 
now_dead

dbGetQuery( db, q.update.reborn)
dbGetQuery( db, q.update.now.dead)
dbGetQuery( db, q.update.end_date)
dbGetQuery( db, q.update.active)

dbGetQuery( db, "SELECT ID, status, active, end_date 
                  FROM 
                      (
                      SELECT status, max(date), ID 
                      FROM status 
                      GROUP BY ID
                      ) 
                  JOIN plants USING (ID) 
                  WHERE status = 1 
                  AND active = 0;")  

dbDisconnect(db)            # Close connection
