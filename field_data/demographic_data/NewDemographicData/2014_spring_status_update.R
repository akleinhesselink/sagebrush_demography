#### update Plants table and status table from Spring 2014 Data
#### 
#### This script runs the first five scripts first to initialize the database 
#### It then adds the data from late spring 2014 
####


rm(list = ls())
library(RSQLite)
source( 'check_db_functions.R')

springStatus = read.csv('2014_spring_size_update_data.csv')

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

newSeedling = springStatusUpdate[ which( springStatusUpdate$ID == 411 ),  ] ##### new seedling 
newSeedling$status = 1
newSeedling$ID = 1103
newSeedling$notes = "New seedling located near old plant that died"
springStatusUpdate <- rbind( springStatusUpdate, newSeedling)

lastDate = max(springStatusUpdate$date)
res = dbSendQuery( db, "SELECT * FROM plants WHERE active = 1 AND date( start_date) <= date( ? )" , list( lastDate))
active = fetch( res, -1)
dbClearResult( res )

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
assert_that( length(Bad) == 0 )

missing <- checkForMissing( springStatusUpdate$ID, active$ID )
assert_that( length( missing) == 0 ) 

springStatusUpdate[ which( springStatusUpdate$ID %in% Bad & springStatusUpdate$status == 1), ] ##### live plants 
springStatusUpdate[ which( springStatusUpdate$ID %in% Bad & springStatusUpdate$status == 3), ] ##### uncertain plants 


dbWriteTable(db, name = 'status', value = springStatusUpdate, append = TRUE, row.names = FALSE)

res = dbSendQuery( db, "SELECT ID, field_tag, date FROM status 
                   WHERE date(date) >= date('2014-05-19') AND status = 0")
springDeadUpdate = fetch(res, -1)
dbClearResult(res)
springDeadUpdate

for(i in 1:nrow(springDeadUpdate)){ 
  ID = springDeadUpdate[i, 'ID']
  date = springDeadUpdate[i, 'date']
  res = dbSendQuery( db, "UPDATE plants SET active = 0, end_date = ? 
                     WHERE ID = ? AND active = 1", list(date, ID))
  dbClearResult(res)
}

res = dbSendQuery(db, "UPDATE status SET stem_d1 = NULL WHERE stem_d1 = 0")
dbClearResult(res)

res = dbSendQuery(db, "UPDATE status SET ch = NULL WHERE (status != 1 OR ch = 0)")
dbClearResult(res)

res = dbSendQuery(db, "UPDATE status SET c1 = NULL WHERE (status != 1 OR c1 = 0)")
dbClearResult(res)

res = dbSendQuery(db, "UPDATE status SET c2 = NULL WHERE (status != 1 OR c1 = 0)")
dbClearResult(res)

dbDisconnect(db)            # Close connection
