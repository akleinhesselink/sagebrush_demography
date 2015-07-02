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

earlySpringStatus = read.csv('2014_Early_Spring_Update.csv')

earlySpringStatus$date2 = as.Date(earlySpringStatus$date2, origin = '1899-12-30')

earlySpringUpdate = data.frame( earlySpringStatus[, c('ID', 'date2', 'TAG')], c1 = NA, c2 = NA, ch = NA, 
                                canopy = NA, infls = NA, lv_stems = NA, dd_stems = NA, stem_d1 = NA, stem_d2 = NA, 
                                earlySpringStatus[, c('Spring.Status', 'Spring.Notes', 'herbivory')])

names(earlySpringUpdate)[ c(2, 3, 13, 14)] <- c('date','field_tag', 'status', 'notes') #### standard names for headers 

earlySpringUpdate$date = strftime(earlySpringUpdate$date)
earlySpringUpdate[ earlySpringUpdate$status == 0, ]
earlySpringUpdate$herbivory[  is.na( earlySpringUpdate$herbivory  ) ]  <- 0
earlySpringUpdate = earlySpringUpdate[ !is.na(earlySpringUpdate$status),  ] ### drop status NA

earlySpringUpdate[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] <- as.numeric( unlist( earlySpringUpdate[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] ))

db = dbConnect(SQLite(), dbname = 'sage.sqlite')

lastDate = max(earlySpringUpdate$date)
res = dbSendQuery( db, "SELECT * FROM plants WHERE active = 1 AND date( start_date) <= date( ? )" , list( lastDate))
active = fetch( res, -1)
dbClearResult( res )

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

checkActive( earlySpringUpdate$ID, active$ID)

bad = c(10, 271, 634, 639, 441, 642, 442, 646, 448, 449, 288, 410, 411, 717, 718, 721, 724)
earlySpringStatus[ earlySpringStatus$ID %in% bad , ] #### as long as the spring status is 0 it should be ok, just confirming ones that died in the fall

missing = checkForMissing( earlySpringUpdate$ID, active$ID ) 

missingInfo = list()

for(i in 1:length(missing)) { 
  res = dbSendQuery( db, 'SELECT * FROM plants WHERE ID == ?', list(missing[i]))
  missingInfo[[i]] = fetch(res, -1)
  dbClearResult(res) 
}

do.call(rbind, unique( missingInfo )) ##### missing the class 5s, 
                                      ##### 771 is a new seedling is a class 6 and needs to be added to the datasheet 
                                      ###### watch for 678 a class 4 in later updates 

newSeedling = earlySpringUpdate[ earlySpringUpdate$field_tag == 771, ]
newSeedling$ID = 1087
newSeedling$status = 1 
earlySpringUpdate = rbind( earlySpringUpdate, newSeedling)

dbWriteTable(db, name = 'status', value = earlySpringUpdate, 
             append = TRUE, row.names = FALSE)

res = dbSendQuery( db, "SELECT ID, field_tag, date, status FROM status WHERE date(date) > date('2013-09-01') 
                   AND date(date) < date('2014-01-01')")
fallStatus = fetch(res, -1)
dbClearResult(res)

res = dbSendQuery( db, "SELECT ID, field_tag, date, status FROM status WHERE date(date) > date('2014-01-01') 
                   AND date(date) <= date('2014-05-16')")
springStatus = fetch(res, -1)
dbClearResult(res)

WinterChange = merge(fallStatus, springStatus, by = 'ID')
WinterChange[ WinterChange$status.y == 0 & WinterChange$status.x !=0, ] 
WinterChange[ WinterChange$status.y == 1 & WinterChange$status.x !=1, ] #### Note which live plants were supposed to be dead

res = dbSendQuery( db, "SELECT ID, field_tag, date FROM status WHERE date(date) > date('2014-01-01') 
                   AND date(date) <= date('2014-05-16') AND status = 0")
wintersDead = fetch(res, -1)
dbClearResult(res)

for(i in 1:nrow(wintersDead)){ 
  ID = wintersDead[i, 'ID']
  date = wintersDead[i, 'date']
  res = dbSendQuery( db, "UPDATE plants SET active = 0, end_date = ? WHERE ID = ? AND active = 1", 
                     list(date, ID))
  dbClearResult(res)
}


dbDisconnect(db)            # Close connection
