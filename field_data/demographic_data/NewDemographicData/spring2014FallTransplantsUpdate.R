#### update Plants table and status table from 
#### early spring transplant data
#### open sage.sqlite
#### update plants table active and end_date columns

rm(list = ls())
library(RSQLite)
source( 'check_db_functions.R' )

earlySpringTransplants = read.csv('2014_Early_Spring_Fall_Transplants_Update.csv')
names(earlySpringTransplants)

origin = '1899-12-30'
earlySpringTransplants$date = as.Date(earlySpringTransplants$date, origin = origin)
earlySpringTransplants$spring = as.Date(earlySpringTransplants$spring, origin = origin)

fallTransplantsUpdate = data.frame(earlySpringTransplants[, c('spring', 'tag')], c1 = NA, c2 = NA, ch = NA, 
                                canopy = NA, infls = NA, lv_stems = NA, dd_stems = NA, stem_d1 = NA, stem_d2 = NA, 
                                earlySpringTransplants[, c('status', 'notes', 'herbivory')])

names(fallTransplantsUpdate)[ c(1, 2)] <- c('date','field_tag') #### standard names for headers 

fallTransplantsUpdate$date = strftime(fallTransplantsUpdate$date)

db = dbConnect(SQLite(), dbname = '../sage.sqlite')

res = dbSendQuery( db, "SELECT ID, tag1 FROM plants WHERE class = 5")
class5s = fetch(res, -1)
dbClearResult(res)
class5s
dbListFields(db, 'status')

fallTransplantsUpdate = merge(class5s, fallTransplantsUpdate, by.x = 'tag1', by.y = 'field_tag')
names(fallTransplantsUpdate)[1] <- 'field_tag'
names(fallTransplantsUpdate)
fallTransplantsUpdate = cbind(ID = fallTransplantsUpdate$ID, date = fallTransplantsUpdate$date, 
                              field_tag = fallTransplantsUpdate$field_tag, fallTransplantsUpdate[, -c(1:3)])
names(fallTransplantsUpdate)
head(fallTransplantsUpdate)
nrow(fallTransplantsUpdate)

fallTransplantsUpdate$herbivory[  is.na( fallTransplantsUpdate$herbivory  ) ]  <- 0
fallTransplantsUpdate = fallTransplantsUpdate[ !is.na(fallTransplantsUpdate$status),  ] ### drop status NA
fallTransplantsUpdate[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] <- as.numeric( unlist( fallTransplantsUpdate[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] ))

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


dbWriteTable(db, name = 'status', value = fallTransplantsUpdate, 
             append = TRUE, row.names = FALSE)

res = dbSendQuery( db, "SELECT ID, field_tag, date FROM status WHERE date(date) > date('2014-01-01') AND date(date) >= date('2014-05-16') 
                   AND status = 0")
wintersDead = fetch(res, -1)
dbClearResult(res)

for(i in 1:nrow(wintersDead)){ 
  ID = wintersDead[i, 'ID']
  date = wintersDead[i, 'date']
  res = dbSendQuery( db, "UPDATE plants SET active = 0, end_date = ? WHERE ID = ? AND active = 1 AND class = 5", 
                     list(date, ID))
  dbClearResult(res)
}


dbDisconnect(db)            # Close connection
