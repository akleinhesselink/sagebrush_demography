#### update Plants table and status table from early Spring Data
#### open sage.sqlite
#### read in 2014_EarlySpring_update
#### update plants table active and end_date columns
#### trick here will be going back and changing status of 
#### fall plants with status 2 or 3.  These will be changed
#### to 0 if the plant is a 0 in the spring of 2014.


rm(list = ls())
library(xlsx)
library(RSQLite)

setwd('~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/')


earlySpringStatus = read.xlsx2('2014_Early_Spring_Update.xlsx', sheetIndex= 1 )

earlySpringStatus$date2 = as.Date(as.numeric(levels(earlySpringStatus$date2))[earlySpringStatus$date2], origin = '1899-12-30')


earlySpringUpdate = data.frame( earlySpringStatus[, c('ID', 'date2', 'TAG')], c1 = NA, c2 = NA, ch = NA, 
                                canopy = NA, infls = NA, lv_stems = NA, dd_stems = NA, stem_d1 = NA, stem_d2 = NA, 
                                earlySpringStatus[, c('Spring.Status', 'Spring.Notes', 'herbivory')])

names(earlySpringUpdate)[ c(2, 3, 13, 14)] <- c('date','field_tag', 'status', 'notes') #### standard names for headers 

earlySpringUpdate$date = strftime(earlySpringUpdate$date)

earlySpringUpdate[ earlySpringUpdate$status == 0, ]

earlySpringUpdate

db = dbConnect(SQLite(), dbname = '../sage.sqlite')

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

wintersDead

for(i in 1:nrow(wintersDead)){ 
  ID = wintersDead[i, 'ID']
  date = wintersDead[i, 'date']
  res = dbSendQuery( db, "UPDATE plants SET active = 0, end_date = ? WHERE ID = ? AND active = 1", 
                     list(date, ID))
  dbClearResult(res)
}


dbDisconnect(db)            # Close connection
