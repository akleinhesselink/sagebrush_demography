#### update Plants table and status table 
#### open sage.sqlite
#### read in 2013_Fall_update
#### update plants table active and end_date columns
#### tricky to match plants in plants table with the
#### current tag number.  New tags do not correspond to IDs
#### and sometimes plants switch to a new  tag #

rm(list = ls())
library(xlsx)
library(RSQLite)

setwd('~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/')

fallStatus = read.xlsx2('2013_Fall_update.xlsx', sheetIndex= 1 )
fallTransplants = read.xlsx2('2013_NewFallTransplantsStatus.xlsx', sheetIndex=1)

fallStatus$date = as.Date(as.numeric(levels(fallStatus$date))[fallStatus$date], origin = '1899-12-30')
fallTransplants$date = as.Date(as.numeric(levels(fallTransplants$date))[fallTransplants$date], origin = '1899-12-30')

fallStatus$date = strftime(fallStatus$date)
fallTransplants$date = strftime(fallTransplants$date)
fallStatus$c1 = as.numeric(levels(fallStatus$c1))[fallStatus$c1]
fallStatus$c2 = as.numeric(levels(fallStatus$c2))[fallStatus$c2]
fallStatus$ch = as.numeric(levels(fallStatus$ch))[fallStatus$ch]
fallStatus$stem_d1 = as.numeric(levels(fallStatus$stem_d1))[fallStatus$stem_d1]
fallStatus$stem_d2 = as.numeric(levels(fallStatus$stem_d2))[fallStatus$stem_d2]
fallStatus$canopy = as.numeric(levels(fallStatus$canopy))[fallStatus$canopy]

names(fallStatus)
names(fallTransplants)

fallStatusUpdate = fallStatus[, c('ID', 'date', 'field_tag', 
                                  'c1', 'c2', 'ch', 'canopy', 'infls', 
                                  'lv_stems', 'dd_stems', 'stem_d1', 
                                  'stem_d2', 'status','notes', 'herbivory')]

fallTransplantsUpdate = fallTransplants[, c('date', 'field_tag', 'c1', 'c2', 'ch', 
                                            'canopy', 'infls', 'lv_stems', 'dd_stems', 'stem_d1', 
                                            'stem_d2', 'status', 'notes', 'herbivory')]

names(fallStatusUpdate)

db = dbConnect(SQLite(), dbname = '../sage.sqlite')


dbWriteTable(db, name = 'status', value = fallStatusUpdate, 
             append = TRUE, row.names = FALSE)


##### assign ID's to new fall transplants in STATUS table 
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

dbWriteTable(db, name = 'status', value = fallTransplantsUpdate, 
             append = TRUE, row.names = FALSE)

res = dbSendQuery( db, "SELECT ID, field_tag, date FROM status WHERE date(date) > date('2013-08-30') AND 
                   date(date) < date('2014-01-01') AND status = 0")

fallDeadUpdate = fetch(res)
dbClearResult(res)

nrow(fallDeadUpdate)

for(i in 1:nrow(fallDeadUpdate)){ 
  ID = fallDeadUpdate[i, 'ID']
  date = fallDeadUpdate[i, 'date']
  
  res = dbSendQuery( db, "UPDATE plants SET active = 0, end_date = ? WHERE ID = ? AND active = 1", 
                     list(date, ID))
  dbClearResult(res)
}

res = dbSendQuery(db, "SELECT * FROM plants WHERE date(start_date) < date('2014-01-01')")
pre2014Plants = fetch(res, -1)
dbClearResult(res)

pre2014Plants[ which( pre2014Plants$end_date > as.Date('2013-10-01') ) , ]


dbDisconnect(db)            # Close connection

