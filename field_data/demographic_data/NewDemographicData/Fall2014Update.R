#### update Plants table and status table from Fall 2014 Data
#### 
####
rm(list = ls())
library(RSQLite)
setwd('~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/')

fallStatus = read.csv('2014_FallData.csv')
names(fallStatus)

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

fallStatusUpdate[ fallStatusUpdate$ID == 832, ] 

fallDeadUpdate = subset( fallStatusUpdate, status == 0)

db = dbConnect(SQLite(), '../sage.sqlite')

dbWriteTable(db, name = 'status', value = fallStatusUpdate, append = TRUE, row.names = FALSE)

res = dbSendQuery(db, 'SELECT * FROM status WHERE ID = 832')
testPlant = fetch(res, -1)
dbClearResult(res)
testPlant

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
