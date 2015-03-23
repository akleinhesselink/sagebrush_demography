#### update Plants table and status table from Spring 2014 Data
#### 
#### This script runs the first five scripts first to initialize the database 
#### It then adds the data from late spring 2014 
####


rm(list = ls())
library(RSQLite)

setwd('~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/')

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

db = dbConnect(SQLite(), '../sage.sqlite')

springStatusUpdate[ springStatusUpdate$ID == 81, 'ch' ] <- NA
springStatusUpdate[ springStatusUpdate$ID == 81,  ] 

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

res = dbSendQuery(db, "UPDATE status SET stem_d1 = NULL WHERE (status != 1 OR stem_d1 = 0)")
dbClearResult(res)

res = dbSendQuery(db, "UPDATE status SET ch = NULL WHERE (status != 1 OR ch = 0)")
dbClearResult(res)

res = dbSendQuery(db, "UPDATE status SET c1 = NULL WHERE (status != 1 OR c1 = 0)")
dbClearResult(res)

res = dbSendQuery(db, "UPDATE status SET c2 = NULL WHERE (status != 1 OR c1 = 0)")
dbClearResult(res)

dbDisconnect(db)            # Close connection
