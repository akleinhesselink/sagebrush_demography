#### update Plants table and status table from Fall 2014 Data
#### 
####
rm(list = ls())
library(xlsx)
library(RSQLite)
setwd('~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/')

source('spring2014Update.R') ##### Recreate the database by re-running the initiation scripts 


fallStatus = read.xlsx2('2014_FallData.xlsx', sheetIndex= 1 , startRow=1)
names(fallStatus)

head( fallStatus[, 1:15])
head( fallStatus[ is.na(fallStatus$date), ] ) 

fallStatus$date = as.Date(as.numeric(levels(fallStatus$date))[fallStatus$date], origin = '1899-12-30')
fallStatus$ID = as.integer(levels(fallStatus$ID))[fallStatus$ID]


fallStatus$TAG = as.integer(levels(fallStatus$tag1))[fallStatus$tag1]
fallStatus$canopy = as.numeric(levels(fallStatus$canopy)[ fallStatus$canopy ])
fallStatus$c1 = as.numeric(levels(fallStatus$c1)[ fallStatus$c1 ])
fallStatus$c2 = as.numeric(levels(fallStatus$c2)[ fallStatus$c2 ])
fallStatus$ch = as.numeric(levels(fallStatus$ch)[ fallStatus$ch ])
fallStatus$stem_d1 = as.numeric(levels(fallStatus$stemd1)[ fallStatus$stemd1])
fallStatus$infls = as.numeric(levels(fallStatus$infls)[ fallStatus$infls ] )

##### Need to change the class of the infls length data from factor to integer/numeric 
allInflsColumns =  grep('i[1-9]+', names(fallStatus) )  
inflsDataList = as.list( fallStatus[, allInflsColumns] ) 
fallStatus[ , allInflsColumns] = lapply( inflsDataList,   FUN = function( x ){ as.numeric( levels( x )[ x ]) } )  

str(fallStatus)

fallStatusUpdate = data.frame( fallStatus[, c('ID', 'date', 'TAG', 'c1', 'c2' , 'ch' , 'canopy', 'infls')],  
                                 lv_stems = NA, dd_stems = NA, stem_d1 = fallStatus$stem_d1, stem_d2 = NA, 
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
