#### update Plants table and status table from Fall 2014 Data
#### 
####
rm(list = ls())
library(RSQLite)
source( 'check_db_functions.R')

springStatus = read.csv('2015_SpringData.csv')

springStatus [ is.na( springStatus$ID) , ]

springStatus[ is.na( springStatus$status), 'notes'] 

springStatus$date = as.Date( springStatus$date, format ='%m/%d/%y') 

springStatus$field_tag = springStatus$tag2
springStatus$field_tag[ is.na( springStatus$field_tag ) ] <- springStatus$tag1[ is.na( springStatus$field_tag ) ] 

db = dbConnect(SQLite(), 'sage.sqlite')

colList = dbListFields( db, name='status')
colList

addList = colList[ !colList %in% names(springStatus)]
addList

addDF <- as.data.frame ( matrix( NA, ncol= length(addList)) ) 
names(addDF) <- addList

springStatus <- cbind( springStatus, addDF ) 
springStatus <- springStatus[ , colList]

springStatus$date = strftime(springStatus$date)

springDeadUpdate = subset( springStatus, status == 0)

springStatus$herbivory[  is.na( springStatus$herbivory  ) ]  <- 0

springStatus[ is.na( springStatus$ID ) , ] 

springStatus[ which( springStatus$status == 2 ) , ]

springStatus = springStatus[ !is.na( springStatus$status ),  ] #### drop those with NA for status

names(springStatus)[ which( names( springStatus) == 'TAG' ) ]  <- 'field_tag'  #### standardize tag name 
springStatus[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] <- as.numeric( unlist( springStatus[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] ))

springStatus[ duplicated( springStatus$ID ) , ] 

springStatus[ springStatus$ID %in% c(36, 45, 60), ]

lastDate = max(springStatus$date)
res = dbSendQuery( db, "SELECT * FROM plants WHERE active = 1 AND date( start_date) <= date( ? )" , list( lastDate))
active = fetch( res, -1)
dbClearResult( res )


#### run checks 
checkStatus (springStatus$status)
checkPlantID ( springStatus$ID)
checkTags( springStatus$field_tag, na.rm = FALSE)
checkDate( springStatus$date, na.rm= FALSE )
checkHerbivory ( springStatus$herbivory )
checkPositiveRange ( springStatus$ch, upper.limit= 200)
checkPositiveRange ( springStatus$c1, upper.limit = 200)
checkPositiveRange( springStatus$c2, upper.limit = 200)
checkPositiveRange( springStatus$stem_d1, upper.limit = 100)
checkPositiveRange( springStatus$stem_d2, upper.limit = 100)
checkPositiveRange( springStatus$canopy, upper.limit = 150)
checkPositiveRange( springStatus$infls, upper.limit = 900)
checkAllMonths( springStatus$date[ which( springStatus$infls > 0 )], early= 9, late = 11)

Bad = checkActive( springStatus$ID, active$ID )
Bad 

springStatus [ springStatus$ID %in% Bad & springStatus$status == 1 , ]  ##### these should be dead 

missing = checkForMissing( springStatus$ID, active$ID)
missing

res = dbSendQuery(db, "SELECT site, transect, Y, plants.ID, tag1, class, date, c1, c2, ch, status, status.notes, herbivory FROM plants JOIN status ON status.ID = plants.ID WHERE plants.ID IN (?) ORDER BY plants.ID, date", missing )
missing_info = fetch(res, -1)
dbClearResult(res )

missing_info #### plant 400 and plant 1093 where missing in the fall 2014
             #### plant 1118 was skipped because it wasn't on the spring 2015 datasheet -- look for it in the fall 


dbWriteTable(db, name = 'status', value = springStatus, append = TRUE, row.names = FALSE)

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

res = dbSendQuery(db, "UPDATE status SET infls = NULL WHERE (status != 1 OR c1 = 0)")
dbClearResult(res)

res = dbSendQuery(db, "UPDATE status SET herbivory = 0 WHERE herbivory != 1")
dbClearResult(res)

dbDisconnect(db)            # Close connection
