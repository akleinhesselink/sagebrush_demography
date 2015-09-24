#### update Plants table and status table from Fall 2014 Data
#### 
####
rm(list = ls())

source( 'R/check_db_functions.R')
source( 'R/dbQueryTools.R')

springStatus = read.csv('field_data/demographic_data/2015_SpringData.csv')

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


lastDate = max(springStatus$date)
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

springStatus [ springStatus$ID %in% Bad , ]  ##### these should be dead 

missing = checkForMissing( springStatus$ID, active$ID)
missing

q = paste( "SELECT ID, site, class, date, status, ch 
           FROM plants 
           JOIN status USING (ID) 
           WHERE ID IN (", questionMarks( missing), ");")

missing_info = dbGetQuery( db, q, missing) 

missing_info  #### plant 400 and plant 1093 where missing in the fall 2014
              #### plant 1118 was skipped because it wasn't on the spring 2015 datasheet 
              #### -- look for it in the fall 

statusChangeReport( old = active, new = springStatus)

active.size$area = (active.size$c1/2)*(active.size$c2/2)*pi
springStatus$area = (springStatus$c1/2)*(springStatus$c2/2)*pi 

showSizeDiff( old= active.size , new = springStatus, measure= 'ch') ### check 175
showSizeDiff( old = active.size, new = springStatus, measure = 'stem_d1')
showSizeDiff( old = active.size, new = springStatus, measure = 'canopy') ### check 321 
showSizeDiff( old = active.size, new = springStatus, measure = 'area') #### 321 a little small

springStatus[ springStatus$ID %in% c( 321),  ]  #### see notes 

dbGetQuery( db, "SELECT ID, site, treatment, c1, c2, canopy, date, status.notes
                  FROM status JOIN plants USING (ID) 
                  WHERE ch IS NOT NULL 
                  AND ID IN (321) 
                  ORDER BY ID, date") 

springStatus[ springStatus$ID %in% 175, ] ### Typo on height? 

dbGetQuery( db, "SELECT ID, site, treatment, ch, canopy, date, status.notes
                  FROM status JOIN plants USING (ID) 
                  WHERE ch IS NOT NULL 
                  AND ID IN (175) 
                  ORDER BY ID, date") 


graphics.off()

springStatus = springStatus[ , -which( names(springStatus) == 'area' )] #### drop area 



dbWriteTable(db, name = 'status', value = springStatus, append = TRUE, row.names = FALSE)

dbGetQuery(db, "UPDATE status SET stem_d1 = NULL WHERE (status != 1 OR stem_d1 = 0)")

dbGetQuery(db, "UPDATE status SET ch = NULL WHERE (status != 1 OR ch = 0)")

dbGetQuery(db, "UPDATE status SET c1 = NULL WHERE (status != 1 OR c1 = 0)")

dbGetQuery(db, "UPDATE status SET c2 = NULL WHERE (status != 1 OR c1 = 0)")

dbGetQuery(db, "UPDATE status SET infls = NULL WHERE (status != 1 OR c1 = 0)")

dbGetQuery(db, "UPDATE status SET herbivory = 0 WHERE herbivory != 1")

reborn = dbGetQuery( db, q.reborn)
reborn
now_dead = dbGetQuery( db, q.now.dead)
now_dead

dbGetQuery( db, q.update.reborn  ) 
dbGetQuery( db, q.update.now.dead)
dbGetQuery(db, q.update.end_date)
dbGetQuery(db, q.update.active)

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
