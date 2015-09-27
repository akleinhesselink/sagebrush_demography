# update Plants table and status table from Fall 2014 Data

source('R/2015_spring_status_update.R')

# merge status with plants table by ID 

sm_status <- read.csv('field_data/demographic_data/sm_plants_status.csv')
sm_status <- sm_status[ which(!is.na( sm_status$status) ) , ] 


db = dbConnect(SQLite(), 'sage.sqlite')

sm_plants <- dbGetQuery( db, "SELECT ID, tag1 FROM plants WHERE class = 7")

sm_status <- merge(sm_plants, sm_status)

sm_status [ is.na( sm_status$ID) , ]

sm_status[ is.na( sm_status$status), 'notes'] 

sm_status$date <- as.Date( as.character( sm_status$date )  ) 

sm_status$field_tag <- sm_status$tag1
sm_status$stem_d1 <- sm_status$stemd1

head(sm_status)

colList = dbListFields( db, name='status')

addList = colList[ !colList %in% names(sm_status)]
addList

addDF <- as.data.frame ( matrix( NA, ncol= length(addList)) ) 
names(addDF) <- addList

sm_status <- cbind( sm_status, addDF ) 
sm_status <- sm_status[ , colList]

sm_status$date = strftime(sm_status$date)

springDeadUpdate = subset( sm_status, status == 0)

sm_status$herbivory[  is.na( sm_status$herbivory  ) ]  <- 0

sm_status[ which( sm_status$status == 2 ) , ]

sm_status[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] <- as.numeric( unlist( sm_status[ , c('ch', 'c1', 'c2', 'canopy', 'stem_d1', 'stem_d2', 'infls')] ))

sm_status[ duplicated( sm_status$ID ) , ] 

#### run checks 
checkStatus (sm_status$status)

# checkPlantID ( sm_status$ID) will have duplicates 
checkTags( sm_status$field_tag, na.rm = FALSE)
checkDate( sm_status$date, na.rm= FALSE )
checkHerbivory ( sm_status$herbivory )
checkPositiveRange ( sm_status$ch, upper.limit= 200)
checkPositiveRange ( sm_status$c1, upper.limit = 200)
checkPositiveRange( sm_status$c2, upper.limit = 200)
checkPositiveRange( sm_status$stem_d1, upper.limit = 100)
checkPositiveRange( sm_status$stem_d2, upper.limit = 100)
checkPositiveRange( sm_status$canopy, upper.limit = 150)
checkPositiveRange( sm_status$infls, upper.limit = 900)
checkAllMonths( sm_status$date[ which( sm_status$infls > 0 )], early= 9, late = 11)


# Add plants to database ----------------------------------------- 
dbWriteTable(db, name = 'status', value = sm_status, append = TRUE, row.names = FALSE)

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

