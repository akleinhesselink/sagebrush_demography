source( 'R/check_db_functions.R')
source('R/dbQueryTools.R')

firstStatus = read.csv('field_data/demographic_data/2012_Fall_to_2013_SpringStatus.csv')

#### change numeric to date 
origin = '1904-01-01' ### use correct origin for microsoft excel 

firstStatus$date = strftime(as.Date(firstStatus$date, origin = origin))

firstStatus <- firstStatus [ !is.na( firstStatus$ID), ]  #### drop where ID == NA's
print( firstStatus[ is.na( firstStatus$date)]) #### print missing dates 

### round measurements less than one up to one
firstStatus [  which(firstStatus$c1 < 1) , 'c1' ]  <- 1
firstStatus [  which(firstStatus$c2 < 1) , 'c2' ]  <- 1
firstStatus [  which(firstStatus$ch < 1) , 'ch' ]  <- 1

### round small measurements 
### round 1/2 cm down 
firstStatus$c1 <- round(firstStatus$c1 - 0.01)
firstStatus$c2 <- round(firstStatus$c2 - 0.01)
firstStatus$ch <- round(firstStatus$ch - 0.01)

firstStatus$canopy <- as.numeric(firstStatus$canopy)
firstStatus$herbivory[ is.na( firstStatus$herbivory) ] <- 0 
firstStatus$infls <- as.numeric(firstStatus$infls)
firstStatus[ is.na( firstStatus$infls ) , 'infls'] <- 0 

###### connect the database 
db = dbConnect(SQLite(), dbname = 'sage.sqlite')

lastDate = max(firstStatus$date)
active = dbGetPreparedQuery( db, "SELECT * FROM plants WHERE active = 1 AND start_date <= ?", data.frame(lastDate ))

###### Run the checks 
see_if( checkStatus( firstStatus$status))
see_if( checkTags( firstStatus$ID, na.rm = FALSE))
see_if( checkTags( firstStatus$field_tag, na.rm = FALSE))
see_if( checkDate( firstStatus$date, na.rm= FALSE ))
see_if( checkHerbivory ( firstStatus$herbivory ))
see_if( checkPositiveRange ( firstStatus$ch, upper.limit= 200))
see_if( checkPositiveRange ( firstStatus$c1, upper.limit = 200))
see_if( checkPositiveRange( firstStatus$c2, upper.limit = 200))
see_if( checkPositiveRange( firstStatus$stem_d1, upper.limit = 100))
see_if( checkPositiveRange( firstStatus$stem_d2, upper.limit = 100))
see_if( checkPositiveRange( firstStatus$canopy, upper.limit = 150))
see_if( checkPositiveRange( firstStatus$infls, upper.limit = 900))
see_if( checkAllMonths( firstStatus$date[ which( firstStatus$infls > 0 )], early= 9, late = 11))

Bad = checkActive( x=firstStatus$ID , active= active$ID)
Bad

missing = checkForMissing( firstStatus$ID, active = active$ID)
missing

old = subset(firstStatus, as.Date(date) < '2013-01-01')
new = subset(firstStatus, as.Date( date) > '2013-01-01')
old$area <- (old$c1/2)*(old$c2/2)*pi
new$area <- (new$c1/2)*(new$c2/2)*pi
old = merge(active[,c('site', 'species', 'class', 'ID') ], old, by = 'ID')

statusChangeReport( old = old, new = new )

showSizeDiff( old = old, new = new, measure= 'ch')
showSizeDiff( old = old, new = new, measure = 'area') #### plant 149 may be too big in the fall

showSizeDiff( old = old[ old$class == 1, ], new = new, measure = 'stem_d1')

firstStatus$notes = as.character(firstStatus$notes)
firstStatus$notes[ firstStatus$ID == 149 ]<- c('fall 2012 measurement may include separate plant nearby', '')

dbGetQuery(db, "CREATE TABLE status
                (
                ID INTEGER,
                date DATE, 
                field_tag INTEGER, 
                c1 REAL,
                c2 REAL,
                ch REAL,
                canopy REAL, 
                infls INTEGER, 
                lv_stems INTEGER, 
                dd_stems INTEGER, 
                stem_d1 REAL, 
                stem_d2 REAL, 
                status INTEGER,
                notes TEXT, 
                herbivory INTEGER,
                CONSTRAINT obs_pk PRIMARY KEY (ID, date) );")

dbWriteTable(db, name = 'status', value = firstStatus, row.names = FALSE, overwrite = TRUE)

dbGetQuery( db, "UPDATE plants SET end_date = NULL")

exceptions = c(69, 632, 733, 800) ## these plants are status 2 in the spring and tags were removed, mark active = 0 

early_date = as.Date( min( firstStatus$date[ firstStatus$date > '2013-01-01' ]  ) ) - 1   #### first date in 2013 

dbGetQuery( db, q.update.end_date ) #, rep( strftime( early_date ), 2 )  )

dbGetPreparedQuery( db, makeExceptionalUpdateQuery( exceptions ), as.data.frame(matrix(rep(exceptions, 2), nrow = 1)))

dbGetQuery( db, q.update.active )

dbGetPreparedQuery(db, "SELECT * FROM plants WHERE end_date > ?" , data.frame(early_date))

dbDisconnect(db)            # Close connection
