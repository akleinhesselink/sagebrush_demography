rm(list = ls())
library(RSQLite)

source( 'check_db_functions.R')

firstStatus = read.csv('2012_Fall_to_2013_SpringStatus.csv')

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

statusTypes = sapply(firstStatus, class)

statusTypes[1:length(statusTypes)] <- c('int', 'text', 'int', rep('real', 9), 
                                        'int', 'character', 'int')

firstStatus$canopy <- as.numeric(firstStatus$canopy)
firstStatus$herbivory[ is.na( firstStatus$herbivory) ] <- 0 
firstStatus$infls <- as.numeric(firstStatus$infls)
firstStatus[ is.na( firstStatus$infls ) , 'infls'] <- 0 

###### connect the database 
db = dbConnect(SQLite(), dbname = 'sage.sqlite')

lastDate = max(firstStatus$date)
res = dbSendQuery( db, "SELECT * FROM plants WHERE active = 1 AND start_date <= ?", list(lastDate ))
active = fetch( res, -1)
dbClearResult( res )

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

graphics.off( )

firstStatus$notes = as.character(firstStatus$notes)
firstStatus$notes[ firstStatus$ID == 149 ]<- c('fall 2012 measurement may include separate plant nearby', '')


dbWriteTable(db, name = 'status', field.types = as.list(statusTypes), 
             value = firstStatus, row.names = FALSE, overwrite = TRUE)

res = dbSendQuery( db, "SELECT ID, field_tag, date FROM status WHERE date(date) > date('2013-04-01') AND 
                   date(date) < date('2013-08-01') AND (status = 0 OR ID = 69 OR ID = 632 OR ID = 733 OR ID = 800)") #### Mark reused tags or plants that were dug up as inactive  
springDeadUpdate = fetch(res)
dbClearResult(res)

for(i in 1:nrow(springDeadUpdate)){ 
  ID = springDeadUpdate[i, 'ID']
  date = springDeadUpdate[i, 'date']  
  res = dbSendQuery( db, "UPDATE plants SET active = 0, end_date = ? WHERE ID = ? AND active = 1", 
                     list(date, ID))
  dbClearResult(res)
}


dbDisconnect(db)            # Close connection
