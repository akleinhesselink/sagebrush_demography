#### update status table with 2013 Summer Survival monitoring 
#### tricky to match plants in plants table with the
#### current tag number.  New tags do not correspond to IDs
#### and sometimes plants switch to a new  tag #

source('R/2013_spring_status_update.R')

raw_mid_summer = read.csv("field_data/demographic_data/2013_summer_survival_update.csv")

raw_mid_summer$status = as.character( raw_mid_summer$status) 
raw_mid_summer[ raw_mid_summer$status == 'u', 'status'] <- 3
raw_mid_summer$status = as.numeric(raw_mid_summer$status)
raw_mid_summer$date = as.Date ( as.character(raw_mid_summer$date), format= '%m/%d/%y')

db = dbConnect(SQLite(), dbname = 'sage.sqlite')

#### Look up ID's for status update: 
originalPlants = dbGetQuery(db, 'SELECT ID, tag1, tag2, tag_switched, end_date FROM plants WHERE date(start_date) < date("2013-08-01")')

switched = subset(originalPlants, tag_switched < as.Date('2013-09-01'))

LiveOriginals = subset(originalPlants, is.na(end_date)|end_date > as.Date('2013-08-01') )

raw_mid_summer$ID = NA
raw_mid_summer[ which( raw_mid_summer$field_tag == 735 & raw_mid_summer$status == 0) ,  'ID'] <- 735 #### ID for the first one
raw_mid_summer[ which( raw_mid_summer$field_tag == 735 & raw_mid_summer$status == 1), 'ID']   <- 646  #### ID for switched tag
raw_mid_summer[ which( raw_mid_summer$field_tag == 685) , 'ID'] <- 687

midSummerMerge =  merge(raw_mid_summer, LiveOriginals, by.x = 'field_tag', by.y = 'tag1', all.y = TRUE)

midSummerMerge[ which( midSummerMerge$field_tag == 735 & midSummerMerge$status == 0) ,] #### ID for the first one
midSummerMerge[ which( midSummerMerge$field_tag == 735 & midSummerMerge$status == 1), ] #### ID for switched tag

midSummerMerge <- midSummerMerge[ -which( midSummerMerge$field_tag == 687), ] 

midSummerMerge = rbind(midSummerMerge, cbind(raw_mid_summer[ which( raw_mid_summer$field_tag == 685), 
                                            c('field_tag', 'date', 'status', 'notes')], 
                            ID.x = 687, ID.y = 687, tag2 = NA, tag_switched = NA, end_date = NA))

midSummerMerge[ is.na( midSummerMerge$ID.x), 'ID.x' ] = midSummerMerge[ is.na( midSummerMerge$ID.x), 'ID.y' ]
midSummerMerge[ is.na( midSummerMerge$status) ,  'date' ]  <- '2013-08-06'
midSummerMerge[ is.na( midSummerMerge$status), 'status'] <- 1  #### I only recorded plants that were dead at some sites, so missing plants are alive 

SummerStatusUpdate = cbind(midSummerMerge[, c('ID.x', 'date', 'field_tag') ] , c1 = NA, c2 = NA, ch = NA, canopy = NA, 
      infls = NA, lv_stems = NA, dd_stems = NA, stem_d1 = NA, stem_d2 = NA, midSummerMerge[ , c('status','notes')], herbivory = NA)

names(SummerStatusUpdate)[1] <- "ID"

SummerStatusUpdate$date = strftime( SummerStatusUpdate$date)

SummerStatusUpdate[ SummerStatusUpdate$ID == 646, ] ### double ID 646 
SummerStatusUpdate = SummerStatusUpdate[ -which(SummerStatusUpdate$field_tag == 646), ] #### drop the plant with field tag 646 because the tag was switched 

SummerStatusUpdate <- SummerStatusUpdate [ !is.na( SummerStatusUpdate$ID), ]  #### drop where ID == NA's
print( SummerStatusUpdate[ is.na( SummerStatusUpdate$date)]) #### print missing dates 

SummerStatusUpdate$c1 <- as.numeric( SummerStatusUpdate$c1 ) 
SummerStatusUpdate$c2 <- as.numeric( SummerStatusUpdate$c2 ) 
SummerStatusUpdate$ch <- as.numeric( SummerStatusUpdate$ch )
SummerStatusUpdate$canopy <- as.numeric(SummerStatusUpdate$canopy)
SummerStatusUpdate$stem_d1 <- as.numeric(SummerStatusUpdate$stem_d1)
SummerStatusUpdate$stem_d2 <- as.numeric(SummerStatusUpdate$stem_d2)
SummerStatusUpdate$herbivory[ is.na( SummerStatusUpdate$herbivory) ] <- 0 
SummerStatusUpdate$infls <- as.numeric(SummerStatusUpdate$infls)
SummerStatusUpdate[ is.na( SummerStatusUpdate$infls ) , 'infls'] <- 0 

lastDate = max(SummerStatusUpdate$date)

active = dbGetPreparedQuery( db, "SELECT *, max(date) FROM plants 
                   JOIN status USING (ID) 
                   WHERE active = 1 AND start_date <= ? AND date <= ?
                   GROUP BY ID", data.frame(lastDate, lastDate))

##### run checks 
see_if( checkPlantID( SummerStatusUpdate$ID))
see_if( checkStatus( SummerStatusUpdate$status))
see_if( checkTags( SummerStatusUpdate$field_tag, na.rm = FALSE))
see_if( checkDate( SummerStatusUpdate$date, na.rm= FALSE ))
see_if( checkHerbivory ( SummerStatusUpdate$herbivory ))
see_if( checkPositiveRange ( SummerStatusUpdate$ch, upper.limit= 200))
see_if( checkPositiveRange ( SummerStatusUpdate$c1, upper.limit = 200))
see_if( checkPositiveRange( SummerStatusUpdate$c2, upper.limit = 200))
see_if( checkPositiveRange( SummerStatusUpdate$stem_d1, upper.limit = 100))
see_if( checkPositiveRange( SummerStatusUpdate$stem_d2, upper.limit = 100))
see_if( checkPositiveRange( SummerStatusUpdate$canopy, upper.limit = 150))
see_if( checkPositiveRange( SummerStatusUpdate$infls, upper.limit = 900))
see_if( checkAllMonths( SummerStatusUpdate$date[ which( SummerStatusUpdate$infls > 0 )], early= 9, late = 11))

Bad = checkActive( x= SummerStatusUpdate$ID, active=active$ID) 
Bad

missing = checkForMissing( x = SummerStatusUpdate$ID, active = active$ID)
missing #### plants that should be marked as missing status 2 

SummerStatusUpdate[ SummerStatusUpdate$ID %in% missing , ] 

statusChangeReport( old= active, new = SummerStatusUpdate)

SummerStatusUpdate[ SummerStatusUpdate$status == 2, ]  #### watch for these IDs in future updates 
exceptions = c(640, 835 ) #### exceptions -- lost and now need to be marked active = 0 

dbWriteTable(db, name = 'status', value = SummerStatusUpdate, 
             append = TRUE, row.names = FALSE)

early_date = strftime( as.Date( min(SummerStatusUpdate$date ) ) - 1 )    #### first date in Summer 2013 update 

dbGetQuery(db, q.reborn)
dbGetQuery(db, q.now.dead)

dbGetQuery( db, q.update.end_date ) # rep(early_date, 2) )
dbGetPreparedQuery( db, makeExceptionalUpdateQuery( exceptions ), as.data.frame(matrix(rep(exceptions, 2), nrow = 1)))

dbGetQuery( db, q.update.active)

dbDisconnect(db)            # Close connection

