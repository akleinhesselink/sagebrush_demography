#### update status table with 2013 Summer Survival monitoring 
#### open sage.sqlite
#### read in 2013_summer_status_update
#### update plants table active and end_date columns
#### tricky to match plants in plants table with the
#### current tag number.  New tags do not correspond to IDs
#### and sometimes plants switch to a new  tag #

rm(list = ls())
library(RSQLite)

setwd('~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/')

raw_mid_summer = read.csv("2013_summer_survival_update.csv")
raw_mid_summer$status = as.character( raw_mid_summer$status) 
raw_mid_summer[ raw_mid_summer$status == 'u', 'status'] <- 3
raw_mid_summer$status = as.factor(raw_mid_summer$status)
raw_mid_summer$date = as.Date ( as.character(raw_mid_summer$date), format= '%m/%d/%y')

db = dbConnect(SQLite(), dbname = '../sage.sqlite')

#### Look up ID's for status update: 
res = dbSendQuery(db, 'SELECT ID, tag1, tag2, tag_switched, end_date FROM plants WHERE date(start_date) < date("2013-08-01")')
originalPlants = fetch(res, -1)
dbClearResult(res)

switched = subset(originalPlants, tag_switched < as.Date('2013-09-01'))
switched

LiveOriginals = subset(originalPlants, is.na(end_date)|end_date > as.Date('2013-08-01'))

raw_mid_summer$ID = NA
raw_mid_summer[ which( raw_mid_summer$field_tag == 735 & raw_mid_summer$status == 0) ,  'ID']<- 735 #### ID for the first one
raw_mid_summer[ which( raw_mid_summer$field_tag == 735 & raw_mid_summer$status == 1), 'ID']  <- 646  #### ID for switched tag
raw_mid_summer[ which( raw_mid_summer$field_tag == 685) , 'ID'] <- 687

midSummerMerge =  merge(raw_mid_summer, LiveOriginals, by.x = 'field_tag', by.y = 'tag1', all.y = TRUE)

midSummerMerge[ which( midSummerMerge$field_tag == 735 & midSummerMerge$status == 0) ,] #  'ID']<- 735 #### ID for the first one
midSummerMerge[ which( midSummerMerge$field_tag == 735 & midSummerMerge$status == 1), ] #  'ID']  <- 646  #### ID for switched tag

midSummerMerge <- midSummerMerge[ -which( midSummerMerge$field_tag == 687), ] 

midSummerMerge = rbind(midSummerMerge, cbind(raw_mid_summer[ which( raw_mid_summer$field_tag == 685), 
                                            c('field_tag', 'date', 'status', 'notes')], 
                            ID.x = 687, ID.y = 687, tag2 = NA, tag_switched = NA, end_date = NA))

tail(midSummerMerge)
nrow(midSummerMerge)

midSummerMerge[ is.na( midSummerMerge$ID.x), 'ID.x' ] = midSummerMerge[ is.na( midSummerMerge$ID.x), 'ID.y' ]
tail( midSummerMerge)

midSummerMerge[ is.na( midSummerMerge$status) ,  'date' ]  <- '2013-08-06'
midSummerMerge[ is.na( midSummerMerge$status), 'status'] <- 1

SummerStatusUpdate = cbind(midSummerMerge[, c('ID.x', 'date', 'field_tag') ] , c1 = NA, c2 = NA, ch = NA, canopy = NA, 
      infls = NA, lv_stems = NA, dd_stems = NA, stem_d1 = NA, stem_d2 = NA, midSummerMerge[ , c('status','notes')], herbivory = NA)

names(SummerStatusUpdate)[1] <- "ID"

SummerStatusUpdate$date = strftime( SummerStatusUpdate$date)

SummerStatusUpdate[ SummerStatusUpdate$ID == 646, ] ### double ID 646 
SummerStatusUpdate = SummerStatusUpdate[ -which(SummerStatusUpdate$field_tag == 646), ] #### drop the plant with field tag 646 because the tag was switched 


dbWriteTable(db, name = 'status', value = SummerStatusUpdate, 
             append = TRUE, row.names = FALSE)

dbDisconnect(db)            # Close connection
