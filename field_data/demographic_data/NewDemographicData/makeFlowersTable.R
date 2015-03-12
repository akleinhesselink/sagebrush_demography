##### make infls table in database 

rm(list = ls())
library(xlsx)
library(RSQLite)
library(ggplot2)

setwd('~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/')

CC = c('character', 'numeric', 'numeric', 'numeric', 'character', 'integer', 'integer', 'integer', 'integer', 'numeric',
       'numeric', 'numeric', 'numeric', 'numeric', rep('numeric', 11), 'character', 'numeric', 'numeric', 'numeric', 'numeric')

Fall2013 = read.xlsx2( file = '2013_Fall_update.xlsx', colClasses= CC, sheetIndex=1)

flowers2013 = Fall2013[ , c('ID', 'date', 'i1', 'i2', 'i3', 'i4', 'i5', 'i6', 'i7', 'i8', 'i9', 'i10')]

flowers2013$date = as.numeric( flowers2013$date )
flowers2013$date = strftime(as.Date(flowers2013$date, origin = '1899-12-30'))

flowers2013Long = reshape(flowers2013, direction= 'long', v.names= 'length', idvar='ID', 
               varying = list(3:12), times = names(flowers2013)[3:12], timevar = 'infls')

flowersTypes = NA
flowersTypes[1:length(flowers2013Long)] <- c('int', 'char', 'char', 'int')
flowers2013Long = flowers2013Long[ -which(is.nan(flowers2013Long$length )),  ]  ##### remove missing values 

###### create the database 
db = dbConnect(SQLite(), dbname = '../sage.sqlite')

dbWriteTable(db, name = 'flowers', value = flowers2013Long, row.names = FALSE, overwrite = TRUE)

dbDisconnect(db)



