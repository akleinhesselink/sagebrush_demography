##### make infls table in database 

rm(list = ls())
library(xlsx)
library(RSQLite)
library(ggplot2)
library(reshape2)

setwd('~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/')

Fall2014 = read.xlsx2( file = '2014_FallData.xlsx', sheetIndex = 1)

flowers2014 = Fall2014[ , c('ID', 'date', 'i1', 'i2', 'i3', 'i4', 'i5', 'i6', 'i7', 'i8', 'i9', 'i10')]

flowers2014$date = as.numeric( levels( flowers2014$date)[flowers2014$date ] )
flowers2014$date = strftime(as.Date(flowers2014$date, origin = '1899-12-30'))

head( flowers2014)
flowers2014[ , c(3:12)] <- lapply ( as.list(flowers2014[, c(3:12)]), FUN = function(x){ as.numeric(levels(x)[x]) } )

flowers2014Long = melt(flowers2014, id.vars= c('ID', 'date'))
head(flowers2014Long)

flowersTypes = NA
flowersTypes[1:length(flowers2014Long)] <- c('int', 'char', 'char', 'int')

names( flowers2014Long) <- c("ID", "date", "infls", "length")

flowers2014Long = flowers2014Long[ -which(is.na(flowers2014Long$length )),  ]  ##### remove missing values 

###### create the database 
db = dbConnect(SQLite(), dbname = '../sage.sqlite')

dbListTables(db)
dbListFields(db, 'flowers')

dbWriteTable(db, name = 'flowers', value = flowers2014Long, append = TRUE, row.names = FALSE)

dbDisconnect(db)

