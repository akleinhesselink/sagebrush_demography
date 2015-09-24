rm(list = ls())

require(reshape2)
require(RSQLite)
require(DBI)

Fall2014 = read.csv( file = 'field_data/demographic_data/2014_FallData.csv')

flowers2014 = Fall2014[ , c('ID', 'date', 'i1', 'i2', 'i3', 'i4', 'i5', 'i6', 'i7', 'i8', 'i9', 'i10')]

origin = '1899-12-30'
flowers2014$date = strftime(as.Date(flowers2014$date, origin = origin))

flowers2014Long = melt(flowers2014, id.vars= c('ID', 'date'))
head(flowers2014Long)

flowersTypes = NA
flowersTypes[1:length(flowers2014Long)] <- c('int', 'char', 'char', 'int')

names( flowers2014Long) <- c("ID", "date", "infls", "length")

flowers2014Long = flowers2014Long[ -which(is.na(flowers2014Long$length )),  ]  ##### remove missing values 

###### update database
db = dbConnect(SQLite(), dbname = 'sage.sqlite')

dbWriteTable(db, name = 'flowers', value = flowers2014Long, append = TRUE, row.names = FALSE)

dbDisconnect(db)

