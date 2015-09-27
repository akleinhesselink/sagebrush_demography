##### make infls table in database 

source('R/2015_spring_status_update.R')


Fall2013 = read.csv( file = 'field_data/demographic_data/2013_Fall_update.csv')

flowers2013 = Fall2013[ , c('ID', 'date', 'i1', 'i2', 'i3', 'i4', 'i5', 'i6', 'i7', 'i8', 'i9', 'i10')]

origin = '1899-12-30'
flowers2013$date = as.Date( flowers2013$date, origin = origin)
flowers2013$date = strftime(flowers2013$date)

flowers2013Long = reshape(flowers2013, direction= 'long', v.names= 'length', idvar='ID', 
               varying = list(3:12), times = names(flowers2013)[3:12], timevar = 'infls')

flowersTypes = NA
flowersTypes[1:length(flowers2013Long)] <- c('int', 'char', 'char', 'int')

flowers2013Long = flowers2013Long[ -which(is.na(flowers2013Long$length )),  ]  ##### remove missing values 

###### create the database 
db = dbConnect(SQLite(), dbname = 'sage.sqlite')

dbWriteTable(db, name = 'flowers', value = flowers2013Long, row.names = FALSE, overwrite = TRUE)

dbDisconnect(db)

