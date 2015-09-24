rm(list = ls())
library(xlsx)
library(RSQLite)
library(plyr)

db = dbConnect(SQLite(), '../sage.sqlite')

res = dbSendQuery( db, "SELECT status, notes FROM status WHERE notes LIKE '%left_tag%'")
statusNotes = fetch(res, -1)
dbClearResult(res)

res = dbSendQuery( db, "SELECT site, transect, Y, X, plants.ID, tag1, tag2, species, class, treatment, status, date, status.notes, plants.notes 
                          FROM plants 
                          JOIN status ON status.ID = plants.ID 
                            WHERE (status.date > date('2014-09-01') AND active = 1 OR status.notes LIKE '%left_tag%')")

activePlants = fetch(res, -1)
dbClearResult(res)

head(activePlants)
tail(activePlants)
names(activePlants) [ grep('notes', names(activePlants) )  ] <- c('notes.1', 'notes.2')

table(activePlants$site, activePlants$class, activePlants$status)

active = arrange(activePlants, site, transect, Y)
head(active)
dbDisconnect(db)

active = active[ , c('tag1', 'tag2', 'site', 'transect', 'Y', 'X', 'ID', 'species', 'class', 'treatment', 'status', 'date', 'notes.1', 'notes.2') ]

active[ is.na( active$tag2 ) , 'tag2' ]  <- ''
head( active ) 

write.xlsx(active, file= file.path( '../../data_sheets',  '2015_SpringDataSheet.xlsx' ) , row.names= FALSE)
