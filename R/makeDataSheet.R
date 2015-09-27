rm(list = ls())
library(RSQLite)
library(plyr)

db <- dbConnect(SQLite(), 'sage.sqlite')

status_notes <- dbGetQuery( db, "SELECT status, notes FROM status WHERE notes LIKE '%left_tag%'")
head(status_notes)

dbListTables(db) 
dbListFields(db, 'sites')
dbListFields(db, 'status')
dbListFields(db, 'plants')

active <- dbGetQuery( db, "SELECT tag1, plants.site, transect, Y, X, treatment, class, species, status AS last_status, status.date AS last_date, plants.notes AS plants_notes, status.notes AS status_notes, plants.ID, ch, c1, c2, canopy
                  FROM plants 
                  JOIN sites ON plants.site = sites.site
                  JOIN status ON plants.ID = status.ID
                  WHERE (class < 7 AND active = 1 AND status.date > Date('2015-01-01')) OR status.notes LIKE '%left_tag%'
                      ORDER BY site_no, transect, Y, status.date")

head(active, 100)

table(active$site, active$class)

write.table(active, file = file.path(file = 'output/data_sheets/',  '2015_fall_datasheet.csv' ), sep = ',', row.names= FALSE)
