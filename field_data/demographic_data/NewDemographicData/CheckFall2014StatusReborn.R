##### Check reborn's and deaths 
##### find plants that were dead or uncertain in the spring 
###### but that were marked as alive in the fall 

##### conversely find plants that were unceratin in the spring and 
##### still undertain in the fall, consider marking as dead. 
##################################################################################
rm(list = ls())
library(xlsx)
library(RSQLite)
setwd('~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/')

db = dbConnect(SQLite(), dbname = '../sage.sqlite')

res = dbSendQuery( db, "SELECT * FROM status 
                   WHERE date(date) >= date('2014-10-01') AND status = 1")
fallLiveUpdate = fetch(res, -1)
dbClearResult(res)
fallLiveUpdate

res = dbSendQuery( db, "SELECT * FROM status 
                   WHERE date(date) >= date('2014-10-01') AND status != 1")
fallDeadUpdate = fetch(res, -1)
dbClearResult(res)
fallDeadUpdate

###### look for plants that were status 2 or 3 in the spring but alive in the fall
res = dbSendQuery( db, "SELECT * FROM status WHERE status !=1 AND date(date) <= date('2014-10-01')" )
springDead = fetch( res, -1)
dbClearResult(res)

res = dbSendQuery( db, "SELECT * FROM status WHERE status=1 AND date(date) <= date('2014-10-01')" )
springLive = fetch( res, -1)
dbClearResult(res)

reborn = fallLiveUpdate [ which(  fallLiveUpdate$ID  %in% springDead$ID ) , ] 
reborn
springDead [ which ( springDead$ID %in% fallLiveUpdate$ID ), ]

##### go into database and re-assign these plants as alive at previous sampling dates 
res = dbSendQuery( db, 'SELECT * FROM plants WHERE ID = ?', reborn$ID[2]) 
fetch(res, -1)
dbClearResult(res)

res = dbSendQuery( db, 'SELECT * from status WHERE ID = ?', reborn$ID[2])
fetch(res, -1)
dbClearResult(res)

reborn$ID[2]
##### update 
res = dbSendQuery(db, 'UPDATE status SET status = 1 WHERE ID = 596' )
dbClearResult(res)

res = dbSendQuery(db, "UPDATE status SET status = 1 WHERE ID = 678")
dbClearResult(res)

####### 
newlyDeads = fallDeadUpdate [ which( fallDeadUpdate$ID %in% springLive$ID) , ] 
head( newlyDeads)
str(newlyDeads)

####### 
fallUncertain = fallDeadUpdate[ fallDeadUpdate$status > 1 , ] 
nowDead = fallUncertain [ fallUncertain$ID %in% springDead$ID , ] 
nowDead
