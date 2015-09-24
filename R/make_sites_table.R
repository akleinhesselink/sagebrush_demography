source( 'R/check_db_functions.R')

sites = read.csv('field_data/site_characteristics/site_positions.csv')

sitesTypes = sapply(sites, class)

see_if( checkSiteLabels( sites$site))


###### create the database 
db = dbConnect(SQLite(), dbname = 'sage.sqlite')

dbWriteTable(db, name = 'sites', value = sites, row.names = FALSE, overwrite = TRUE)

dbDisconnect(db)            # Close connection

