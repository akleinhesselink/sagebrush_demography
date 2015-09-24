#####
##### Start checking data 
#####

rm(list = ls())
library(RSQLite)
library(plyr)
library(lme4)

source('demoPlotFunctions.R')
source('aggDemoDataFunctions.R')

db = dbConnect(SQLite(), 'sage.sqlite')

res = dbSendQuery(db, "SELECT * FROM plants")
plants = fetch(res, -1)
dbClearResult(res)

res = dbSendQuery( db, "SELECT * FROM sites")
sites = fetch(res, -1)
dbClearResult(res)

res = dbSendQuery( db, "SELECT * FROM flowers")
flowers = fetch(res, -1)
dbClearResult(res)

res = dbSendQuery( db, "SELECT status.ID, date, status, herbivory, c1, c2, ch, canopy, infls, stem_d1, status.notes 
                   FROM status JOIN plants ON status.ID = plants.ID WHERE class < 4" ) 
naturals = fetch(res, -1)
dbClearResult(res)

dbDisconnect(db)


plant_sites = merge( plants, sites, by = 'site')

head( plant_sites  ) 

length( unique( plant_sites$ID ) )  ##### Total number of plants tagged 

table( plant_sites$site, plant_sites$class, plant_sites$species )

table( plant_sites$species )

ggplot(plant_sites, aes( x = as.factor(site_no), y = ..count.. , group = species, fill = species )) + 
  geom_bar(position = 'dodge') + facet_grid ( class ~ . )

subset(plant_sites , site_no > 7 & species == 'ATP' & class < 4) ##### This plant should be changed to atv
subset(plant_sites , site_no %in% c(1,2,3,5) & species == 'ATV' & class < 4) ##### This plant should be changed to atv


ggplot( naturals, aes( x = date, y = ch, group = ID )) + geom_line() 

head(naturals)
naturals$year = strftime( x= naturals$date, format='%Y')
naturals$doy = strftime ( x = naturals$date, format = '%j')

ggplot( naturals, aes( x = as.numeric( doy), y = ..count.. )) + 
  facet_wrap( ~ year, ncol= 1 ) + 
  stat_bin(binwidth= 2) + 
  geom_vline( xintercept = c(152, 274)) 

flowers$date = as.Date(flowers$date)
naturals$date = as.Date(naturals$date)

naturals$area = pi*(naturals$c1/2) * (naturals$c2/2)

####################################################
seasons = c( T1 = as.Date('2012-01-01'), T2013.1 = as.Date('2013-01-01'),  
             T2013.2 = as.Date('2013-08-01'), T2014.1 = as.Date('2014-01-01'), 
             T2014.2 = as.Date('2014-08-01'), T2015.1 = as.Date('2015-01-01'))

naturals = genTransitionTable(demodf = naturals, breaks = seasons)
seasonalGroups = aggStatus( demodf= naturals )

head( naturals ) 
head( seasonalGroups)

aggregate(date ~ transition, naturals , FUN = 'median')
as.Date( '2014-10-21') - as.Date('2014-05-20') #### difference in days between Spring 2014 and Fall 2014

###### assign num days for each transition #################
seasonalGroups$ndays[ seasonalGroups$transition == 'T1' ] <- 232
seasonalGroups$ndays[ seasonalGroups$transition == 'T2013.1' ] <- 132
seasonalGroups$ndays[ seasonalGroups$transition == 'T2013.2' ] <- 205
seasonalGroups$ndays[ seasonalGroups$transition == 'T2014.1' ] <- 154
seasonalGroups$ndays[ seasonalGroups$transition == 'T2015.1'] <- 220
#############################################################

change_df = genChangeDF(seasonalGroupsDF= seasonalGroups)
head(change_df)

################# Relative growth by 30 days/mo ##############
change_df$rgr = 30*(log(change_df$area.1) - log(change_df$area))/change_df$ndays #### monthly relative growth
change_df$height.g = 10*(30*(change_df$ch.1 - change_df$ch)/change_df$ndays) #### monthly height growth mm/mo.
change_df$stemRGR = 30*(log(pi*(change_df$stem_d1.1/2)^2) - log(pi*(change_df$stem_d1/2)^2))/change_df$ndays #### stem area RGR 
change_df$canopyRGR = 30*(log(change_df$canopy.1) - log(change_df$canopy) )/change_df$ndays ###### canopy change
##############################################################

plantSites = merge(plants[, c('ID', 'site', 'treatment', 'species', 'class', 'date_treated')], sites, by = 'site')

plantSites$date_treated

plot_df = merge(change_df, plantSites[, c('ID','species', 'class', 'treatment','site', 'site_no', 'ele', 'date_treated')], by = 'ID')

table( plot_df$date_treated ) 

plot_df$treatment[ is.na(plot_df$treatment)  ]  <- 'control'
plot_df[ plot_df$transition.1 == 'T2013.1' , 'treatment' ] <- 'control'

plot_df[ which(plot_df$transition.1 == 'T2014.1' & plot_df$date_treated > as.Date('2014-04-01')), 'treatment' ] <- 'control'

plot_df$transition.1 = as.factor(plot_df$transition.1)

plot_df$seasonLabel = factor(plot_df$transition.1, labels = c('Fall 2012 to Spring 2013', 
                                                              'Spring 2013 to Fall 2013', 
                                                              'Fall 2013 to Spring 2014', 
                                                              'Spring 2014 to Fall 2014')) 

plot_df$Season[ plot_df$transition.1 == 'T2013.1'] <- 'Winter'
plot_df$Season[ plot_df$transition.1 == 'T2013.2'] <- 'Summer'
plot_df$Season[ plot_df$transition.1 == 'T2014.1'] <- 'Winter'
plot_df$Season[ plot_df$transition.1 == 'T2014.2'] <- 'Summer'

plot_df$speciesLab = factor(plot_df$species, labels = c('A.tripartita', 'A.tridentata vaseyana'))

