#####
##### Start checking data 
#####

rm(list = ls())
library(RSQLite)
library(ggplot2)

setwd('~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/')
source('demoPlotFunctions.R')
source('aggDemoDataFunctions.R')

db = dbConnect(SQLite(), '../sage.sqlite')

res = dbSendQuery(db, "SELECT * FROM plants")
plants = fetch(res, -1)
dbClearResult(res)

res = dbSendQuery( db, "SELECT * FROM sites")
sites = fetch(res, -1)
dbClearResult(res)

res = dbSendQuery( db, "SELECT status.ID, date, status, herbivory, c1, c2, ch, stem_d1, canopy, status.notes 
                   FROM status JOIN plants ON status.ID = plants.ID WHERE class = 4 AND date > Date('2013-05-24')" ) 
transplants = fetch(res, -1)
dbClearResult(res)

dbDisconnect(db)

plantSites = merge(plants[, c('ID', 'site', 'treatment', 'species', 'date_treated')], sites, by = 'site')

transplants$date = as.Date(transplants$date)
transplants$area = pi*(transplants$c1/2) * (transplants$c2/2)

############## Add a planting date 
start = data.frame( ID = unique(transplants$ID), date = '2013-05-25', status = 1, notes = 'planted') ##### make a data frame giving status 1 plants upon transplants
transplants =  merge(transplants, start, by = c('ID', 'date', 'status', 'notes'), all = TRUE) 
transplants$herbivory[  is.na( transplants$herbivory )   ] <- 0 
##### 

seasons = c( T1 = as.Date('2013-01-01'), 
             T2013.1 = as.Date('2013-06-01'),  
             T2013.2 = as.Date('2013-08-01'), 
             T2014.1 = as.Date('2014-01-01'), 
             T2014.2 = as.Date('2014-08-01'))

transplants = genTransitionTable(demodf=transplants, breaks = seasons)

seasonalGroups = aggStatus( demodf= transplants)

###### assign num days for each transition #################
seasonalGroups$ndays[ seasonalGroups$transition == 'T1' ] <- 30
seasonalGroups$ndays[ seasonalGroups$transition == 'T2013.1' ] <- 132
seasonalGroups$ndays[ seasonalGroups$transition == 'T2013.2' ] <- 205
seasonalGroups$ndays[ seasonalGroups$transition == 'T2014.1' ] <- 154
#############################################################

change_df = genChangeDF(seasonalGroupsDF= seasonalGroups)

################# Relative growth by 30 days/mo ##############
change_df$rgr = 30*(log(change_df$area.1) - log(change_df$area))/change_df$ndays #### monthly relative growth
change_df$height.g = 10*(30*(change_df$ch.1 - change_df$ch)/change_df$ndays) #### monthly height growth mm/mo.
change_df$stemRGR = 30*(log(pi*(change_df$stem_d1.1/2)^2) - log(pi*(change_df$stem_d1/2)^2))/change_df$ndays #### stem area RGR 
##############################################################
plot_df = merge(change_df, plantSites[, c('ID','species', 'treatment','site', 'site_no', 'ele', 'date_treated')], by = 'ID')

plot_df$treatment[ is.na(plot_df$treatment)  ]  <- 'control'
plot_df[ plot_df$transition.1 == 'T2013.1' , 'treatment' ] <- 'control'

plot_df$stemSizeSD = plot_df$stem_d1
plot_df$stemSizeSD[ is.na( plot_df$stemSizeSD ) ] <- plot_df$stem_d1.1[ is.na(plot_df$stemSizeSD) ]

##### survival x elevation plots
plot_df$transition.1 = as.factor(plot_df$transition.1)
plot_df$treatment = as.factor(plot_df$treatment)

plot_df$seasonLabel = factor(plot_df$transition.1, labels = c('Spring 2013 (first month)', 
                                                              'Spring 2013 to Fall 2013', 
                                                              'Fall 2013 to Spring 2014', 
                                                              'Spring 2014 to Fall 2014')) 

plot_df$speciesLab = factor(plot_df$species, labels = c( 'A.tripartita', 'A.tridentata vaseyana'))

#################### plot labels #############################
xlabEle = "Elevation (m)"
xlabSize = expression('Plant area log(cm'^2*')')
ylabRGR = expression( 'Monthly relative growth' ~ '(log area'[t] - 'log area'[t-1] * ')/mos.' ) 
ylabStemRGR = expression( 'Monthly relative stem growth' ~ '(log stem area'[t] - 'log stem area'[t-1] * ')/mos.' ) 
ylabHeightGrowth = "Height growth (mm/mo.)"
##############################################################

#################### Survival plot ###########################

p = survElevationPlot(plotdf = plot_df, xlab = xlabEle)
p 
atp = subset(plot_df, species == 'ATP' & transition.1 != 'T2013.1')
atv = subset(plot_df, species == 'ATV' & transition.1 != 'T2013.1')

################# Survival by size ############################
survSizePlot(plotdf= subset( plot_df, transition.1 != 'T2013.1'), xlab = xlabSize)

################# Risk of herbivory ########################### 
ggplot( subset( plot_df, transition.1 != 'T2013.1'), aes( x = ele, y = herbivory.1, group = treatment, color = treatment)) + 
  geom_point() + geom_smooth( method = 'glm', family = 'binomial', se = FALSE) +   
  facet_grid(species ~ transition.1)

m1 = glm( survival ~ herbivory.1 + stemSizeSD, atp, family = 'binomial')
summary(m1)

ggplot( atp, aes( x = stemSizeSD, y = survival, color = factor( herbivory.1))) + geom_point() + 
  geom_smooth(method = 'glm', family = 'binomial')

m1 = glm( survival ~ herbivory.1 + stemSizeSD, atv, family = 'binomial')
summary(m1)

ggplot( atv, aes( x = stemSizeSD, y = survival, color = factor( herbivory.1))) + geom_point() + 
  geom_smooth(method = 'glm', family = 'binomial')


################### RGR plots ################################ 
p = RGRElevationPlot( subset( plot_df, transition.1 != 'T2013.1'), xlab = xlabEle, ylab = ylabRGR)
p

###### plot omit herbivory  
ggplot( subset(plot_df, transition.1 != 'T2013.1' & herbivory.1 == 0) , aes( x = ele, y = rgr, color = treatment)) + 
  geom_point()  + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_grid(species ~ transition.1)


################# stem RGR ####################################
stemRGRElevationPlot(subset(plot_df, transition.1 != 'T2013.1'), xlab =xlabEle, ylab = ylabStemRGR )

###### plot omit herbivory  
ggplot( subset(plot_df, transition.1 != 'T2013.1' & herbivory.1 == 0) , aes( x = ele, y = stemRGR, color = treatment)) + 
  geom_point()  + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_grid(species ~ transition.1)

################## canopy height growth #######################
heightGrowthElevationPlot(plotdf = subset(plot_df, transition.1 != 'T2013.1'), xlab = xlabEle, ylab = ylabHeightGrowth)


###### plot omit herbivory  
ggplot( subset(plot_df, transition.1 != 'T2013.1' & herbivory.1 == 0) , aes( x = ele, y = height.g, color = treatment)) + 
  geom_point()  + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_grid(species ~ transition.1)

##### Extra plots ################################################
ggplot( subset ( plot_df, herbivory.1 == 0) , aes( x = species, fill = treatment, y = rgr  )) + 
  geom_boxplot( position = 'dodge') + facet_grid(. ~ transition.1)

##### 
transplants$area[ transplants$area == -999 ] <- NA
transplants$ch [ transplants$ch == -999] <- NA

transplants$ID = as.factor( transplants$ID ) 
spagPlot = transplants [ -which ( is.na(transplants$area) ) , ]
spagPlot = merge( spagPlot, plants[ , c('ID', 'treatment', 'site', 'species', 'date_treated') ], by = 'ID')
spagPlot[ which( as.Date( spagPlot$date )  <=  as.Date( spagPlot$date_treated)  ),  'treatment'] <- "control"

ggplot( spagPlot, aes(x = date, y =  area  , group = ID, color = treatment )) + 
  geom_point( ) + geom_line() + facet_grid( . ~ species) 
