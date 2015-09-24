#####
##### Start checking data 
#####

rm(list = ls())
library(RSQLite)

setwd('~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/')
source('demoPlotFunctions.R')
source('aggDemoDataFunctions.R')
db = dbConnect(SQLite(), '../sage.sqlite')

res = dbSendQuery(db, "SELECT * FROM plants")
plants = fetch(res, -1)
dbClearResult(res)

plants$date_treated = as.Date(plants$date_treated)

res = dbSendQuery( db, "SELECT * FROM sites")
sites = fetch(res, -1)
dbClearResult(res)

res = dbSendQuery( db, "SELECT status.ID, date, status, herbivory, c1, c2, ch, stem_d1, infls, canopy, status.notes 
                  FROM status JOIN plants ON status.ID = plants.ID WHERE class = 1" ) 
seedlings = fetch(res, -1)
dbClearResult(res)

aggregate( stem_d1 ~ date, seedlings, FUN = 'mean')

dbDisconnect(db)

seedlings$stem_d1
##### formatting 
seedlings$date = as.Date(seedlings$date)
seedlings$area = pi*(seedlings$c1/2) * (seedlings$c2/2)

seasons = c( T1 = as.Date('2012-01-01'), 
             T2013.1 = as.Date('2013-01-01'),  
             T2013.2 = as.Date('2013-08-01'), 
             T2014.1 = as.Date('2014-01-01'),  
             T2014.2 = as.Date('2014-08-01'))


seedlings = genTransitionTable(demodf=seedlings, breaks = seasons)

seedlings[ seedlings$ID == 832, ]

seasonalGroups = aggStatus( demodf= seedlings)

###### assign num days for each transition #################
seasonalGroups$ndays[ seasonalGroups$transition == 'T1' ] <- 232
seasonalGroups$ndays[ seasonalGroups$transition == 'T2013.1' ] <- 132
seasonalGroups$ndays[ seasonalGroups$transition == 'T2013.2' ] <- 205
seasonalGroups$ndays[ seasonalGroups$transition == 'T2014.1'] <- 154
#############################################################

change_df = genChangeDF(seasonalGroupsDF= seasonalGroups)

#######

################# Relative growth by 30 days/mo ##############
change_df$rgr = 30*(log(change_df$area.1) - log(change_df$area))/change_df$ndays #### monthly relative growth
change_df$height.g = 10*(30*(change_df$ch.1 - change_df$ch)/change_df$ndays) #### monthly height growth mm/mo.
change_df$stemRGR = 30*(log(pi*(change_df$stem_d1.1/2)^2) - log(pi*(change_df$stem_d1/2)^2))/change_df$ndays #### stem area RGR 
##############################################################

plantSites = merge(plants[, c('ID', 'site', 'treatment', 'species', 'date_treated')], sites, by = 'site')
plot_df = merge(change_df, plantSites[, c('ID','species', 'treatment','site', 'site_no', 'ele', 'date_treated')], by = 'ID')

plot_df$treatment[ is.na(plot_df$treatment)  ]  <- 'control'
plot_df[ plot_df$transition.1 == 'T2013.1' , 'treatment' ] <- 'control'

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
atp = subset(plot_df, species == 'ATP')
atv = subset(plot_df, species == 'ATV')

################# Survival by size ############################
survSizePlot(plotdf= plot_df, xlab = xlabSize)


################# Herbivory risk ############################## 
ggplot( plot_df, aes( x = ele, y = herbivory.1, group = treatment, color = treatment)) + 
  geom_point() + geom_smooth( method = 'glm', family = 'binomial', se = FALSE) + 
  facet_grid(species ~ transition.1)

################### RGR plots ################################ 
RGRElevationPlot( plot_df, xlab = xlabEle, ylab = ylabRGR)

p = RGRElevationPlot(subset(plot_df, !rgr < -0.5), xlab = xlabEle, ylab = ylabRGR) #### Remove outlier 
p 

###### plot omit herbivory  
ggplot( subset(plot_df, herbivory.1 == 0) , aes( x = ele, y = rgr, color = treatment)) + 
  geom_point()  +   
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_grid(species ~ transition.1)

################# stem RGR ####################################
stemRGRElevationPlot( plotdf = plot_df, xlab =xlabEle, ylab = ylabStemRGR)

###### plot omit herbivory  
ggplot( subset(plot_df, herbivory.1 == 0) , aes( x = ele, y = stemRGR, color = treatment)) + 
  geom_point()  +   
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_grid(species ~ transition.1)


################## canopy height growth #######################
heightGrowthElevationPlot(plotdf = plot_df, xlab = xlabEle, ylab = ylabHeightGrowth)


###### plot omit herbivory  
ggplot( subset(plot_df, herbivory.1 == 0) , aes( x = ele, y = height.g, color = treatment)) + 
  geom_point()  +   
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_grid(species ~ transition.1)



###### Extra plots 
ggplot( subset ( plot_df, herbivory.1 == 0) , aes( x = species, fill = treatment, y = rgr  )) + 
  geom_boxplot( position = 'dodge') + facet_grid(. ~ transition.1)

ggplot( subset ( plot_df, herbivory.1 == 0) , aes( x = species, fill = treatment, y = rgr)  ) + 
  geom_boxplot( position = 'dodge') + facet_grid(. ~ transition.1)

##### check height and area shrinkage and growth 
height_shrinkers = plot_df[ which(plot_df$height.g < -1), 'ID' ]
unique(height_shrinkers)

area_shrinkers = plot_df[ which(plot_df$rgr < -0.2) , 'ID']
unique(area_shrinkers)

###### spagheti plot 
seedlings$area[ seedlings$area == -999 ] <- NA
seedlings$ch [ seedlings$ch == -999] <- NA
seedlings$ID = as.factor( seedlings$ID ) 
spagPlot = seedlings [ -which ( is.na(seedlings$area) ) , ]

spagPlot = merge( spagPlot, plants[ , c('ID', 'treatment', 'site', 'species', 'date_treated') ], by = 'ID')
spagPlot[ which( as.Date( spagPlot$date )  <  as.Date( spagPlot$date_treated)  ), 'treatment' ]  <- "control"

ggplot( spagPlot, aes(x = date, y =  area  , group = ID, color = treatment )) + 
  geom_point( ) + geom_line() + facet_grid( . ~ species) + ylab(xlabSize)
