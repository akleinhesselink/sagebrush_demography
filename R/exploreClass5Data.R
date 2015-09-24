#####
##### Start checking data for class five transplants
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

res = dbSendQuery( db, "SELECT * FROM sites")
sites = fetch(res, -1)
dbClearResult(res)

res = dbSendQuery( db, "SELECT status.ID, date, status, herbivory, c1, c2, ch, stem_d1, canopy, status.notes 
                   FROM status JOIN plants ON status.ID = plants.ID WHERE class = 5 AND date > Date('2013-05-24')" ) 
transplants = fetch(res, -1)
dbClearResult(res)


dbDisconnect(db)

plantSites = merge(plants[, c('ID', 'site', 'treatment', 'species', 'date_treated')], sites, by = 'site')

transplants$date = as.Date(transplants$date)
transplants$area = pi*(transplants$c1/2) * (transplants$c2/2)

transplants$herbivory[  transplants$herbivory != '1'   ] <- 0 
transplants$herbivory = as.numeric( transplants$herbivory)

##### 
seasons = c( T1 = as.Date('2013-01-01'), 
             T2013.1 = as.Date('2013-06-01'),  
             T2013.2 = as.Date('2013-08-01'), 
             T2014.1 = as.Date('2014-01-01'), 
             T2014.2 = as.Date('2014-08-01'))

transplants = genTransitionTable(demodf=transplants, breaks = seasons)
seasonalGroups = aggStatus( demodf= transplants)

###### assign num days for each transition #################
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
plot_df[ plot_df$transition.1 == 'T2014.1' , 'treatment' ] <- 'control'

plot_df$stemSizeSD = plot_df$stem_d1
plot_df$stemSizeSD[ is.na( plot_df$stemSizeSD ) ] <- plot_df$stem_d1.1[ is.na(plot_df$stemSizeSD) ]

plot_df$ATPRange[ plot_df$ele <= 1756  ] <- 'inside'
plot_df$ATPRange[ plot_df$ele > 1756 ] <-  'above'

plot_df$ATVRange [ plot_df$site %in% c('C', 'H', 'L', 'M', 'J', 'G') ]  <- 'inside' 
plot_df$ATVRange [ !plot_df$site %in% c('C', 'H', 'L', 'M', 'J', 'G') ]  <- 'below'

plot_df$ATVRange = factor(plot_df$ATVRange, levels = c('below', 'inside'), order = TRUE) 

plot_df$ATPRange = factor(plot_df$ATPRange, levels = c('inside', 'above'), order = TRUE) 

##### survival x elevation plots
plot_df$transition.1 = as.factor(plot_df$transition.1)
plot_df$treatment = as.factor(plot_df$treatment)

plot_df$seasonLabel = factor(plot_df$transition, labels = c('Fall 2013 to Spring 2014', 
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

atp = subset(plot_df, species == 'ATP')
atv = subset(plot_df, species == 'ATV')


m1 = glm( survival ~ treatment*ATPRange, subset( atp, transition.1 == 'T2014.2'), family = 'binomial')
summary(m1)

m2 = glm ( survival ~ treatment*ATVRange, subset( atv, transition.1 == 'T2014.2'), family = 'binomial')
summary(m2)

m1 = glm( survival ~ transition.1 + herbivory.1 , atp, family = 'binomial')
m2 = glm( survival ~ transition.1 + herbivory.1, atv, family = 'binomial')
summary(m1)

surviveProp1 = aggregate(survival ~ ATPRange + treatment + seasonLabel, 
                       atp, FUN = function(x) sum(x)/sum(!is.na(x)))

surviveProp2 = aggregate(survival ~ ATVRange + treatment + seasonLabel, 
                         atv, FUN = function(x) sum(x)/sum(!is.na(x)))

ggplot(surviveProp1, aes(x = factor( ATPRange) , y = survival, fill = treatment)) + 
  geom_bar( stat = 'identity', position = 'dodge' ) + facet_grid( . ~ seasonLabel )

ggplot(surviveProp2, aes(x = factor(ATVRange), y = survival , fill = treatment)) + 
  geom_bar( stat = 'identity', position = 'dodge') + facet_grid(. ~ seasonLabel)


################# Survival by size ############################
survSizePlot(plotdf= subset( plot_df), xlab = xlabSize)

################# Herbivory risk ############################## 
ggplot( plot_df, aes( x = ele, y = herbivory.1, group = treatment, color = treatment)) + 
  geom_point() + geom_smooth( method = 'glm', family = 'binomial', se = FALSE) + 
  facet_grid(species ~ transition.1)

ggplot( atp, aes( x = stemSizeSD, y = survival, color = factor( herbivory.1 )  ) ) + 
  geom_point() + geom_smooth( method = 'glm', family = 'binomial', se = FALSE) + 
  facet_grid( . ~ transition.1)

ggplot( atv, aes( x = stemSizeSD, y = survival, color = factor( herbivory.1 )  ) ) + 
  geom_point() + geom_smooth( method = 'glm', family = 'binomial', se = FALSE) + 
  facet_grid( . ~ transition.1)

ggplot( atp, aes( x = stemSizeSD, y = survival, color = factor( ATPRange )  ) ) + 
  geom_point() + geom_smooth( method = 'glm', family = 'binomial', se = FALSE) + 
  facet_grid( . ~ transition.1)

ggplot( atv, aes( x = stemSizeSD, y = survival, color = factor( ATVRange )  ) ) + 
  geom_point() + geom_smooth( method = 'glm', family = 'binomial', se = FALSE) + 
  facet_grid( . ~ transition.1)

######## herbivory rates inside and outside of range 
herbProp1 = aggregate(herbivory.1 ~ ATPRange + treatment + seasonLabel, 
                      atp, FUN = function(x) sum(x)/sum(!is.na(x)))

herbProp2 = aggregate(herbivory.1 ~ ATVRange + treatment + seasonLabel, 
                      atv, FUN = function(x) sum(x)/sum(!is.na(x)))

ggplot(herbProp1, aes(x = factor( ATPRange) , y = herbivory.1, fill = treatment)) + 
  geom_bar( stat = 'identity', position = 'dodge' )

ggplot(herbProp2, aes(x = factor(ATVRange), y = herbivory.1 , fill = treatment)) + 
  geom_bar( stat = 'identity', position = 'dodge')



################### RGR plots ################################ 
p = RGRElevationPlot( subset( plot_df ), xlab = xlabEle, ylab = ylabRGR)
p

###### plot omit herbivory  
ggplot( subset(plot_df, herbivory.1 == 0) , aes( x = ele, y = rgr, color = treatment)) + 
  geom_point()  +   
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_grid(species ~ transition.1)

m1 = lm(rgr ~ ele + treatment, subset(atp, herbivory.1 == 0 & transition.1 == 'T2014.2'))
summary(m1)

m2 = lm(rgr ~ ele*treatment, subset(atv, herbivory.1 == 0 & transition.1 == 'T2014.2'))
summary(m2)

##### Growth above and below range 

m1 = lm( rgr ~ ATPRange + treatment + transition.1, subset( atp, herbivory.1 == 0) )
summary(m1)

m2 = lm( rgr ~ ATVRange + treatment + transition.1, subset( atv, herbivory.1 == 0) )
summary(m2)

################# stem RGR ####################################
stemRGRElevationPlot(plot_df, xlab =xlabEle, ylab = ylabStemRGR )

###### plot omit herbivory  
ggplot( subset(plot_df, herbivory.1 == 0) , aes( x = ele, y = stemRGR, color = treatment)) + 
  geom_point()  +   
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_grid(species ~ transition.1)

################## canopy height growth #######################
p = heightGrowthElevationPlot(plot_df, xlab = xlabEle, ylab = ylabHeightGrowth)
p

###### plot omit herbivory  
ggplot( subset(plot_df, herbivory.1 == 0) , aes( x = ele, y = height.g, color = treatment)) + 
  geom_point()  +   
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_grid(species ~ transition.1)

