#####
##### 
##### 
#####

rm(list = ls())
library(RSQLite)
library(plyr)
library(lme4)

source('demoPlotFunctions.R')
source('aggDemoDataFunctions.R')

db = dbConnect(SQLite(), 'sage.sqlite')

plants = dbGetQuery(db, "SELECT * FROM plants")

sites = dbGetQuery( db, "SELECT * FROM sites")

flowers = dbGetQuery( db, "SELECT * FROM flowers")

naturals = dbGetQuery( db, "SELECT status.ID, date, status, herbivory, c1, c2, ch, canopy, infls, stem_d1, status.notes 
                   FROM status JOIN plants ON status.ID = plants.ID WHERE class < 4" ) 

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
as.Date( '2015-06-01') - as.Date('2014-10-21')

###### assign num days for each transition #################
seasonalGroups$ndays[ seasonalGroups$transition == 'T1' ] <- 232
seasonalGroups$ndays[ seasonalGroups$transition == 'T2013.1' ] <- 132
seasonalGroups$ndays[ seasonalGroups$transition == 'T2013.2' ] <- 205
seasonalGroups$ndays[ seasonalGroups$transition == 'T2014.1' ] <- 154
seasonalGroups$ndays[ seasonalGroups$transition == 'T2014.2' ] <- 223
#############################################################

change_df = genChangeDF(seasonalGroupsDF= seasonalGroups)

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

plot_df$transition.1 = as.factor(plot_df$transition.1)

plot_df$seasonLabel = factor(plot_df$transition.1, labels = c('Fall 2012 to Spring 2013', 
                                                              'Spring 2013 to Fall 2013', 
                                                              'Fall 2013 to Spring 2014', 
                                                              'Spring 2014 to Fall 2014', 
                                                              'Fall 2014 to Spring 2015')) 

plot_df$Season[ plot_df$transition.1 == 'T2013.1'] <- 'Winter'
plot_df$Season[ plot_df$transition.1 == 'T2013.2'] <- 'Summer'
plot_df$Season[ plot_df$transition.1 == 'T2014.1'] <- 'Winter'
plot_df$Season[ plot_df$transition.1 == 'T2014.2'] <- 'Summer'
plot_df$Season[ plot_df$transition.1 == 'T2015.1'] <- 'Winter'

plot_df$speciesLab = factor(plot_df$species, labels = c('A.tripartita', 'A.tridentata vaseyana'))

#################### plot labels #############################
xlabEle = "Elevation (m)"
xlabSize = expression('Plant area log(cm'^2*')')
ylabRGR = expression( 'Monthly relative growth' ~ '(log area'[t] - 'log area'[t-1] * ')/mos.' ) 
ylabStemRGR = expression( 'Monthly relative stem growth' ~ '(log stem area'[t] - 'log stem area'[t-1] * ')/mos.' ) 
ylabHeightGrowth = "Height growth (mm/mo.)"
ylabCanopyRGR = expression( 'Monthly relative growth' ~ '(log area'[t] - 'log area'[t-1] * ')/mos.' ) 
ylabNinfls = 'Number of inflorescences'
xlabCanopy = expression('Plant canopy area log(100 cm'^2*')')
ylabInfls = "Mean infls length (cm)"
##############################################################
#################### Survival plot ###########################
survElevationPlot(plotdf = plot_df, xlab = xlabEle) ##### All plants 

survElevationPlot(plotdf = subset(plot_df, class == 1), xlab = xlabEle, smooth = FALSE) ##### non-seedlings

survSizePlot(plotdf= plot_df, xlab = xlabSize)

################# Survival by size ############################
atp = subset(plot_df, species == 'ATP')
atv = subset(plot_df, species == 'ATV')

atp$eleSD  = ( atp$ele - max( atp$ele ))/10 ##### standardize so the maximum elevation is 0
atv$eleSD  = ( atv$ele - min( atv$ele ))/10 ##### standardize so the minimum elevation is 0

atp$sizeSD = ( log(atp$area) - mean( log(atp$area), na.rm = TRUE ))/ sd( log(atp$area), na.rm = TRUE) 
atv$sizeSD = ( log(atv$area) - mean( log(atv$area), na.rm = TRUE))/ sd( log(atv$area), na.rm = TRUE)

###### size quantiles 
atpQs = quantile( atp$sizeSD, na.rm = TRUE , c( 0.25, 0.5, 0.75)) 
hist(atp$sizeSD)
abline( v = atpQs)

########### formula 
mod2 = formula ( 'survival ~ sizeSD*treatment*eleSD + (1|ID) ')

m1 = glmer( mod2, atp, family = 'binomial')
m2 = update(m1, . ~ . - sizeSD:treatment:eleSD )

AIC(m1, m2)
summary(m1)

pred_df = data.frame( expand.grid( sizeSD = atpQs, eleSD = unique(atp$eleSD), treatment = c('control', 'remove') ) ) 

pred_df$y = predict( object = m1,  newdata = pred_df, re.form= NA, type= 'response')

pred_df$sizeLabs = factor( pred_df$sizeSD, labels = c('small', 'med', 'large'))
p = ggplot(pred_df, aes( y = y, x = eleSD*10, group = treatment, color = treatment )) + 
  facet_grid( sizeLabs ~ . ) + geom_smooth( se = FALSE )

p 

atp$sizeClasses = cut(atp$sizeSD , breaks= c( min(atp$sizeSD, na.rm = TRUE), atpQs[2], atpQs[3], max(atp$sizeSD, na.rm = TRUE))) 

summary( atp$sizeClasses ) 

atp$sizeClasses = factor( atp$sizeClasses, labels = c('small', 'med', 'large') ) 

survPropDF = aggregate(survival ~ sizeClasses + treatment + eleSD, data = atp, FUN = function(x) sum(x)/sum(!is.na(x)))
head(survPropDF)
survPropDF$y = survPropDF$survival
survPropDF$sizeLabs = survPropDF$sizeClasses

p + geom_point( data = survPropDF ) + ylim( 0.7, 1) ##### plot fitted model vs. observed 


####### seasonal effect on controls 
mod3 <- formula ( 'survival ~ log(area)*eleSD*Season + (1|ID)')

s1.atp.c = glmer( mod3, subset(atp, treatment == 'control'), family = 'binomial')
s2.atp.c = update(s1.atp.c, . ~ . - log(area):eleSD:Season )
s3.atp.c = update(s2.atp.c, . ~ . - log(area):eleSD )
s4.atp.c = update( s3.atp.c, . ~ . - log(area):Season)
s5.atp.c = update( s4.atp.c, . ~ . - eleSD:Season)

AIC(s1.atp.c, s2.atp.c, s3.atp.c, s4.atp.c, s5.atp.c)

summary(s4.atp.c)

s1.atv.c = glmer( mod3, subset(atv, treatment == 'control'), family = 'binomial')

s2.atv.c = update(s1.atv.c, . ~ . - log(area):eleSD:Season )
s3.atv.c = update(s2.atv.c, . ~ . - log(area):eleSD )
s4.atv.c = update( s3.atv.c, . ~ . - log(area):Season)
s5.atv.c = update( s4.atv.c, . ~ . - eleSD:Season)

AIC(s2.atv.c, s3.atv.c, s4.atv.c, s5.atv.c)

summary(s5.atv.c)


survElevationPlot( plotdf= subset(atp, treatment == 'control'), xlab = xlabEle)
survSizePlot( plotdf=subset(atp, treatment == 'control'), xlab = xlabSize)

atvQs = quantile( atv$area, na.rm = TRUE , c( 0.25, 0.5, 0.9))
log(as.numeric( atvQs )) 

atp$areaClass = cut(atp$area, breaks= as.numeric(c (0,  atpQs) )  )
atv$areaClass = cut( atv$area, breaks = as.numeric( c (0, atvQs)))

survProp = aggregate( survival ~ ele + transition.1 + areaClass + treatment, atp, FUN = function(x) sum(x)/sum(!is.na(x)))
survPropATV = aggregate( survival ~ ele + transition.1 + areaClass + treatment, atv, FUN = function(x) sum(x)/sum(!is.na(x)))

##### atp 
ggplot(survProp, aes(x = ele, y = survival, group = areaClass, color = areaClass)) + 
  geom_point( position = position_jitter() ) + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_grid(transition.1 ~ treatment)

###### atv 
ggplot(survPropATV, aes(x = ele, y = survival, group = areaClass, color = areaClass)) + 
  geom_point( position = position_jitter() ) + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_grid(transition.1 ~ treatment)


###### fit glm models with ID as a random effect.  
s1 = glmer(survival ~ log(area) + eleSD*transition.1 + (1|ID), data = atv, family= 'binomial' )
summary(s1)

####### make dataframe for prediction against 
gradient = data.frame( expand.grid( Season = c('Summer', 'Winter'), eleSD = seq(-2, 1.5, by = 0.5), area = as.numeric(atpQs), treatment = c('control', 'remove')))

gradient$surv =predict( s1, newdata= gradient, re.form=NA, type = 'response')

ggplot(gradient, aes( x = eleSD, y = surv, color = factor(area) )) + 
  geom_line() + facet_grid(Season ~ treatment)

summary(s1 ) 


################### RGR plots ################################ 
rgrPlot = RGRElevationPlot(subset( plot_df), xlab = xlabEle, ylab = ylabRGR, markHerbivory= TRUE)
rgrPlot

rgrPlot2 = RGRElevationPlot(subset( plot_df, herbivory.1 == 0), xlab = xlabEle, ylab = ylabRGR, markHerbivory= FALSE)
rgrPlot2

RGRElevationPlot

ggplot(subset( atp, herbivory.1 == 0), aes( x = ele, y = rgr, group = treatment, color = treatment)) + 
  geom_point() + facet_grid( . ~ Season) + geom_smooth( method = 'lm', se = FALSE)

ggplot(atv, aes( x = ele, y = rgr, group = treatment, color = treatment)) + 
  geom_point() + facet_grid( class ~ Season) + geom_smooth( method = 'lm', se = FALSE)

test = subset( atp, herbivory.1 == 0 & transition.1 %in% c('T2014.1', 'T2014.2') ) 
aggregate( rgr ~ eleSD + treatment, data = atp, 'mean')

table(test$eleSD, test$treatment, is.na(test$rgr))

atpRGR1 = lmer( rgr ~ sizeSD*treatment*eleSD + (1|ID) + (1|transition.1), data = subset(atp, herbivory.1 == 0 ) )  
summary(atpRGR1)      

atpRGR2 = update(atpRGR1, . ~ . - sizeSD:treatment:eleSD )
atpRGR3 = update( atpRGR2, . ~ . - sizeSD:eleSD )
atpRGR4 = update( atpRGR3, . ~ . - sizeSD:treatment )
atpRGR5 = update( atpRGR4, . ~ . - eleSD:treatment)
atpRGR6 = update( atpRGR5, . ~ . - eleSD )
atpRGR7 = update( atpRGR6, . ~ . - treatment)

summary(atpRGR6)

AIC(atpRGR1, atpRGR2, atpRGR3, atpRGR4, atpRGR5, atpRGR6, atpRGR7)

summary(atpRGR5)
summary(atpRGR6)


atp$rgrSD = (atp$rgr - mean(atp$rgr, na.rm = TRUE))/ sd(atp$rgr, na.rm = TRUE)

rgrM1 = lmer( rgrSD ~ log(area)*treatment*eleSD*Season + (1|ID), data = subset( atp, herbivory.1 == 0))

pred_df = expand.grid( area = as.numeric(atpQs), treatment = c('control', 'remove'), eleSD = seq( -2, 1.5, 0.25) , Season = c('Summer', 'Winter')) 

pred_df$rgrPred = predict( rgrM1, newdata= rgr_df, re.form= NA)

summary(rgrM1)

confint.merMod( rgrM1 )
fixef( rgrM1 )

ggplot(pred_df, aes( x = eleSD, y = rgrPred, group = area, color =as.factor( area ))) + 
  facet_grid(Season ~ treatment) + geom_line() 


################### RGR of canopy area ####################### 
RGRCanopyElevationPlot(subset(plot_df, class > 1 & transition.1 != 'T2013.1'), xlab = xlabEle, ylab = ylabCanopyRGR)

ggplot( subset( plot_df, canopy > 10 ), aes( y = canopyRGR,  x = rgr , color = treatment)) + 
  facet_grid( . ~ species) + 
  geom_point()  + 
  geom_abline( aes ( 0, 1) ) 

subset(plot_df, canopyRGR < -0.25)

################## canopy height growth #######################
heightGrowthElevationPlot(plotdf = subset( plot_df, class > 1), xlab = xlabEle, ylab = ylabHeightGrowth)

####################### canopy area vs. L x W area ####################
ggplot(plot_df, aes( x = canopy, y = area, color = treatment, group = treatment)) + 
  geom_point() + facet_grid(speciesLab ~ seasonLabel) + xlab(xlabCanopy) + 
  ylab(xlabSize)+ geom_abline(intercept = 0, slope = 100) 

################## inflorescence number #######################
inflsNumber = aggregate( infls ~ ID + transition, naturals, FUN = 'identity') 
inflsDF = merge(inflsNumber, seasonalGroups, by = c( "ID", "transition")) 
inflsDF = merge(inflsDF, plantSites, by = c( 'ID'))
inflsDF[ inflsDF$transition == 'T1', 'treatment' ] <- 'control' 

inflsDF$seasonLabel <- factor( inflsDF$transition, labels  = c( "2012 Fall", "2013 Fall", "2014 Fall"))
inflsDF$speciesLab <- factor( inflsDF$species, labels = c('A.tripartita', 'A.tridentata vaseyana'))

pNumInfls = inflsNumElevationPlot(subset(inflsDF, class > 1) , ylab = ylabNinfls, xlab = xlabEle) + 
  scale_color_discrete( name = 'Competition' ) + myTheme

pNumInfls

m1 = glm(infls ~ ele, subset(inflsDF, class > 1 & species == 'ATP' & seasonLabel == '2012 Fall' ), family = 'quasipoisson')
summary(m1)

m2 = glm(infls ~ ele, subset(inflsDF, class > 1 & species == 'ATV' & seasonLabel == '2012 Fall' ), family = 'quasipoisson')
summary(m2)

m3 = glm(infls ~ ele + treatment, subset(inflsDF, class > 1 & species == 'ATP' & seasonLabel == '2013 Fall' ), family = 'quasipoisson')
summary(m3)

m4 = glm(infls ~ ele + treatment, subset(inflsDF, class > 1 & species == 'ATV' & seasonLabel == '2013 Fall' ), family = 'quasipoisson')
summary(m4)

statsDFinfls = expand.grid( speciesLab = levels(inflsDF$speciesLab), seasonLabel = levels(inflsDF$seasonLabel))
statsDFinfls$ele = 1770
statsDFinfls$infls = 600
statsDFinfls$text = NA
statsDFinfls$treatment = 'control'
statsDFinfls$text[ 1 ] = 'Elev. n.s.\nComp n.s.'
statsDFinfls$text[ 2 ] = 'Elev. n.s.\nComp n.s.'
statsDFinfls$text[ 3 ] = 'Elev. p=0.09\nComp n.s.'
statsDFinfls$text[ 4 ] = 'Elev. p<0.01\nComp n.s.'
  
pNumInfls + geom_text(data = statsDFinfls, aes(label= text, hjust = 0, vjust = 0, fontface= 'italic'), color = 1, size = 3.7) + 
  theme(strip.text.y = element_text(size = 12, face = "italic"), strip.text.x = element_text(size = 12)) 


####################### number infls by area ##############################
ggplot(subset (inflsDF , class > 1), aes(x = log(area), y = infls, group = treatment, color = treatment))+ 
  geom_point() + facet_grid(speciesLab ~ seasonLabel) + ylab("infls #") + xlab(xlabSize)

ggplot(subset (inflsDF, class > 1), aes( x = log(canopy), y = infls, group = treatment, color = treatment)) + 
  geom_point() + facet_grid(species ~ transition) + xlab(xlabCanopy)

################### Inflorescence length data ######################################
meanFL = aggregate( length ~ ID + date, flowers , mean)
nFls = aggregate( length ~ ID + date, flowers, length)
names(nFls)[3] <- 'count'
meanFL = merge( meanFL, nFls, by = c('ID', 'date'))   
FL_df = merge(meanFL, naturals, by = c('ID', 'date'))
FL_df$FLintensity = FL_df$length*FL_df$infls
FL_df = merge(FL_df, plantSites, by = 'ID')

FL_df$season = NA
seasons
FL_df$season[ which( FL_df$date < as.Date( seasons[4] )) ] <- names(seasons[3]) 
FL_df$season[ which( FL_df$date > as.Date( seasons[4] )) ] <- names(seasons[5])

factor(FL_df$season)
FL_df$season = factor( FL_df$season, labels= c('Fall 2013', 'Fall 2014'))

FL_df$speciesLab = factor(FL_df$species, labels = c("A.tripartita", "A.tridentata vaseyana"))

ggplot(FL_df, aes(x = log(canopy), y = length, group = treatment, color = treatment)) + 
  geom_point() + 
  facet_grid( speciesLab ~ season ) + 
  geom_smooth( method = 'lm', se = FALSE) + 
  ylab(ylabInfls) + 
  xlab(xlabCanopy) 

################## Infls length by elevation and treatment ###########################
pinfls = ggplot(FL_df, aes(x = ele, y = length, group = treatment, color = treatment)) + geom_point() + 
  facet_grid( speciesLab ~ season ) + geom_smooth( method = 'lm', se = FALSE) + ylab(ylabInfls) + 
  xlab(xlabEle)
pinfls 

ggplot(FL_df, aes(x = ele, y = length)) + geom_point() + 
  facet_grid( speciesLab ~ season ) + geom_smooth( method = 'lm', se = FALSE) + ylab(ylabInfls) + 
  xlab(xlabEle)

lenMod = lm(length ~ ele +treatment, weights= count, data = subset(FL_df, species == 'ATP'))
anova(lenMod)
summary(lenMod)

lenMod2 = lm(length ~ ele*treatment, weights = count, data = subset(FL_df, species == 'ATV'))
anova(lenMod2)
summary(lenMod2)

statsDFinfls = expand.grid( speciesLab = levels(plot_df$speciesLab))
statsDFinfls$ele = 1750
statsDFinfls$length = 11
statsDFinfls$text = NA
statsDFinfls$treatment = 'control'
statsDFinfls$text[ 1 ] = 'Elev. p <0.01\nComp n.s.'
statsDFinfls$text[ 2 ] = 'Comp by Elev. p<0.05'

pinfls + geom_text(data = statsDFinfls, aes(label= text, hjust = 0, vjust = 0, fontface= 'italic'), color = 1, size = 3.7)

ggplot(FL_df, aes(x = ele, y = log(FLintensity), group = treatment, color = treatment)) + geom_point() + 
  facet_grid(speciesLab ~ season) + geom_smooth(method = 'lm', se = FALSE) + 
  ylab("infls # X mean infls length") + xlab(xlabEle)


ggplot(FL_df, aes(x = ele, y = log(FLintensity)/log(area), group = treatment, color = treatment)) + geom_point() + 
  facet_grid(speciesLab ~ season) + geom_smooth(method = 'lm', se = FALSE) + 
  ylab("infls production per unit area") + xlab(xlabEle)


flintMod = lmer(log(FLintensity) ~ season*ele*treatment + (1|ID) , data = subset(FL_df, species == 'ATP'))
summary(flintMod)

m2 = update(flintMod, . ~ . - season:ele:treatment)
m3 = update(m2, . ~ . - season:ele)
m4 = update(m3, . ~ . - ele:treatment)
m5 = update(m4, . ~ . - ele)

summary(m5)



flintMod2 = lm(log(FLintensity) ~ log(canopy) + ele*treatment , data = subset(FL_df, species == 'ATV'))
summary(flintMod2)


head( FL_df )
head(plot_df)

all_df = merge(plot_df, FL_df[, c('ID', 'length', 'infls', 'transition')], by.x = c('ID', 'transition.1'), by.y = c('ID', 'transition') , all.x = TRUE)

names(all_df)

all_df = all_df[ , c('ID', 'transition.1', 'date', 'date.1', 'area', 'area.1', 'herbivory.1', 'ndays', 'survival', 
                     'stem_d1', 'stem_d1.1', 'canopy', 'canopy.1', 'length', 'infls', 'Season', 
                     'species', 'treatment', 'site', 'ele')]

saveRDS(all_df, '~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/IPM_DF.RDS' )

dbDisconnect(db)