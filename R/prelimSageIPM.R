library(IPMpack)

dff = generateData()

head(dff)

sage_df = readRDS('~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/IPM_DF.RDS')

head(sage_df)

sage_df2 = sage_df[, c('ID', 'transition.1', 'date', 'date.1', 'area', 'area.1', 'survival', 'Season', 'species', 'treatment', 'ele')]
head(sage_df2)

sage_df3 = subset(sage_df2, Season == 'Winter')
head(sage_df3)

merge( sage_df2, )


sage_df2$stage = 'continuous'
sage_df2$stageNext = 'continuous'
 
sage_df2$size = log( sage_df2$size ) 
sage_df2$sizeNext = log(sage_df2$sizeNext )

head(sage_df2)

head( dff )

plot(sage_df2$size, sage_df2$sizeNext, xlab = "Size at t", ylab = "Size at t+1")


grATP <- makeGrowthObj(dataf = sage_df2, Formula = sizeNext ~ size + size2 + size3 + species )
svATP <- makeSurvObj(dataf= sage_df2, Formula = surv ~ size + size2 + size3 + species )

picGrow(growObj = grATP, dataf= sage_df2)

picSurv(survObj = svATP, dataf= ATP)

hist( ATP$size ) 
min(ATP$size[is.finite(ATP$size)])

max(ATP$size, na.rm = TRUE)


Pmatrix <- makeIPMPmatrix( nBigMatrix = 100, 
                            minSize = -1, maxSize = 10,
                            growObj = grATP, survObj = svATP, 
                          correction = 'constant')

diagnosticsPmatrix(Pmatrix, growObj=grATP, survObj=svATP, ATP)


