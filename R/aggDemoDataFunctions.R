########### aggregate Demographic data functions ############
######  Functions specific to sagebrush data 
######  Aggregate by ID and time period in order to calculate 
######  growth, survival etc. 
#############################################################


genTransitionTable = function( demodf, breaks ){ 
  
  transitionTable = data.frame( date = sort(unique(demodf$date)), transition = NA) 
  for(i in breaks ){ 
    transitionTable[ transitionTable$date >= i, 'transition' ] <- names(breaks)[ breaks == i]
  }
  outdf = merge(transitionTable, demodf, by = 'date')
  return(outdf)
} 

aggStatus = function(demodf ) { 

  demodf$area [ is.na(demodf$area)  ] <- -999
  demodf$stem_d1[ is.na(demodf$stem_d1) ] <- -999
  demodf$ch[ is.na(demodf$ch)] <- -999
  demodf$herbivory[ is.na(demodf$herbivory) ] <- 0
  demodf$canopy[ is.na(demodf$canopy)] <- -999
  
  seasonalArea = aggregate(area ~ ID + transition, demodf, 'max')
  seasonalStatus = aggregate(status ~ ID + transition, demodf, 'min')
  seasonalHerbivory = aggregate(herbivory ~ ID + transition, demodf, 'max')
  seasonalHeight = aggregate(ch ~ ID + transition, demodf, 'max')
  seasonalStem = aggregate(stem_d1 ~ ID + transition, demodf, 'max')
  seasonalCanopy = aggregate(canopy ~ ID + transition, demodf, 'max')
  seasonalDate = aggregate( date ~ ID + transition, demodf, 'max')
  
  seasonalGroups = merge(seasonalArea, seasonalStatus, by = c('ID', 'transition'))
  seasonalGroups = merge(seasonalGroups, seasonalHeight, by = c('ID', 'transition'))
  seasonalGroups = merge(seasonalGroups, seasonalStem, by = c('ID','transition'))
  seasonalGroups = merge(seasonalGroups, seasonalCanopy, by = c('ID','transition'))
  seasonalGroups = merge(seasonalGroups, seasonalHerbivory, by = c('ID', 'transition'))
  seasonalGroups = merge( seasonalGroups, seasonalDate, by = c('ID', 'transition'))
  
  seasonalGroups$area[ seasonalGroups$area == -999 ]<- NA
  seasonalGroups$ch[ seasonalGroups$ch == -999 ]<- NA
  seasonalGroups$stem_d1[ seasonalGroups$stem_d1 == -999 ]<- NA
  seasonalGroups$canopy [ seasonalGroups$canopy == -999] <- NA
  
  return(seasonalGroups )
}

genChangeDF = function( seasonalGroupsDF ) { 

  #####  order by ID and then transition 
  seasonalGroups = seasonalGroupsDF[order(seasonalGroupsDF$ID, seasonalGroupsDF$transition), ]
  
  change_df = data.frame(seasonalGroupsDF[ -nrow(seasonalGroupsDF), ], seasonalGroupsDF[-1, ])
    
  change_df = change_df[ , sort(names(change_df)) ]
  change_df = change_df[ change_df$ID == change_df$ID.1 , ]
  change_df = change_df[ , c('ID', 'transition.1', 'area', 'area.1', 'ch', 'ch.1', 
                             'stem_d1', 'stem_d1.1', 'canopy', 'canopy.1', 'status.1', 'herbivory.1', 'date', 'date.1', 'ndays')]
  
  change_df$survival[ change_df$status.1 == 1] <- 1
  change_df$survival[ change_df$status.1 == 0] <- 0
  change_df$survival[ change_df$status.1 == 3] <- 0
  
  return(change_df )
}

