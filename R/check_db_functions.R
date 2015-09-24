require(assertthat)
require(DBI)
require(RSQLite)

checkSiteLabels = function( x ) { 
  x = factor(x)
  good_labels = LETTERS[ 1:13 ]
  assert_that(is.factor(x)) 
  length(setdiff( levels( x ), good_labels)) == 0
}

on_failure(checkSiteLabels) <- function( call, env) { 
  paste0(deparse(call$x),  " outside of label range") 
}

is.wholenumber <- function(x ) { 
  tol = .Machine$double.eps^0.5
  abs(x - round(x)) < tol
} 

allWholeNumbers <- function( x, na.rm = FALSE ) { 
  if( na.rm == TRUE) { 
    x = x [ - which( is.na( x ))]
  }
  sum( !is.wholenumber( x ) ) == 0 
}

on_failure(allWholeNumbers) <- function( call, env) { 
  paste0(deparse(call$x), " contains non-whole number")
}

checkTags = function( x , upper.limit= 2000, na.rm = TRUE) { 
  assert_that ( is.numeric (x), 
                min ( x , na.rm = na.rm) > 0, max( x , na.rm = na.rm) < upper.limit, 
                allWholeNumbers( x, na.rm = na.rm ))
}

checkPlantID = function( x , upper.limit = 2000) { 
  assert_that( is.numeric( x ), 
               min( x )  > 0, max( x ) < upper.limit, 
               allWholeNumbers( x ), 
               checkDuplicateID ( x )) 
}

checkDuplicateID = function( x ) { 
  sum( duplicated (x) ) == 0 
}

on_failure(checkDuplicateID) <- function( call, env) { 
  paste0(deparse(call$x),  " contains duplicate IDs") 
}

checkDate = function( x, na.rm = FALSE ) {
  date = as.Date(x)
  assert_that(min( date, na.rm= na.rm )  > as.Date( '2012-10-01' ), max( date, na.rm = na.rm )  < Sys.Date())
}

checkStatus = function( x ) { 
  assert_that( is.numeric(x), min(x) >= 0, max(x) < 4, allWholeNumbers( x ))
}

checkHerbivory <- function( x ) { 
  assert_that( is.numeric(x), min(x) >= 0, max(x) < 2, allWholeNumbers( x ))  
}

checkPositiveRange <- function( x, upper.limit = 200) { 
  assert_that( is.numeric( x), min( x, na.rm= TRUE) >= 0, max( x, na.rm = TRUE) < upper.limit )
}

checkMonthRange <- function( x , early = 9, late = 11 ) { 
  date = as.Date( x )
  months.Date( date ) %in% month.name [early:late ] 
}

checkAllMonths <- function( x , early = 9 , late = 11 ) { 
  sum(!checkMonthRange(x, early = early, late = late)) == 0 
}

on_failure(checkAllMonths) <- function( call, env) { 
  paste0(deparse(call$x),  " months out of range") 
}

checkActive <- function( x, active ) { 
  BadIDs = setdiff ( x , active) 
  if( length( BadIDs) > 0  ) { 
    warning( "Some ID's in status update not found among active plants: ")
    return( BadIDs)
  } 
}

checkForMissing <- function( x, active ) { 
  missing = setdiff( active, x )
  if (length(missing) > 0 ) { 
    warning( 'Some plants missing from status update: ')
    missing
  }
}

checkSpecies <- function( x ) { 
  x <- factor( x )
  identical(levels(x), y = levels( factor(c('ATP', 'ATV')) ))                        
}

checkClasses <- function ( x ) { 
  x <- factor( x )
  identical(levels(x), levels(factor(1:7)))
}



checkTreatment <- function ( x ) { 
  x <- factor(x)
  identical( levels( x ), levels( factor(c('control', 'remove'))))
}

statusChangeReport <- function( old, new ) { 
  
  merged =  merge( old, new, by = 'ID', suffixes=c('.old', '.new'))

  print( "Survivors:") 
  categorizeStatusChange( merged= merged, from.status=1, to.status=1)
  
  print( "Deaths:")
  categorizeStatusChange( merged = merged, from.status=1, to.status=0)  
  
  print( "Uncertain dead: ")
  categorizeStatusChange( merged = merged, from.status=1, to.status=3)  
  
  print( "Lost: ")
  categorizeStatusChange( merged = merged, from.status=1, to.status=2)  
    
  print( "Stays Dead: ")
  categorizeStatusChange( merged = merged, from.status=0, to.status=0)  
  
  print( "Reborn: ")
  categorizeStatusChange( merged = merged, from.status=0, to.status=1)  
  
  print( "Dead to uncertain: ")
  categorizeStatusChange( merged = merged, from.status=0, to.status=3)  
  
  print( "Dead to lost: ")
  categorizeStatusChange( merged = merged, from.status=0, to.status=2)  
  
  print( "Uncertain to Dead: ")
  categorizeStatusChange( merged = merged, from.status=3, to.status=0)  

  print( "Uncertain to Live: ")
  categorizeStatusChange( merged = merged, from.status=3, to.status=1)  
  
  print( "Uncertain to uncertain: ")
  categorizeStatusChange( merged = merged, from.status=3, to.status=3)  
  
  print( "Uncertain to Lost: ")
  categorizeStatusChange( merged = merged, from.status=3, to.status=2)  
  
  print( "Lost to Dead: ")
  categorizeStatusChange( merged = merged, from.status=2, to.status=0)  

  print( "Lost to alive: ")
  categorizeStatusChange( merged = merged, from.status=2, to.status=1)  
  
  print( "Lost to uncertain: ")
  categorizeStatusChange( merged = merged, from.status=2, to.status=3)  
  
  print( "Lost to Lost: ")
  categorizeStatusChange( merged = merged, from.status=2, to.status=2)  

  waitkey()
}

categorizeStatusChange = function( merged, from.status, to.status ){   
  #### merged is a dataframe with the old and new status's merged by ID
  showColumns = c('ID', 'site', 'species', 'class', 'field_tag.old', 'field_tag.new', 'status.old', 'status.new', 'herbivory.new')
  changed = merged[ which( merged$status.old == from.status & merged$status.new == to.status), ]
  print(paste( "total", nrow(changed)))
  
  if( nrow(changed) > 0 ) { 
    print(changed[ , showColumns ]) 
  }   
  print( '----------------------------')
}

showSizeDiff = function( old, new, measure ) { 
  
  merged = merge(old, new, by = 'ID', suffixes=c('.old', '.new') )  
  oldSize =  merged[ , which(names(merged) == paste(measure, '.old', sep = '')) ] 
  newSize =  merged[ , which(names(merged) == paste(measure, '.new', sep = '')) ]  

  quartz(width=5, height = 5)
  plot( x = oldSize, y = newSize, xlab = paste('old', measure), ylab = paste('new', measure))
  text( x = oldSize, y = newSize, labels = merged$ID, adj=c(-0.2))
  abline( 0, 1)
  waitkey()

}


waitkey = function(){
  readline( prompt = 'press [enter] to continue' ) 
}

#### tests of checks 
ID.1 = seq(-1, 10 , 1)
ID.2 = seq( 1000, 3000, 1)
ID.3 = c( 1.1, 2, 3, 4 )

StatusPass = c( 0 , 1, 2, 3)
StatusFail = c( 0 , 1.1, 3, 4)

see_if( checkStatus( StatusPass) ) 
see_if( checkStatus(StatusFail) )   

see_if(checkDate( c('2013-10-01', NA), na.rm = TRUE))

see_if(checkDate( c(NA, '2013-01-01'), na.rm = TRUE))

see_if(checkDate( '2011-01-01'))
see_if(checkDate( '2016-01-01'))

see_if(checkPlantID( ID.1))
see_if(checkPlantID( ID.2))
see_if(checkPlantID( ID.3))

