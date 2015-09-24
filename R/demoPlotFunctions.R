require( ggplot2 )

##### Demographic rate plot functions
##### Uses ggplot to produce figures for 

##### 1. Survival rate by elevation x treatment x species
##### 2. RGR by elevation x treatment x species 
##### 3. Survival by size x species x treatment
##### 4. Growth by size x species x treatment
##### 5. Flower number by elevation x species x treatment 
##### 6. Flower length by elevation x species x treatment 

survElevationPlot <- function( plotdf , xlab, herbivory = FALSE, smooth = TRUE){ 
  
  surviveProp = aggregate(survival ~ site + ele + speciesLab + treatment + seasonLabel, 
                          plotdf , FUN = function(x) sum(x)/sum(!is.na(x)))
    
  p = ggplot( plotdf,  aes( x = ele, y =  survival, group = treatment)) +
    geom_point( position = position_jitter(w = 10, h = 0), size = 2, alpha = 0.3) +   
    geom_point(data = surviveProp, aes(y = survival, color = treatment), size = 3) + 
    facet_grid( speciesLab  ~ seasonLabel) + 
    scale_y_continuous( limits = c(-0.05,1.05), breaks = seq(0, 1, 0.25)) + 
    scale_x_continuous( limits = c(1610, 1870) , breaks = seq(1620, 1870, 50)) + 
    xlab(xlabEle) + 
    scale_color_discrete( name = 'competition' ) 
   
  if( smooth == TRUE ){ 
    p = p + geom_smooth(aes( color = treatment) , method= 'glm', family = 'binomial', se = FALSE) 
  }
  
  if( herbivory == TRUE){ 
    p = p + geom_point( position = position_jitter(w = 5, h = 0), data = subset( plotdf, herbivory.1 == 1), 
                        aes(x = ele, y = survival, shape = as.factor(herbivory.1)), size = 4.2, alpha = 0.8) + 
      scale_shape_manual(name = '', labels = c('herbivory'), values= c(2))
  }

  return(p)
}

survSizePlot <- function( plotdf, xlab) { 

    p = ggplot(plotdf, aes( x = log(area), y = survival, color = treatment, group = treatment)) + 
      geom_point(position = position_jitter( w = 0, height=0.03)) + facet_grid(speciesLab ~ seasonLabel) + 
      xlab(xlab) + geom_smooth(method = 'glm', family = 'binomial', se = FALSE) + 
      scale_y_continuous(limits = c(-0.1,1.1), breaks = c(0,1)) +
      scale_color_discrete( name = 'competition' )  
  
    return(p)
}


RGRElevationPlot <- function( plotdf,  xlab, ylab, markHerbivory = TRUE){ 
    
  p = ggplot (plotdf , aes(x = ele, y = rgr, group = treatment, color = treatment)) + 
    geom_point(position = position_jitter(w = 0.5, h = 0)) + 
    facet_grid( speciesLab ~ seasonLabel) + geom_smooth(method = 'lm', se = FALSE) + 
    ylab(ylab) + 
    xlab(xlab) + 
    scale_color_discrete( name = 'competition' ) + 
    scale_x_continuous( limits = c(1610, 1870) , breaks = seq(1620, 1870, 50)) 
  
  if(markHerbivory == TRUE) { 
  p = p + geom_point( data = subset( plotdf, herbivory.1 == 1), 
                      aes(x = ele, y = rgr, shape = as.factor(herbivory.1), color = treatment), size = 5) + 
    scale_shape_manual(name = '', labels = c('herbivory'), values= c(2))  
  }
  
  return( p )
}

RGRCanopyElevationPlot <- function( plotdf,  xlab, ylab){ 
  
  p = ggplot (plotdf , aes(x = ele, y = canopyRGR, group = treatment, color = treatment)) + 
    geom_point(position = position_jitter(w = 0.5, h = 0)) + 
    facet_grid( speciesLab ~ seasonLabel) + geom_smooth(method = 'lm', se = FALSE) + 
    ylab(ylab) + 
    xlab(xlab) + 
    scale_x_continuous( limits = c(1610, 1870) , breaks = seq(1620, 1870, 50)) + 
    scale_color_discrete( name = 'competition' )  
  
  return( p )
}

stemRGRElevationPlot <- function( plotdf,  xlab, ylab ){ 
  
  p = ggplot (plotdf , aes(x = ele, y = stemRGR, group = treatment, color = treatment)) + 
    geom_point(position = position_jitter(w = 2, h = 0)) + 
    facet_grid( speciesLab ~ seasonLabel) + geom_smooth(method = 'lm', se = FALSE) + 
    ylab(ylab) + 
    xlab(xlab) + 
    geom_point( data = subset( plotdf, herbivory.1 == 1), 
                aes(x = ele, y = stemRGR, shape = as.factor(herbivory.1), color = treatment), size = 4.2) + 
    scale_color_discrete( name = 'competition' ) + 
    scale_x_continuous( limits = c(1610, 1870) , breaks = seq(1620, 1870, 50)) + 
    scale_shape_manual(name = '', labels = c('herbivory'), values= c(2))
  
  return( p )
}

heightGrowthElevationPlot <- function( plotdf, xlab, ylab, markHerbivory = TRUE){ 
  
  p = ggplot (plotdf , aes(x = ele, y = height.g, group = treatment, color = treatment)) + 
    geom_point(position = position_jitter(w = 2, h = 0)) + 
    facet_grid( speciesLab ~ seasonLabel) + geom_smooth(method = 'lm', se = FALSE) + 
    ylab(ylab) + 
    xlab(xlab) + 
    scale_color_discrete( name = 'competition' ) + 
    scale_x_continuous( limits = c(1610, 1870) , breaks = seq(1620, 1870, 50)) 
  
  if(markHerbivory == TRUE){ 
   p = p + geom_point( data = subset( plotdf, herbivory.1 == 1), 
                aes(x = ele, y = height.g, shape = as.factor(herbivory.1), color = treatment), size = 4.2) + 
    scale_shape_manual(name = '', labels = c('herbivory'), values= c(2))
  }
  
  return( p )
}

inflsNumElevationPlot <- function( plotdf, xlab, ylab ) { 
  p = ggplot(subset ( plotdf, class > 1), aes(x = ele, y = infls, group = treatment, color = treatment)) + geom_point() + 
    facet_grid( speciesLab ~ seasonLabel) + geom_smooth(method = 'glm', family = 'quasipoisson', se = FALSE)  + ylab( ylab ) + 
    scale_x_continuous( limits = c(1610, 1870) , breaks = seq(1620, 1870, 50)) + 
    xlab(xlab)
  return( p )
}

myTheme = theme_gray() + theme(strip.text.y = element_text(size = 12, face = "italic"), 
                               strip.text.x = element_text(size = 11), 
                               legend.title = element_text(size = 12), 
                               legend.text = element_text(size = 12), 
                               axis.title.y = element_text( size = 12)) 
theme_set(myTheme)
