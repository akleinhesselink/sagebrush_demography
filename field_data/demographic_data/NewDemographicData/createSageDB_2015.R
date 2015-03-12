#### initialization script 
#### runs all the earlier scripts to create the sagebrush database 
#### need to be run in this order 
####
#### needs to be in the same directory as raw excel and csv data files 
####  

library(xlsx)
library(RSQLite)
library(ggplot2)

setwd('~/Documents/Kleinhesselink/Artemisia_tripartita_project/field_data/demographic_data/NewDemographicData/')

source('reCreateSageDB.R') #### incorporates data from fall 2012 and spring 2013
source('summer2013StatusUpdate.R') #### mid-summer survival data (not collected in 2014)
source('fall2013Update.R') #### fall 2013 survival, growth and flowering data 
source('earlySpring2014Update.R') #### early spring data on survival only 
source('spring2014FallTransplantsUpdate.R') #### transplants only 
source('spring2014Update.R') #### main spring data on survival and growth
source('Fall2014Update.R')  #### all fall 2014 survival, growth and flowering data 
source('makeFlowersTable.R') #### initiate flowers table for database with 2013 data 
source('UpdateFlowersTable.R') #### add Fall 2014 data to table



