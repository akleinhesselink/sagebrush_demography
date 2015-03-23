#!usr/bin/Rscript
#### script for batch conversion of XLS and XLSX file to CSV 
#### Run from the command line to convert all xlsx files in folder
#### to csv files with the same name.  csv files are placed in the 
#### current working directory.
####
#### usage example: 
####   > Rscript convert_xls2csv.R <foldername>

require( xlsx)

XLSXpattern = '*\\.xlsx$'

wd = system(command='pwd', intern = TRUE)
setwd(wd)

args = commandArgs( trailingOnly=TRUE)

fileList = dir(path = args, pattern= XLSXpattern)

startTime = Sys.time()
count = 0

for (f in fileList) { 
  print(paste('converting ', f))
  path = file.path( args, f)
  tempDat = read.xlsx2( path, sheetIndex= 1)
  write.table(x= tempDat, file = gsub(f, pattern= XLSXpattern, replacement = '.csv'), row.names = FALSE, sep = ',')
  count = count + 1 
}

print( paste( count, 'files converted in', Sys.time() - startTime) )

