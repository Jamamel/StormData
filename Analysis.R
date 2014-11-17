# create folder to store downloaded data in chosen working directory
datadir <- paste(getwd(),'/data',sep = '')
dir.create(datadir)

# download data and store in data folder
zipdata <- paste(datadir,'/StormData.csv.bz2',sep = '')
download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2',zipdata)


# unzip and load downloaded data ---------------------------------------------------------
# data will be uploaded and assigned to object "d" of class data.table
# data.table allows for data manipulation by reference, avoiding unnecessary copying and
# memory hoarding of large datasets (like the one in this analysis)
library(data.table)

# read sample to determine column classes for full upload
d <- read.csv(bzfile(zipdata),nrows = 5000, header = T,stringsAsFactors = F)
dim(d)
str(d,1)


cclass <- sapply(d,class)

cclass[c('BGN_TIME','F')] <- 'character'
cclass[cclass %in% 'logical'] <- 'character'

d <- data.table(read.table(bzfile(zipdata),header = T, colClasses = cclass,sep = ',',na.strings = ''))
str(d,1)
summary(d)

# data cleaning -------------------------------------------------------------------------
# column class definition =============================

# use lubridate library to assign date classes among others
library(lubridate)
datecols <- c('BGN_DATE','END_DATE')
d0 <- copy(d)
d[,datecols := lapply(.SD, as.IDate,format = '%m/%d/%y'), .SDcols = datecols ]

