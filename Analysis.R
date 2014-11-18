# create folder to store downloaded data in chosen working directory
datadir <- paste(getwd(),'/data',sep = '')
dir.create(datadir)

# download data and store in data folder
zipdata <- paste(datadir,'/StormData.csv.bz2',sep = '')
# download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2',zipdata)


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
cclass[c('BGN_TIME','END_TIME','F')] <- 'character'
cclass[cclass %in% 'logical'] <- 'character'

d <- data.table(read.table(bzfile(zipdata),header = T, colClasses = cclass,sep = ',',na.strings = ''))
setnames(d, old = colnames(d), new = tolower(colnames(d)))
str(d,1)
summary(d)

# data cleaning -------------------------------------------------------------------------
# column class definition =============================

# use lubridate library to assign date classes among others
library(lubridate)
datecols <- c('bgn_date','end_date')
timecols <- c('end_date','end_time')

d[,eval(datecols) := lapply(.SD, as.IDate,format = '%m/%d/%Y'), .SDcols = datecols]
d[,eval(timecols) := lapply(.SD, as.ITime,format = "%H%M"), .SDcols = timecols]
setkey(d,refnum)


# begin analysis to determine damage to health -------------------------------------------
# if damage to health is measured purely by positive counts of fatalities or injuries the dataset
# is greatly redueced. This means fewer un-tidy event type labels to deal with (easier to identify
# cleaning patterns for cleaning).
healthcols <- c('fatalities','injuries')
healthd <- d[fatalities > 0 & injuries > 0]

healthd[,event := tolower(evtype)]
healthd[event == 'rip currents',event := 'rip current']
healthd[event == 'flash flood',event := 'flood']
healthd[event %in% c('tstm wind','thunderstorm winds'),event := 'thunderstorm wind']

# unique(healthd$event)
# healthd[,.N,by = event][order(-N),]
tidyhd <- healthd[event %in% healthd[,sum(fatalities),by = event][order(-V1),][1:10,event],]

library(reshape2)
tidyhd <- melt(tidyhd,id.vars = c('refnum','event','bgn_date'),measure.vars = c('fatalities','injuries'))
tidyhd[,year := year(bgn_date)]
plothd <- tidyhd[,sum(value), by = c('variable','event','year')]
# plothd <- tidyhd[,mean(value), by = c('variable','event','year')]
setnames(plothd,'V1','value')
plothd[!event %in% x$event[1:5],event := 'other']
# plothd[,tornado := value]
# plothd[event != 'tornado',tornado := NA]
# plothd[,heat := value]
# plothd[event != 'excessive heat',heat := NA]


# coverage of events recoded
x <- healthd[,sum(fatalities),by = event][order(-V1),]
sum(x$V1[1:10])/sum(x$V1)

# plot fatality and injurie timelines, and bars with historics
library(ggplot2)
ggplot(plothd,aes(x = year,y = value,group = event, fill = event)) +
  geom_bar(stat = 'identity') + facet_grid(variable ~ ., scales = "free") +
  scale_fill_brewer(palette = 'RdBu')
#   geom_bar(stat = 'identity',aes(y = tornado), lwd = 1.3, fill = "red") +
#   geom_bar(stat = 'identity',aes(y = heat), lwd = 1.3, fill = "green")
names(healthd)
