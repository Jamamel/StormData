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
# cleaning patterns for tidying).
healthcols <- c('fatalities','injuries')
healthd <- d[fatalities > 0 & injuries > 0]

healthd[,event := tolower(evtype)]

healthd[grepl('tornado',event),event := 'tornado']
healthd[grepl('hurricane',event),event := 'hurricane']
healthd[grepl('typhoon',event),event := 'hurricane']
healthd[grepl('winter weather',event),event := 'winter weather']
healthd[grepl('cold',event),event := 'winter weather']
healthd[grepl('thunderstorm',event),event := 'thunderstorm']
healthd[grepl('flash flood',event),event := 'flood']
healthd[grepl('heat',event),event := 'heat']
healthd[grepl('high wind',event),event := 'high wind']

healthd[event %in% c('wild fires','wild/forest fire'),event := 'wildfire']
healthd[event == 'rip currents',event := 'rip current']
healthd[event %in% c('tstm wind'),event := 'thunderstorm']


# coverage of events recoded
x <- healthd[,list(sum(fatalities), sum(injuries)),by = event]
x[,damage := V1 + V2]

# coverage % fatalities
y <- x[order(-V1),]; y
sum(y$V1[1:10])/sum(y$V1)

# coverage % injuries
sum(y$V2[1:10])/sum(y$V2)


# check coverages for years 2010 & 2011
x2 <- healthd[year %in% 2010:2011,list(sum(fatalities), sum(injuries)),by = event]
x2 <- x2[,damage := V1 + V2][order(-damage),]

# coverage % fatalities
y2 <- x2[order(-V1),]; y2
sum(y2$V1[1:10])/sum(y2$V1)

# coverage % injuries
sum(y2$V2[1:10])/sum(y2$V2)

# no events with fatalities or injuries have missing beginning date (bgn_date)
healthd[is.na(bgn_date),.N]
healthd[,year := year(bgn_date)]

# unique(healthd$event)
# healthd[,.N,by = event][order(-N),]
tidyhd <- healthd[event %in% healthd[,sum(fatalities),by = event][order(-V1),][1:10,event],]

library(reshape2)
tidyhd <- melt(tidyhd,id.vars = c('refnum','event','bgn_date','year'),measure.vars = c('fatalities','injuries'))

# calculate total(sum) fatalities and injuries by event per year
plothd <- tidyhd[,sum(value), by = c('variable','event','year')]
# plothd <- tidyhd[,mean(value), by = c('variable','event','year')]
setnames(plothd,'V1','frequency')
year <- plothd$year

br <- seq(from = min(year),to = min(year) + floor(diff(range(year))/5)*5 + 5, by = 5)
br[length(br)] <- max(year)
plothd[,year5 := cut(year, br, include.lowest = T)]

plothd[!event %in% y$event[1:5],event := 'other']

plothd <- plothd[,sum(frequency), by = c('variable','event','year5')]
setnames(plothd,'V1','cases')

# plot fatality and injurie timelines, and bars with historics
library(ggplot2)
library(grid)
library(scales)
ggplot(plothd,aes(x = year5,y = cases,group = event, fill = event)) +
  geom_bar(stat = 'identity') +
  ggtitle("U.S. Health Damage by Event per 5-year Period") +
  facet_grid(variable ~ ., scales = "free_y") +
  scale_fill_brewer(palette = 'Set2') +
  scale_x_discrete(name = "") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom",
        panel.margin = unit(2, "lines"),
        panel.background = element_rect(fill = NA),
        strip.text.y = element_text(size=14, face="bold"),
        strip.background = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5, size = 12),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face="bold", size = 16),
        legend.title=element_blank())


