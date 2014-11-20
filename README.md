# StormData - Identifying the health & economical impact of severe weather events
Jamamel  
Thursday, November 20, 2014  

# Synopsis

The following analysis aims to assess the impact of sever weather events (e.g. tornadoes, floods) through recorded health & economic damaged observed in the U.S over from 1950 through 2011. The dataset used is the  U.S. National Oceanic and Atmospheric Administration's (NOAA) [storm database](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2). Recorded fatality, injury,property & crop damage entries across the country are broken down to identify leading events responsible historically for these losses. We present the top types of events to which these damages are associated so that budget allocation in prevention and reaction in disaster management is better allocated.This analysis is meant for the sole purpose of visualizing said distributions and relationships. No models or explicit recommendations are produced, but will hopefully help identify areas where plausible hypothesis can more thouroughly investigated in future endeavours.

# Questions

We aim to address two specific questions, though other corollary analyses are also presented:

- Across the United States, which types of events are most harmful with respect to population health?
- Across the United States, which types of events have the greatest economic consequences?

# Data Processing

For information about the data model, design and analyses performed to produce the storm data used, please refer to:

- [National Weather Service Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)
- [National Climatic Data Center Storm Events FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)

All required R packages are loaded and data downloaded from the mentioned url and loaded into R


```r
library(knitr)
library(data.table)
library(lubridate)
library(magrittr)
library(reshape2)
library(ggplot2)
library(grid)
library(scales)
library(gridExtra)
```




```r
# create folder to store downloaded data in chosen working directory
datadir <- paste(getwd(),'/data',sep = '')
suppressWarnings(dir.create(datadir))

# download data and store in data folder
zipdata <- paste(datadir,'/StormData.csv.bz2',sep = '')
# download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2',zipdata)


# unzip and load downloaded data ---------------------------------------------------------
# data will be uploaded and assigned to object "d" of class data.table
# data.table allows for data manipulation by reference, avoiding unnecessary copying and
# memory hoarding of large datasets (like the one in this analysis)


# read sample to determine column classes for full upload
d <- read.csv(bzfile(zipdata),nrows = 5000, header = T,stringsAsFactors = F)

cclass <- sapply(d,class)
cclass[c('BGN_TIME','END_TIME','F')] <- 'character'
cclass[cclass %in% 'logical'] <- 'character'

# read full dataset after classes have been identified
# recode all column names to lower case for ease of use and to match coding standards
d <- data.table(read.table(bzfile(zipdata),header = T, colClasses = cclass,sep = ',',na.strings = ''))
setnames(d, old = colnames(d), new = tolower(colnames(d)))
str(d,1)
```

```
## Classes 'data.table' and 'data.frame':	902297 obs. of  37 variables:
##  $ state__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ bgn_date  : chr  "4/18/1950 0:00:00" "4/18/1950 0:00:00" "2/20/1951 0:00:00" "6/8/1951 0:00:00" ...
##  $ bgn_time  : chr  "0130" "0145" "1600" "0900" ...
##  $ time_zone : chr  "CST" "CST" "CST" "CST" ...
##  $ county    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ countyname: chr  "MOBILE" "BALDWIN" "FAYETTE" "MADISON" ...
##  $ state     : chr  "AL" "AL" "AL" "AL" ...
##  $ evtype    : chr  "TORNADO" "TORNADO" "TORNADO" "TORNADO" ...
##  $ bgn_range : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ bgn_azi   : chr  NA NA NA NA ...
##  $ bgn_locati: chr  NA NA NA NA ...
##  $ end_date  : chr  NA NA NA NA ...
##  $ end_time  : chr  NA NA NA NA ...
##  $ county_end: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ countyendn: chr  NA NA NA NA ...
##  $ end_range : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ end_azi   : chr  NA NA NA NA ...
##  $ end_locati: chr  NA NA NA NA ...
##  $ length    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ width     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ f         : chr  "3" "2" "2" "2" ...
##  $ mag       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ fatalities: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ injuries  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ propdmg   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ propdmgexp: chr  "K" "K" "K" "K" ...
##  $ cropdmg   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ cropdmgexp: chr  NA NA NA NA ...
##  $ wfo       : chr  NA NA NA NA ...
##  $ stateoffic: chr  NA NA NA NA ...
##  $ zonenames : chr  NA NA NA NA ...
##  $ latitude  : num  3040 3042 3340 3458 3412 ...
##  $ longitude : num  8812 8755 8742 8626 8642 ...
##  $ latitude_e: num  3051 0 0 0 0 ...
##  $ longitude_: num  8806 0 0 0 0 ...
##  $ remarks   : chr  NA NA NA NA ...
##  $ refnum    : num  1 2 3 4 5 6 7 8 9 10 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```


We then transform beginning & end date & time variables to proper date formats.


```r
datecols <- c('bgn_date','end_date')
timecols <- c('end_date','end_time')

d[,eval(datecols) := lapply(.SD, as.IDate,format = '%m/%d/%Y'), .SDcols = datecols]
d[,eval(timecols) := lapply(.SD, as.ITime,format = "%H%M"), .SDcols = timecols]
setkey(d,refnum)
```




In order to translate property and crop damages to numerical values, we must create a look-up table for values *k, m,* & *b* (multipliers for thousands, millions, and billions).


```r
# we recode to NA crop damage multiplier codes with ambiguous interpretations
# (!cropdmgexp %in% c('k','m','b'))
d[,cropdmgexp := tolower(cropdmgexp)]
d[!cropdmgexp %in% c('k','m','b'),cropdmgexp := NA]


# we recode to NA property damage multiplier codes with ambiguous interpretations
# (!propdmgexp %in% c('k','m','b'))
d[,propdmgexp := tolower(propdmgexp)]
d[!propdmgexp %in% c('k','m','b'),propdmgexp := NA]


transtab <- list(list('m',1e+06),
                 list('k',1e+03),
                 list('b',1e+09)) %>%
  rbindlist

setnames(transtab, old = c('V1','V2'), new = c('code', 'value'))
setkey(transtab,code)
transtab
```


```
##    code value
## 1:    b 1e+09
## 2:    k 1e+03
## 3:    m 1e+06
```

