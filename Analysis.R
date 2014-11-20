library(data.table)
library(lubridate)
library(magrittr)
library(reshape2)
library(ggplot2)
library(grid)
library(scales)
library(gridExtra)


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

datecols <- c('bgn_date','end_date')
timecols <- c('end_date','end_time')

d[,eval(datecols) := lapply(.SD, as.IDate,format = '%m/%d/%Y'), .SDcols = datecols]
d[,eval(timecols) := lapply(.SD, as.ITime,format = "%H%M"), .SDcols = timecols]
setkey(d,refnum)

# we recode to NA crop damage multiplier codes with ambiguous interpretations
# (!cropdmgexp %in% c('k','m','b'))
d[,cropdmgexp := tolower(cropdmgexp)]
d[!cropdmgexp %in% c('k','m','b'),cropdmgexp := NA]
d[,.N,by = cropdmgexp]

# we recode to NA property damage multiplier codes with ambiguous interpretations
# (!propdmgexp %in% c('k','m','b'))
d[,propdmgexp := tolower(propdmgexp)]
d[!propdmgexp %in% c('k','m','b'),propdmgexp := NA]
d[,.N,by = propdmgexp]



transtab <- list(list('m',1e+06),
                 list('k',1e+03),
                 list('b',1e+09)) %>%
  rbindlist

setnames(transtab, old = c('V1','V2'), new = c('code', 'value'))
setkey(transtab,code)

# translsate the crop damage multiplier to numerical
d[,numcropexp := transtab[cropdmgexp,'value', with = F]]
summary(d$numcropexp)

# calculate numeric crop damage (numcropexp * cropdmg)
d[,crop := (numcropexp * cropdmg)]
summary(d$crop)



# translsate the property damage multiplier to numerical
d[,numpropexp := transtab[propdmgexp,'value', with = F]]
summary(d$numpropexp)

# calculate numeric property damage (numcropexp * cropdmg)
d[,property := (numpropexp * propdmg)]
summary(d$property)


# begin analysis to determine damage to health -------------------------------------------
# if damage to health is measured purely by positive counts of fatalities or injuries the dataset
# is greatly redueced. This means fewer un-tidy event type labels to deal with (easier to identify
# cleaning patterns for tidying).


# begin analysis to determine economic damage -------------------------------------------
# if damage to health is measured purely by positive counts of fatalities or injuries the dataset
# is greatly redueced. This means fewer un-tidy event type labels to deal with (easier to identify
# cleaning patterns for tidying).

describeDmg <- function(d, damagecols, damage, yname, yfrmt){

  subsform <- paste(damagecols[1], ' > 0 & ', damagecols[2],' > 0', sep = '')

  dmgdt <- d[eval(parse(text = subsform))]
  dmgdt[,.N,by = evtype][order(-N),]

  dmgdt[,event := tolower(evtype)]
  dmgdt[,.N,by = event][order(-N),]

  dmgdt[grepl('tornado',event),event := 'tornado']
  dmgdt[grepl('hurricane',event),event := 'hurricane']
  dmgdt[grepl('typhoon',event),event := 'hurricane']
  dmgdt[grepl('winter weather',event),event := 'winter weather']
  dmgdt[grepl('cold',event),event := 'winter weather']
  dmgdt[grepl('thunderstorm',event),event := 'thunderstorm']
  dmgdt[grepl('flood',event),event := 'flood']
  dmgdt[grepl('heat',event),event := 'heat']
  dmgdt[grepl('high wind',event),event := 'high wind']

  dmgdt[event %in% c('wild fires','wild/forest fire'),event := 'wildfire']
  dmgdt[event == 'rip currents',event := 'rip current']
  dmgdt[event %in% c('tstm wind'),event := 'thunderstorm']

  # coverage of events recoded
  sumsform <- paste('list(sum(', damagecols[1], '), sum(', damagecols[2],'))', sep = '')
  x <- dmgdt[,eval(parse(text = sumsform)),by = event]
  x[,eval(parse(text = damage)) := V1 + V2]

  # coverage % fatalities
  y <- x[order(-V1),]; y
  sum(y$V1[1:10])/sum(y$V1)

  # coverage % injuries
  sum(y$V2[1:10])/sum(y$V2)

  # no events with fatalities or injuries have missing beginning date (bgn_date)
  dmgdt[is.na(bgn_date),.N]
  dmgdt[,year := year(bgn_date)]


  # check coverages for years 2010 & 2011
  x2 <- dmgdt[year %in% 2010:2011,eval(parse(text = sumsform)),by = event]
  x2 <- x2[,damage := V1 + V2][order(-damage),]

  # coverage % fatalities
  y2 <- x2[order(-V1),]; y2
  sum(y2$V1[1:10])/sum(y2$V1)

  # coverage % injuries
  sum(y2$V2[1:10])/sum(y2$V2)

  dmgcol1 <- paste('sum(',damagecols[1],')',sep = '')
  dmgcol2 <- paste('sum(',damagecols[2],')',sep = '')

  tidyhd1 <- dmgdt[event %in% dmgdt[,eval(parse(text = dmgcol1)),by = event][order(-V1),][1:10,event],]
  tidyhd2 <- dmgdt[event %in% dmgdt[,eval(parse(text = dmgcol2)),by = event][order(-V1),][1:10,event],]
  tidyhd0 <- unique(rbind(tidyhd1,tidyhd2))
  setkey(tidyhd0,refnum)


  library(reshape2)
  tidyhd <- melt(tidyhd0,id.vars = c('refnum','event','bgn_date','year'),measure.vars = damagecols)

  # calculate total(sum) fatalities and injuries by event per year
  plothd <- tidyhd[,sum(value), by = c('variable','event','year')]
  setnames(plothd,'V1','frequency')
  year <- plothd$year

  br <- seq(from = min(year),to = min(year) + floor(diff(range(year))/3)*3 + 3, by = 3)
  br[length(br)] <- max(year)
  br <- unique(br)
  plothd[,year3 := cut(year, br, include.lowest = T)]

  plothd[!event %in% union(y$event[1:4], y2$event[1:4]),event := 'other']

  plothd <- plothd[,sum(frequency), by = c('variable','event','year3')]
  setnames(plothd,'V1','frequency')


  # plot fatality and injurie timelines, and bars with historics


  capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
sep = "", collapse = " " )
sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  }

title <- paste('U.S.',capwords(damage), ' Damage by Event per 3-year Period')
p <- ggplot(plothd,aes(x = year3,y = frequency,group = event, fill = event)) +
  geom_bar(stat = 'identity') +
  ggtitle(title) +
  facet_grid(variable ~ ., scales = "free_y") +
  scale_fill_brewer(palette = 'Set2') +
  scale_x_discrete(name = "") +
  scale_y_continuous(labels = eval(parse(text = yfrmt)), name = yname) +
  theme(legend.position = "bottom",
        panel.margin = unit(2, "lines"),
        panel.background = element_rect(fill = NA),
        strip.text.y = element_text(size=14, face="bold"),
        strip.background = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5, size = 12),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face="bold", size = 16),
        legend.title=element_blank())
p
list(dt = tidyhd0, viz = p)

}

ecodmg <- describeDmg(d,c('crop','property'),'economic', 'value', 'dollar')
hlthdmg <- describeDmg(d,c('fatalities','injuries'),'health', 'cases', 'comma')

eco <- copy(ecodmg[[1]])
hlt <- copy(hlthdmg[[1]])

hltmask <- hlt[!refnum %in% eco$refnum,'refnum',with = F]

x <- rbind(eco,hlt[hltmask,])
x[,health := fatalities + injuries]
x[,economic := crop + property]

finalplotdt  <- x[,lapply(.SD,mean,na.rm = T), .SDcols = c('health','economic'), by = 'event']

finalplotdt[grepl('tornado',event),event := 'tornado']
finalplotdt[grepl('hurricane',event),event := 'hurricane']
finalplotdt[grepl('typhoon',event),event := 'hurricane']
finalplotdt[grepl('winter weather',event),event := 'winter weather']
finalplotdt[grepl('cold',event),event := 'winter weather']
finalplotdt[grepl('thunderstorm',event),event := 'thunderstorm']
finalplotdt[grepl('flood',event),event := 'flood']
finalplotdt[grepl('heat',event),event := 'heat']
finalplotdt[grepl('high wind',event),event := 'high wind']

finalplotdt[event %in% c('wild fires','wild/forest fire'),event := 'wildfire']
finalplotdt[event == 'rip currents',event := 'rip current']
finalplotdt[event %in% c('tstm wind'),event := 'thunderstorm']
finalplotdt <- finalplotdt[health > 0 & economic > 0]
# finalplotdt <- x[health > 0 & economic > 0]


p <- ggplot(finalplotdt,aes(x = health, y = economic)) +
  geom_point(colour = 'red', size = 5) +
  scale_shape(solid = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(stat = 'smooth', method = 'lm') +
  geom_text(aes(label=event), vjust = -1) +
  ggtitle('U.S. All-Time Mean Health vs. Economic Damage') +
  theme(plot.title = element_text(size = 14, lineheight = -1, face="bold", vjust = 1.5),
        panel.background = element_rect(fill = NA),
        panel.margin = unit(2, "lines"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face="bold", size = 16),
        legend.title=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14))

g <- arrangeGrob(p, sub = textGrob("Note: log10 transofmration applied to both axes - 95% confidence interval.", x = 0, hjust = -0.1, vjust=-0.5, gp = gpar(fontface = "italic", fontsize = 10)))
# ggsave("/Users/Alan/Desktop/plot_grid_extra.png", g)
