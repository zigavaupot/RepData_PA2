library(knitr)
library(stringdist)
library(ggplot2)
library(reshape2)
library(grid)


#setwd("/Users/zigavaupot/Documents/99 Training/Coursera - Data Science/05 Reproducable Research/assignement2/")

if(!file.exists("./repdata-data-StormData.csv.bz2")) {
        fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(fileURL, "repdata-data-StormData.csv.bz2")
}
#data <- read.csv(bzfile("repdata-data-StormData.csv.bz2"))
dataStorms <- data[c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP", "STATE")]

str(dataStorms)

# Prepare event types

#print(levels(dataStorms$EVTYPE))

#print(length(levels(dataStorms$EVTYPE)))

# EVTYPEs are in upper and mixed cases, there are also leading spaces in some cases

#print(levels(dataStorms$EVTYPE))

#print(length(levels(dataStorms$EVTYPE)))

#dataStorms$EVTYPE <- as.character(dataStorms$EVTYPE)
dataStorms$EVTYPE <- toupper(dataStorms$EVTYPE)
trimSpaces <- function(x) gsub("^\\s+|\\s+$","", x)
dataStorms$EVTYPE <- trimSpaces(dataStorms$EVTYPE)

#print(unique(dataStorms$EVTYPE))

#print(length(unique(dataStorms$EVTYPE)))

# read event types table

dataEventTypes <- read.csv("event_types.txt", header=TRUE)
eventTypes <- toupper(dataEventTypes$EVENT_TYPE)

# find match for each EVTYPE in dataStorms with EVENT_TYPE in eventTypes using Laenshtein distance (method="lv") which counts the number of deletions, insertions and substitutions necessary to turn b into a. 
dataStorms$EVTYPE_NORM <- eventTypes[amatch(dataStorms$EVTYPE, eventTypes, method="lv", maxDist=30)]

print(unique(dataStorms$EVTYPE_NORM))

print(length(unique(dataStorms$EVTYPE_NORM)))

# 1st question:Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
# Attributes:
#       - EVTYPE
#       - FATALITIES
#       - INJURIES

#agg1.total <- aggregate(FATALITIES + INJURIES ~ EVTYPE_NORM, data=dataStorms, FUN="sum")
#agg1.fatalities <- aggregate(FATALITIES ~ EVTYPE_NORM, data=dataStorms, FUN="sum")
#agg1.injuries <- aggregate(INJURIES ~ EVTYPE_NORM, data=dataStorms, FUN="sum")

tableAgg <- aggregate(dataStorms[c("FATALITIES", "INJURIES")], list(EVENT_TYPE=dataStorms$EVTYPE_NORM), FUN=sum)
tableAgg2 <- tableAgg[order(tableAgg$FATALITIES + tableAgg$INJURIES, decreasing=TRUE),][1:10,]
tableAgg2$EVENT_TYPE <- reorder(tableAgg2$EVENT_TYPE, -rowSums(tableAgg2[-1]))

tableAgg2.melt <- melt(tableAgg2, id.var="EVENT_TYPE")


g1 <- ggplot(tableAgg2.melt, aes(x=EVENT_TYPE, y=value, fill=variable)) 
g1 <- g1 + 
        geom_bar(position="dodge", stat="identity") +
        scale_fill_brewer(palette="Set1") +
        ylab("Total Fatalities/Injuries") + xlab("Event type") +
        ggtitle("The top 10 most harmful events wiht respect to population health ")+
        theme(plot.title = element_text(face="bold", size=16, vjust=2, hjust=1))+
        theme(axis.text.x = element_text(angle=90))+
        theme(plot.margin = unit(c(1,1,1,1), "cm"))
print(g1)


#levels(data$CROPDMGEXP)

#levels(data$PROPDMGEXP)


# 2nd question:Across the United States, which types of events have the greatest economic consequences?
# Attributes:
#       - EVTYPE
#       - CROPDMG
#       - CROPDMGEXP
#       - DROPDMG
#       - DROPDMGEXP


dataStorms$PROPDMGEXP <- as.character(dataStorms$PROPDMGEXP)
dataStorms$PROPDMGEXP[dataStorms$PROPDMGEXP==""] <- "0"
dataStorms$PROPDMGEXP <- gsub("\\-|\\?", "0", dataStorms$PROPDMGEXP)
dataStorms$PROPDMGEXP <- gsub("\\+", "1", dataStorms$PROPDMGEXP)
dataStorms$PROPDMGEXP <- gsub("H|h", "2", dataStorms$PROPDMGEXP)
dataStorms$PROPDMGEXP <- gsub("K|k", "3", dataStorms$PROPDMGEXP)
dataStorms$PROPDMGEXP <- gsub("M|m", "6", dataStorms$PROPDMGEXP)
dataStorms$PROPDMGEXP <- gsub("B|b", "9", dataStorms$PROPDMGEXP)
for(i in 0:9) {dataStorms$damageProperties[dataStorms$PROPDMGEXP==i] <- dataStorms$PROPDMG[dataStorms$PROPDMGEXP==i]*10^i}

dataStorms$CROPDMGEXP <- as.character(dataStorms$CROPDMGEXP)
dataStorms$CROPDMGEXP[dataStorms$CROPDMGEXP==""] <- "0"
dataStorms$CROPDMGEXP <- gsub("\\-|\\?", "0", dataStorms$CROPDMGEXP)
dataStorms$CROPDMGEXP <- gsub("\\+", "1", dataStorms$CROPDMGEXP)
dataStorms$CROPDMGEXP <- gsub("H|h", "2", dataStorms$CROPDMGEXP)
dataStorms$CROPDMGEXP <- gsub("K|k", "3", dataStorms$CROPDMGEXP)
dataStorms$CROPDMGEXP <- gsub("M|m", "6", dataStorms$CROPDMGEXP)
dataStorms$CROPDMGEXP <- gsub("B|b", "9", dataStorms$CROPDMGEXP)
for(i in 0:9) {dataStorms$damageCrops[dataStorms$CROPDMGEXP==i] <- dataStorms$CROPDMG[dataStorms$CROPDMGEXP==i]*10^i}

tableAggD <- aggregate(dataStorms[c("damageProperties", "damageCrops")], list(EVENT_TYPE=dataStorms$EVTYPE_NORM), FUN=sum)
tableAggD2 <- tableAggD[order(tableAggD$damageProperties + tableAggD$damageCrops, decreasing=TRUE),][1:10,]
tableAggD2$EVENT_TYPE <- reorder(tableAggD2$EVENT_TYPE, -rowSums(tableAggD2[-1]))
tableAggD2$damageProperties <- tableAggD2$damageProperties / 1000000000
tableAggD2$damageCrops <- tableAggD2$damageCrops / 1000000000


tableAggD2.melt <- melt(tableAggD2, id.var="EVENT_TYPE")

g2 <- ggplot(tableAggD2.melt, aes(x=EVENT_TYPE, y=value, fill=variable)) 
g2 <- g2 + 
        geom_bar(stat="identity") +
        scale_fill_brewer(palette="Set1") +
        ylab("Damage in billion US dollars") + xlab("Event type") +
        ggtitle("The top 10 events with greatest economic consequences")+
        theme(plot.title = element_text(face="bold", size=16, vjust=2, hjust=1))+
        theme(axis.text.x = element_text(angle=90))+
        theme(plot.margin = unit(c(1,1,1,1), "cm"))
print(g2)

# print scatter plot

# identify top 5 states by events

states <- aggregate(dataStorms$STATE, by=list(dataStorms$STATE), FUN=NROW)
names(states) <- c("STATE", "NUMBER_OF_EVENTS")
states <- states[order(states$NUMBER_OF_EVENTS, decreasing=TRUE),][1:5,]
states$STATE <- reorder(states$STATE, -rowSums(states[-1]))

# filter on top 5 events and top 5 states
tableAggS <- dataStorms[dataStorms$EVTYPE_NORM %in% tableAgg2[1:5,"EVENT_TYPE"] & dataStorms$STATE %in% states$STATE ,]

tableAggS$DAMAGE <- (tableAggS$damageProperties + tableAggS$damageCrops) / 1000000
tableAggS$POPULATION_AFFECTED <- tableAggS$FATALITIES + tableAggS$INJURIES

#tableAggS <- dataStorms[dataStorms$EVTYPE_NORM %in% tableAgg2[1:5,"EVENT_TYPE"],]
tableAggS2 <- aggregate(tableAggS[c("DAMAGE", "POPULATION_AFFECTED")],
              by=list(tableAggS$EVTYPE_NORM, tableAggS$STATE), FUN=sum)
names(tableAggS2) <- c("EVENT_TYPE", "STATE", "DAMAGE", "POPULATION_AFFECTED")

s <- ggplot(tableAggS2, aes(x=DAMAGE, y=POPULATION_AFFECTED))
s <- s + geom_point(aes(color=STATE, shape=EVENT_TYPE), size = 4) +
        xlab("Damage in billion US dollars") + ylab("Population affected (fatalities & injuries)") +
        ggtitle("Damage and population affected by top 5 states and top 5 event types")+
        theme(plot.title = element_text(face="bold", size=16, vjust=2, hjust=1))+
        theme(plot.margin = unit(c(1,1,1,1), "cm"))
#+ geom_text(aes(label=STATE), size=3)
print(s)


statesE <- aggregate(dataStorms$STATE, by=list(STATE=dataStorms$STATE), FUN=NROW)
statesDP <- aggregate(dataStorms[c("damageProperties", "damageCrops","FATALITIES", "INJURIES")], by=list(STATE=dataStorms$STATE), FUN=sum)
statesAgg <- cbind(statesDP, statesE$x)
names(statesAgg) <- c("STATE","damageProperties", "damageCrops","FATALITIES", "INJURIES", "NUMBER_OF_EVENTS")
statesAgg$DAMAGE <- (statesAgg$damageCrops + statesAgg$damageProperties) / 1000000000
statesAgg$POPULATION_AFFECTED <- (statesAgg$FATALITIES + statesAgg$INJURIES) / 1000

s2 <- ggplot(statesAgg, aes(x=DAMAGE, y=POPULATION_AFFECTED, size=NUMBER_OF_EVENTS,label=STATE), guide=FALSE) + 
        geom_point(colour="white", fill="red", shape=21) + 
        scale_size_area(max_size = 20)+
        geom_text(size=3) +
        scale_x_continuous(name="Damage in billion US dollars", limits=c(0,150))+
        scale_y_continuous(name="Population affected (fatalities & injuries) in 000s", limits=c(0,20))
        
        
print(s2)

#s2 <- ggplot(statesAgg, aes(x=DAMAGE, y=POPULATION_AFFECTED, label=STATE), guide=FALSE)
#s2 <- s2 + geom_point(aes(size=NUMBER_OF_EVENTS, colour="white", fill="red", shape=21)) #+
#        scale_size_area(max_size = 15) +
#        scale_x_continuous(name="Damage in billion US dollars", limits=c(0,10000))+
#        scale_y_continuous(name="Population affected (fatalities & injuries)", limits=c(0,5000))+
#        geom_text(size=4) +
#        ggtitle("Damage and population affected by top 5 states and top 5 event types") +
#        theme(plot.title = element_text(face="bold", size=16, vjust=2, hjust=1))+
#        theme(plot.margin = unit(c(1,1,1,1), "cm"))
#+ geom_text(aes(label=STATE), size=3)
#print(s2)
