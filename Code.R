##DATA PROCESSING 
library(ggplot2)
library(dplyr)
library(reshape2)

if(!file.exists("/StormData.csv.bz2")){
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",destfile="./StormData.csv.bz2")
}

data <- read.csv(bzfile("StormData.csv.bz2"), sep=",", header=T)

head(data)

table(data$CROPDMGEXP)
table(data$PROPDMGEXP)

data<-data %>% 
  mutate(CROPDMG = CROPDMG *case_when(
    CROPDMGEXP == "B" ~ 10^9,
    CROPDMGEXP == "k" | CROPDMGEXP == "K" ~ 10^3,
    CROPDMGEXP == "m" | CROPDMGEXP == "M" ~ 10^6,
    CROPDMGEXP == 2 ~ 2,
    TRUE ~ 0
  ), CROPDMGEXP=NULL
  )

data<-data %>% 
  mutate(PROPDMG = PROPDMG *case_when(
    PROPDMGEXP == "B" ~ 10^9,
    PROPDMGEXP == "k" | PROPDMGEXP == "K" ~ 10^3,
    PROPDMGEXP == "m" | PROPDMGEXP == "M" ~ 10^6,
    PROPDMGEXP == "h" | PROPDMGEXP == "H" ~ 10^2,
    PROPDMGEXP == 1 ~ 1,
    PROPDMGEXP == 2 ~ 2,
    PROPDMGEXP == 3 ~ 3,
    PROPDMGEXP == 4 ~ 4,
    PROPDMGEXP == 5 ~ 5,
    PROPDMGEXP == 6 ~ 6,
    PROPDMGEXP == 7 ~ 7,
    PROPDMGEXP == 8 ~ 8,
    TRUE ~ 0
  ), PROPDMGEXP=NULL
  )

data <- melt(data, id=c('EVTYPE'), variable.name = "category", value.name = "value",na.rm = TRUE)
##ANALYSIS

health <- aggregate(value ~ EVTYPE, data[data$category %in% c("FATALITIES","INJURIES"),], sum)
health<-health[order(-health$value),][1:10,]

barplot(health$value,names.arg=health$EVTYPE,las=2,col="steelblue", ylab="Damage", main="Top 10 storm event types harmful to population health")

 eco <- aggregate(value ~ EVTYPE + category, data[data$category %in% c("PROPDMG","CROPDMG"),], sum)
  eco<-eco[order(-eco$value),][1:20,]
  
  ggplot(eco, aes(EVTYPE, value/1000000000,colour=factor(category))) + 
    geom_bar(stat="identity") + 
    facet_grid(category ~ .) +
    labs(title = "Top 20 storm events with greatest economic consequences", y = "Damage (bn)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
