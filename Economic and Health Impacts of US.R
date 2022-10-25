

library(tidyr) 
library(dplyr)
library(readr)
library(ggplot2)
# To combine the different plots, in one multiplot.
library(gridExtra)


# set data in your working directory
# read data
storm_data <- read.csv("repdata_data_StormData.csv")
dim(storm_data)
str(storm_data)
View(storm_data)
sum(is.na(storm_data$CROPDMGEXP))
sum(is.na(storm_data$EVTYPE))

# select variable we need 
variables <- c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")
storm_data <-  storm_data[,(names(storm_data) %in% variables)]
storm_data$EVTYPE <- as.factor(storm_data$EVTYPE)
str(storm_data)


# Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health? 

# look at injuries
injuries<-storm_data %>% group_by(EVTYPE) %>% summarise(total=sum(INJURIES)) %>% arrange(desc(total))
head(injuries)
str(injuries)
View(injuries)

# as we see that there is typo so i 'll try to fix it to make the first high rank events more accurate 
# we need to do some fix in events name
storm_data$EVTYPE <- gsub("THUNDERSTORM WIND", "TSTM WIND", storm_data$EVTYPE)
storm_data$EVTYPE <- gsub("THUNDERSTORM WINDS", "TSTM WIND", storm_data$EVTYPE)
storm_data$EVTYPE <- gsub("TSTM WINDS", "TSTM WIND", storm_data$EVTYPE)
storm_data$EVTYPE <- gsub("HIGH WINDS", "HIGH WIND", storm_data$EVTYPE)
storm_data$EVTYPE <- gsub("ICE", "ICE STORM", storm_data$EVTYPE)
storm_data$EVTYPE <- gsub("FOG", "FREEZING FOG", storm_data$EVTYPE)
storm_data$EVTYPE <- gsub("FREEZING FREEZING FOG", "FREEZING FOG", storm_data$EVTYPE)
storm_data$EVTYPE <- gsub("DENSE FREEZING FOG", "DENSE FOG", storm_data$EVTYPE)
storm_data$EVTYPE <- gsub("HEAT WAVE", "HEAT", storm_data$EVTYPE)
storm_data$EVTYPE <- gsub("Heat Wave", "HEAT", storm_data$EVTYPE)
storm_data$EVTYPE <- gsub("EXTREME HEAT", "HEAT", storm_data$EVTYPE)
storm_data$EVTYPE <- gsub("ICE STORM STORM", "ICE STORM", storm_data$EVTYPE)
storm_data$EVTYPE <- gsub("WILD/FOREST FIRE", "WILDFIRE", storm_data$EVTYPE)
storm_data$EVTYPE <- gsub("HURRICANE", "HURRICANE/TYPHOON", storm_data$EVTYPE)
storm_data$EVTYPE <- gsub("HURRICANE/TYPHOON/TYPHOON", "HURRICANE/TYPHOON", storm_data$EVTYPE)

# after editing names of events
storm_data$EVTYPE <- as.factor(storm_data$EVTYPE)

# try it now
injuries<-storm_data %>% group_by(EVTYPE) %>% summarise(total_injuries=sum(INJURIES)) %>% arrange(desc(total_injuries))
injuries$type <- "injuries"
injuries$type <- as.factor(injuries$type)
str(injuries)
head(injuries,5)
View(injuries)

# look at fatalities
fatalities<-storm_data %>% group_by(EVTYPE) %>% summarise(total_fatalities=sum(FATALITIES)) %>% arrange(desc(total_fatalities))
fatalities$type <- "fatalities"
fatalities$type <- as.factor(fatalities$type)
str(fatalities)
head(fatalities,5)
View(fatalities)


# all what i did above is just to make sure to correct right events name that have most damages om health
# now i want to Summarize Multiple Variables & Group by One Variable
# i 'll summarize INJURIES and FATALITIES by events in one table to make it easy to see difference between them in one plot
injwithfatal <- aggregate(cbind(INJURIES,FATALITIES) ~ EVTYPE, data = storm_data , FUN = sum)
str(injwithfatal)

# get top 10 injuries events
top10_by_injureis <- subset(injwithfatal, INJURIES > quantile(INJURIES, prob = 0.99))
head(top10_by_injureis)
View(top10_by_injureis)
# get top 10 fatalities events
top10_by_fatalities <- subset(injwithfatal, FATALITIES > quantile(FATALITIES, prob = 0.99))
head(top10_by_fatalities)
View(top10_by_fatalities)



# now we want to sum injuries and fatalities by event type so i can make a plot to total injuries
storm_data <-  mutate(storm_data,total_impact=INJURIES + FATALITIES)
health_impact <- storm_data %>% group_by(EVTYPE) %>% summarise(total_health_impacts=sum(total_impact)) %>% arrange(desc(total_health_impacts))
str(health_impact)
View(health_impact)



# Across the United States, which types of events have the greatest economic consequences?


table(storm_data$PROPDMGEXP)
table(storm_data$CROPDMGEXP)
sum(is.na(storm_data$CROPDMGEXP))
sum(is.na(storm_data$PROPDMGEXP))

# create new variable CROPFACTOR
storm_data$CROPFACTOR[storm_data$CROPDMGEXP==""] <- 0
storm_data$CROPFACTOR[storm_data$CROPDMGEXP=="?"] <- 0
storm_data$CROPFACTOR[storm_data$CROPDMGEXP=="0"] <- 10^0
storm_data$CROPFACTOR[storm_data$CROPDMGEXP=="2"] <- 10^2
storm_data$CROPFACTOR[storm_data$CROPDMGEXP=="k"] <- 10^3
storm_data$CROPFACTOR[storm_data$CROPDMGEXP=="K"] <- 10^3
storm_data$CROPFACTOR[storm_data$CROPDMGEXP=="m"] <- 10^6
storm_data$CROPFACTOR[storm_data$CROPDMGEXP=="M"] <- 10^6
storm_data$CROPFACTOR[storm_data$CROPDMGEXP=="B"] <- 10^9

# create new variable PROPFACTOR
storm_data$PROPFACTOR[storm_data$PROPDMGEXP==""] <- 0
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="?"] <- 0
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="-"] <- 0
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="+"] <- 10^0
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="0"] <- 10^0
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="1"] <- 10^1
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="2"] <- 10^2
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="3"] <- 10^3
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="4"] <- 10^4
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="5"] <- 10^5
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="6"] <- 10^6
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="7"] <- 10^7
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="8"] <- 10^8
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="H"] <- 10^2
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="h"] <- 10^2
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="K"] <- 10^3
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="m"] <- 10^6
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="M"] <- 10^6
storm_data$PROPFACTOR[storm_data$PROPDMGEXP=="B"] <- 10^9

# now we want to sum crops and prop by event type
storm_data <-  mutate(storm_data,ECONOMICCOST = PROPDMG * PROPFACTOR + CROPDMG * CROPFACTOR)
economic_cost <- storm_data %>% group_by(EVTYPE) %>% summarise(total_economic_cost=sum(ECONOMICCOST)) %>% arrange(desc(total_economic_cost))
str(economic_cost)
head(economic_cost,10)
View(economic_cost)



# plot HEALTH IMPACT

# Creates a list that contains 3 plots, the components of the multiplot.
# use melt fun()
library(reshape2)
Health_Consequences_by_injuries <- melt(top10_by_injureis, id.vars = "EVTYPE", variable.name = "Fatalities_or_Injuries")
head(Health_Consequences_by_injuries)

Health_Consequences_by_fatalities <- melt(top10_by_fatalities, id.vars = "EVTYPE", variable.name = "Fatalities_or_Injuries")
head(Health_Consequences_by_injuries)


components_of_the_multiplot <- list(

      "figure_top_left" = ggplot(Health_Consequences_by_injuries,aes(x=reorder(EVTYPE,-value),y=value))+
                                       geom_col(aes(fill = Fatalities_or_Injuries))+
                                       xlab("Event Types")+
                                       ylab("Value")+
                                       ggtitle("Top 10 Injuries Per Event")+
                                       theme(axis.text.x = element_text(angle=30, hjust=1)),

                                 

      "figure_top_Right" = ggplot(Health_Consequences_by_fatalities,aes(x=reorder(EVTYPE,-value),y=value))+
                                        geom_col(aes(fill = Fatalities_or_Injuries))+
                                        xlab("Event Types")+
                                        ylab("Value")+
                                        ggtitle("Top 10 Fatalities Per Event")+
                                        theme(axis.text.x = element_text(angle=45, hjust=1)),
                                  

      "figure_bottom" = ggplot(health_impact[1:10,],aes(x=reorder(EVTYPE,-total_health_impacts),y=total_health_impacts))+
                                     geom_col(aes(fill='color'))+
                                     xlab("Event Types")+
                                     ylab("Total Health Impacts")+
                                     ggtitle("Total Health Impacts by Event")+
                                     theme(axis.text.x = element_text(angle=30, hjust=1))
                                 

)


multiplot <- grid.arrange(grobs = components_of_the_multiplot,
                          nrow=2)




# plot ECONOMIC COST 
g <-  ggplot(economic_cost[1:10,],aes(x=reorder(EVTYPE,-total_economic_cost),y=total_economic_cost))+
            geom_col()+
            xlab("Event Types")+
            ylab("Economic Cost")+
            ggtitle("Total Economic Cost For Events")
print(g)      
      

      
   
      





