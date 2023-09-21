library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)


#a
Raw_Data <- read_excel("RawData.xlsx", sheet = "Raw-Data")
Calendar <- read_excel("RawData.xlsx", sheet = "Calendar")

#b
Raw_Data$'Receipt Date' <- as.Date(Raw_Data$'Receipt Date', format = "%Y/%m/%d")
class(Raw_Data$`Receipt Date`)
Raw_Data$`PO Download Date` <- as.Date(Raw_Data$`PO Download Date`, format = "%Y/%m/%d")
Raw_Data$`Ship Date` <- as.Date(Raw_Data$`Ship Date`, format = "%Y/%m/%d")


# Add Quarter and Year columns to the Raw-Data sheet
Raw_Data <- Raw_Data %>%
  mutate(Year = year(`Receipt Date`),
         Quarter = quarter(`Receipt Date`))

#c

Raw_Data$In_transit_Lead_Time <- difftime(Raw_Data$'Receipt Date', Raw_Data$'Ship Date', units = "days")

Raw_Data$Manufacturing_Lead_Time <- difftime(Raw_Data$'Ship Date', Raw_Data$'PO Download Date', units = "days")

#d
colSums(is.na(Raw_Data))

Raw_Data <- subset(Raw_Data,Raw_Data$Manufacturing_Lead_Time < 100 &Raw_Data$Manufacturing_Lead_Time >=0 |is.na(Raw_Data$Manufacturing_Lead_Time) == TRUE)
Raw_Data <- subset(Raw_Data,Raw_Data$In_transit_Lead_Time>0 |is.na(Raw_Data$In_transit_Lead_Time) == TRUE)

#e
unique(Raw_Data$LOB)
Raw_Data$In_transit_Lead_Time <- as.numeric(Raw_Data$In_transit_Lead_Time)
Raw_Data$Manufacturing_Lead_Time <- as.numeric(Raw_Data$Manufacturing_Lead_Time)
class(Raw_Data$In_transit_Lead_Time)
# different site with different products by using different ship mode should have
#different mean value for In_transit_lead_time and manufacturing_lead_time.
# I already filter the conditions,check one by one,we only have 7 kinds of group below.
new <- Raw_Data %>%
  select(LOB,Origin,`Ship Mode`,In_transit_Lead_Time,Manufacturing_Lead_Time) %>%
  filter(Origin == "Site A"& `Ship Mode`== "OCEAN") 

avg_in = as.integer(mean(new$In_transit_Lead_Time,na.rm = T))
avg_ma = as.integer(mean(new$Manufacturing_Lead_Time,na.rm = T))


new_1<- Raw_Data %>%
  select(LOB,Origin,`Ship Mode`,In_transit_Lead_Time,Manufacturing_Lead_Time) %>%
  filter(Origin == "Site A"& `Ship Mode`== "FASTBOAT")
avg1_in =as.integer(mean(new_1$In_transit_Lead_Time,na.rm = T))
avg1_ma = as.integer(mean(new_1$Manufacturing_Lead_Time,na.rm = T))

new_2<- Raw_Data %>%
  select(LOB,Origin,`Ship Mode`,In_transit_Lead_Time,Manufacturing_Lead_Time) %>%
  filter(Origin == "Site A"& `Ship Mode`== "AIR")
avg2_in = as.integer(mean(new_2$In_transit_Lead_Time,na.rm = T))
avg2_ma = as.integer(mean(new_2$Manufacturing_Lead_Time,na.rm = T))

new_3<- Raw_Data %>%
  select(LOB,Origin,`Ship Mode`,In_transit_Lead_Time,Manufacturing_Lead_Time) %>%
  filter(Origin == "Site B" & `Ship Mode`== "OCEAN")
avg3_in= as.integer(mean(new_3$In_transit_Lead_Time,na.rm = T))
avg3_ma= as.integer(mean(new_3$Manufacturing_Lead_Time,na.rm = T))


new_4<- Raw_Data %>%
  select(LOB,Origin,`Ship Mode`,In_transit_Lead_Time,Manufacturing_Lead_Time) %>%
  filter(Origin == "Site B"& `Ship Mode`== "AIR") 
avg4_in = as.integer(mean(new_4$In_transit_Lead_Time,na.rm = T))
avg4_ma =as.integer(mean(new_4$Manufacturing_Lead_Time,na.rm = T))

new_5<- Raw_Data %>%
  select(LOB,Origin,`Ship Mode`,In_transit_Lead_Time,Manufacturing_Lead_Time) %>%
  filter(Origin == "Site C"& `Ship Mode`== "AIR") 
avg5_in =as.integer(mean(new_5$In_transit_Lead_Time,na.rm = T))
avg5_ma =as.integer(mean(new_5$Manufacturing_Lead_Time,na.rm = T))

new_6<- Raw_Data %>%
  select(LOB,Origin,`Ship Mode`,In_transit_Lead_Time,Manufacturing_Lead_Time) %>%
  filter(Origin == "Site D"& `Ship Mode`== "GROUND") 
avg6_in = as.integer(mean(new_6$In_transit_Lead_Time,na.rm = T))
avg6_ma = as.integer(mean(new_6$Manufacturing_Lead_Time,na.rm = T))


# impute AVG by using different mean values by different conditions.
Raw_Data$In_transit_Lead_Time <- ifelse(Raw_Data$Origin == "Site A"&
                                          Raw_Data$`Ship Mode`=="OCEAN"&is.na(Raw_Data$In_transit_Lead_Time==TRUE),
                                        avg_in,Raw_Data$In_transit_Lead_Time)

Raw_Data$Manufacturing_Lead_Time <- ifelse(Raw_Data$Origin == "Site A"&
                                             Raw_Data$`Ship Mode`=="OCEAN"&is.na(Raw_Data$Manufacturing_Lead_Time==TRUE),
                                           avg_ma,Raw_Data$Manufacturing_Lead_Time)

Raw_Data$In_transit_Lead_Time <- ifelse(Raw_Data$Origin == "Site A"&
                                          Raw_Data$`Ship Mode`=="FASTBOAT"&is.na(Raw_Data$In_transit_Lead_Time==TRUE),
                                        avg1_in,Raw_Data$In_transit_Lead_Time)

Raw_Data$Manufacturing_Lead_Time <- ifelse(Raw_Data$Origin == "Site A"&
                                             Raw_Data$`Ship Mode`=="FASTBOAT"&is.na(Raw_Data$Manufacturing_Lead_Time==TRUE),
                                           avg1_ma,Raw_Data$Manufacturing_Lead_Time)

Raw_Data$In_transit_Lead_Time <- ifelse(Raw_Data$Origin == "Site A"&
                                          Raw_Data$`Ship Mode`=="AIR"&is.na(Raw_Data$In_transit_Lead_Time==TRUE),
                                        avg2_in,Raw_Data$In_transit_Lead_Time)

Raw_Data$Manufacturing_Lead_Time <- ifelse(Raw_Data$Origin == "Site A"&
                                             Raw_Data$`Ship Mode`=="AIR"&is.na(Raw_Data$Manufacturing_Lead_Time==TRUE),
                                           avg2_ma,Raw_Data$Manufacturing_Lead_Time)


Raw_Data$In_transit_Lead_Time <- ifelse(Raw_Data$Origin == "Site B"&
                                          Raw_Data$`Ship Mode`=="OCEAN"&is.na(Raw_Data$In_transit_Lead_Time==TRUE),
                                        avg3_in,Raw_Data$In_transit_Lead_Time)

Raw_Data$Manufacturing_Lead_Time <- ifelse(Raw_Data$Origin == "Site B"&
                                             Raw_Data$`Ship Mode`=="OCEAN"&is.na(Raw_Data$Manufacturing_Lead_Time==TRUE),
                                           avg3_ma,Raw_Data$Manufacturing_Lead_Time)

Raw_Data$In_transit_Lead_Time <- ifelse(Raw_Data$Origin == "Site B"&
                                          Raw_Data$`Ship Mode`=="AIR"&is.na(Raw_Data$In_transit_Lead_Time==TRUE),
                                        avg4_in,Raw_Data$In_transit_Lead_Time)

Raw_Data$Manufacturing_Lead_Time <- ifelse(Raw_Data$Origin == "Site B"&
                                             Raw_Data$`Ship Mode`=="AIR"&is.na(Raw_Data$Manufacturing_Lead_Time==TRUE),
                                           avg4_ma,Raw_Data$Manufacturing_Lead_Time)

Raw_Data$In_transit_Lead_Time <- ifelse(Raw_Data$Origin == "Site C"&
                                          Raw_Data$`Ship Mode`=="AIR"&is.na(Raw_Data$In_transit_Lead_Time==TRUE),
                                        avg5_in,Raw_Data$In_transit_Lead_Time)

Raw_Data$Manufacturing_Lead_Time <- ifelse(Raw_Data$Origin == "Site C"&
                                             Raw_Data$`Ship Mode`=="AIR"&is.na(Raw_Data$Manufacturing_Lead_Time==TRUE),
                                           avg5_ma,Raw_Data$Manufacturing_Lead_Time)

Raw_Data$In_transit_Lead_Time <- ifelse(Raw_Data$Origin == "Site D"&
                                          Raw_Data$`Ship Mode`=="GROUND"&is.na(Raw_Data$In_transit_Lead_Time==TRUE),
                                        avg6_in,Raw_Data$In_transit_Lead_Time)

Raw_Data$Manufacturing_Lead_Time <- ifelse(Raw_Data$Origin == "Site D"&
                                             Raw_Data$`Ship Mode`=="GROUND"&is.na(Raw_Data$Manufacturing_Lead_Time==TRUE),
                                           avg6_ma,Raw_Data$Manufacturing_Lead_Time)

#fill in ship date & receipt date
Raw_Data$In_transit_Lead_Time <- as.difftime(Raw_Data$In_transit_Lead_Time, units ="days")
Raw_Data$Manufacturing_Lead_Time <- as.difftime(Raw_Data$Manufacturing_Lead_Time,units ="days")


Raw_Data <- Raw_Data %>%
  mutate(`Ship Date` =`PO Download Date` + Manufacturing_Lead_Time,
         `Receipt Date` = `Ship Date` + In_transit_Lead_Time,
         Year = year(`Receipt Date`),
         Quarter = quarter(`Receipt Date`))



#f

# Convert In_transit_Lead_Time to numeric format
Raw_Data$In_transit_Lead_Time <- as.numeric(Raw_Data$In_transit_Lead_Time)

# Fit regression model
model <- lm(In_transit_Lead_Time ~ Origin + `Ship Mode`, data = Raw_Data)

summary(model)


ggplot(Raw_Data, aes(x = Origin, y = In_transit_Lead_Time, color = `Ship Mode`)) +
  geom_jitter(width = 0.4, height = 0.4) +
  labs(x = "Origin",
       y = "In-transit Lead Time") +
  ggtitle("In-transit Lead Time by Origin and Ship Mode") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(Raw_Data, aes(x = LOB, y = Manufacturing_Lead_Time, color = `LOB`)) +
  geom_jitter(width = 0.4, height = 0.4) +
  labs(x = "LOB",
       y = "Manufacturing_Lead_Time") +
  ggtitle("Manufacturing Lead Time by LOB") +
  theme(plot.title = element_text(hjust = 0.5))

#g


# Fit regression model
model_1 <- lm(In_transit_Lead_Time ~ LOB + Origin + `Ship Mode` + `PO Download Date` + `Ship Date` + `Receipt Date` + Year + Quarter + Manufacturing_Lead_Time, data = Raw_Data)

summary(model_1)

# Extract coefficients from model summary
coefficients <- coef(summary(model_1))
class(coefficients)
table <- tableGrob(coefficients)
grid.arrange(table)

# List each predictor's correlation with In-transit Lead Time in descending order
table_2 <-as.data.frame(model_1$coefficients[2:14]) 

table_3 <-table_2 %>%
  arrange(desc(table_2))
table_3 <- as.matrix(table_3)
table_3 <- tableGrob(table_3)
grid.arrange(table_3)
















































