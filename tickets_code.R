rm(list=ls()) 
library(tidyr)
library(tidyverse)
library(ggplot2)
library(sjPlot)
library(foreign)
library(stats)
library(margins)
library(zoo)
library(lmtest)
library(censReg)
library(AER)
library('dplyr')

library(readxl)
#1. read the data
price <- read_xlsx("All Tickets.xlsx")


#2.data summary
library(vtable)
library(psych) 
library(Hmisc)
describe(price)

#2.1 how many primary and resale among stubhub and ticketmaster?
stubhub <- price[price$Source=="Stubhub",]
stubhub %>% 
  group_by(stubhub$`Resale/Primary`) %>%
  summarise(count=n())

ticketmaster <- price[price$Source=="Ticketmaster",]
ticketmaster %>%
  group_by(ticketmaster$`Resale/Primary`)%>%
  summarise(count=n())


ggplot(price,aes(x=Source,fill=`Resale/Primary`))+
  geom_bar(stat = "count")+
  labs(title = "Source vs Resale/Primary", subtitle =" ",caption = '', 
       x="Source", y="Count")+theme_bw()

#2.2 explore the price avg, max, min among resale/primary

price%>%
  group_by(price$Game,price$`Resale/Primary`)%>%
  summarise(max=max(`Price ($)`,na.rm=TRUE),
            min=min(`Price ($)`,na.rm=TRUE),
            avg=mean(`Price ($)`,na.rm=TRUE),
            median=median(`Price ($)`,na.rm=TRUE))
#2.3  explore the distribution of price
ggplot(price)+
  geom_density(aes(`Price ($)`,colour=Game))+
  xlim(0,150)+
  labs(title="Kernel Density of Ticket Price by Game")


#2.3 price with distance among games and market
distance_price <-price%>%
  group_by(price$Game,price$Distance)%>%
  summarise(max=max(`Price ($)`,na.rm=TRUE),
            min=min(`Price ($)`,na.rm=TRUE),
            avg=mean(`Price ($)`,na.rm=TRUE),
            median=median(`Price ($)`,na.rm=TRUE))
ggplot(distance_price)+
  geom_line(aes(x=`price$Distance`,y= avg,colour= `price$Game`))+
  scale_x_continuous(breaks = seq(0, 25, by = 1))

ggplot(distance_price)+
  geom_line(aes(x=`price$Distance`,y= median,colour= `price$Game`))+
  scale_x_continuous(breaks = seq(0, 25, by = 1))+
  ylim(20,80)

#2.4 price frequency and outliers
price_frq <- table(price$`Price ($)`)
price_frq <- as.data.frame(price_frq)


ggplot(price_frq, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Frequency of Prices",
       x = "Price",
       y = "Frequency")

sum(price_frq$Freq[1:8])
sum(price_frq$Freq[9:19])
sum(price_frq$Freq[20:29])
sum(price_frq$Freq[30:39])
sum(price_frq$Freq[40:49])
sum(price_frq$Freq[50:59])
sum(price_frq$Freq[60:69])
sum(price_frq$Freq[70:79])
sum(price_frq$Freq[80:89])
sum(price_frq$Freq[90:99])
sum(price_frq$Freq[100:109])
sum(price_frq$Freq[110:119])
sum(price_frq$Freq[120:132])
sum(price_frq$Freq[133:146])
sum(price_frq$Freq[147:182])
sum(price_frq$Freq[183:213])
sum(price_frq$Freq[214:281])
sum(price_frq$Freq[282:293])

#2.5 filter miami heat and toronto
miami <- price[price$Game=="vs Miami Heat",]





#2.6
section_frq <- table(price$Section)
section_frq <- as.data.frame(section_frq)
sum(section_frq$Freq[1:11])
sum(section_frq$Freq[12:41])
sum(section_frq$Freq[42:75])

#2.7

d_100 <- price[price$Distance==0&price$Section<199&price$Section>=100,]

max(d_100$`Price ($)`,na.rm = T)
min(d_100$`Price ($)`,na.rm = T)
mean(d_100$`Price ($)`,na.rm = T)

d_200 <- price[price$Distance==0&price$Section<233&price$Section>=200,]
max(d_200$`Price ($)`,na.rm = T)
min(d_200$`Price ($)`,na.rm = T)
mean(d_200$`Price ($)`,na.rm = T)

d_28 <- price[price$Distance==0&price$Section<30,]
max(d_28$`Price ($)`,na.rm = T)
min(d_28$`Price ($)`,na.rm = T)
mean(d_28$`Price ($)`,na.rm = T)
