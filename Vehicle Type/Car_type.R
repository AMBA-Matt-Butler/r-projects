search()

install.packages("readODS")
install.packages("tidyverse")
install.packages("janitor")
install.packages("zoo")
install.packages("lubridate")

library(readODS)
library(tidyverse)
library(janitor)
library(dplyr)
library(zoo)
library(lubridate)
library(scales)
library(plyr)



df <- read_ods("veh0161.ods", range="A7:DI2685")
df <- df[,-c(2:30)]
df <- clean_names(df)

df <- df %>%
  arrange(desc(x2021_q3_gb))
names(df)[1] <- "model"

#replacing NA with 0
df1<-df
df1[is.na(df1)] <- 0

# make total of all cars (not just top 50)
df1 <- df1 %>%
  adorn_totals("row")




#only including top 50 last Quarter
df1_f <- df1 %>%
  filter(x2021_q3_gb>=2543)


#By type
df1_type <- df1 %>%
  filter(x2021_q3_gb>=2543)

df1_type <- df1_type[-c(52),]

df1_type$type <- "1"
df1_type <- df1_type %>% 
  relocate(type, .after=model)
df1_type$type[df1_type$model=="TESLA MODEL 3"]<-"2"
df1_type$type[df1_type$model=="KIA NIRO"]<-"2"
df1_type$type[df1_type$model=="VOLKSWAGEN ID3"]<-"2"
df1_type$type <- as.factor(df1_type$type)

ddply(df1_type, .(type), colwise(count))

qnames <- names(df1_type)

df1_type %>% 
  group_by(type) %>%
  summarise(colwise(qnames))

args(summarise_all)
df1_type[52,] <- df1_type %>% 
  filter(type=="1") %>%
  
  sum(df1_type[52,])

df1_type <- df1_type[-c(52),]

#convert original to long form
dflong <- df1_f %>%
  pivot_longer(!model, names_to= "Quarter")



#create date
qs<- seq(ymd('2001-01-01'),ymd('2021-07-01'), by = '1 quarter')
qsr<-rev(qs)
date<- rep(qsr, times=52)
dflong$date <- as.Date(date)


#set type?
dflong$type <- "1"
dflong$type[dflong$model=="TESLA MODEL 3"]<-"2"
dflong$type[dflong$model=="KIA NIRO"]<-"2"
dflong$type[dflong$model=="VOLKSWAGEN ID3"]<-"2"
dflong$type[dflong$model=="Total"]<-"3"
dflong$type <- as.factor(dflong$type)

dflong$total<-"1"
dflong$total[dflong$model=="Total"]<-"0"
dflong$total <- as.numeric(dflong$total)


#smoooth data to yearly avg, also merge into two types

#plots
dflong <- dflong %>%
  arrange(type)

ggplot(dflong %>% filter(type !="3"))+
  geom_area(aes(x=date, y=value, group=model, fill=type))+
  labs(title= "Cars Sold since 2000 (Quarterly)",
       x= "Date",
       y= "Cars Sold")+
  scale_y_continuous(label=comma)+
  theme(legend.position="None",
        strip.text.x = element_blank())


#facet wrap
ggplot(dflong %>% filter(type =="1"))+
  geom_line(aes(x=date, y=value, group=model, colour="#CECECE"))+
  geom_line(data = dflong %>% filter(type =="2"), aes(x=date, y=value, group=model, colour="black"))+
  geom_line(data = dflong %>% filter(type =="3"), aes(x=date, y=value, group=model), colour="black")+
  scale_colour_manual(values=c("#CECECE", "#0A460F", "#FFFFFF"))+
  facet_wrap(~total, nrow=3, scales="free")+
  labs(title= "Cars Sold since 2000 (Quarterly)",
       x= "Date",
       y= "Cars Sold",
       subtitle="Electric Vehicles (Green) sales rise as total volume falls.",
       caption="EV models: Tesla Model 3, Kia Niro, Volkswagen ID3\n Data from DfT: veh0161")+
  scale_y_continuous(label=comma)+
  theme(legend.position="None",
        strip.text.x = element_blank())
ggsave("EV models.jpg", width = 6, height = 6, dpi =600)
args(ggsave)

#issue, axis doesn't go to 0, could repeat all as index or as an area chart with percentages


#all cars [plot]
ggplot(dflong %>% filter(type =="3"))+
  geom_line(aes(x=date, y=value, group=model, colour="#CECECE"))+
  #geom_line(data = dflong %>% filter(type =="2"), aes(x=date, y=value, group=model, colour="black"))+
  theme(legend.position="None")
  #scale_colour_manual(values=c("#CECECE", "#0A460F"))

#sort out colours
#second axis showing total cars





