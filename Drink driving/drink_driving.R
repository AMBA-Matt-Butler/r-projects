library(readr)
library(janitor)
library(ggplot2)
library(dplyr)

#Importing data
df <- read_csv("ras51001.csv", skip = 3)

acc <- df[,1:7] %>% clean_names()
names(acc)[1] <- "year"
names(acc)[7] <- "total_accidents"

cas <- df[,9:17] %>% clean_names()
names(cas)[1] <- "year"
names(cas)[9] <- "total_accidents"
names(cas)[2] <- "killed"
names(cas)[5] <- "serious_u"
names(cas)[7] <- "slight_u"
names(cas)[6] <- "serious_ad"
names(cas)[8] <- "slight_ad"
#Creating FWI
cas <- cas %>%
  mutate(fwi = killed + serious_u/10 + slight_u/100)

#Removing notes
acc[acc == "[z]"] <- NA
cas[cas == "[z]" |cas == "[x]" ] <- NA
acc[4] <- as.numeric(gsub(",","",acc[4]))
acc[6] <- as.numeric(gsub(",","",acc[6]))
#Complete data
acc_f <- acc%>%
  filter(year >=2005)
acc <- as.numeric(acc)

#Accident base index plot
ggplot(acc, aes(x=year))+
  labs(title = "Change in Drink Drive Accidents since 1979",
       y = "Index (Base Year of 1979)") +
  geom_line(aes(y = total_accidents/19470))+
  geom_line(aes(y = fatal_accidents/1380))

#since 2005
ggplot(acc_f, aes(x=year))+
  labs(title = "Change in Drink Drive Accidents since 2005",
       y = "Index (Base Year of 2005)") +
  geom_line(aes(y = total_accidents/10080))+
  geom_line(aes(y = fatal_accidents/470))
label = c("")
#Casualty FWI base plot
ggplot(cas, aes(x=year))+
    labs(title = "Change in Drink Drive Casualties since 1979",
         subtitle = "Adjusted data NOT used (only available from 2005)",
         caption = "Black = FWI, Blue = Slight, Green = Serious, Red = Fatal",
       y = "Index (Base Year of 1979)") +
  geom_line(aes(y = fwi/2685))+
  geom_line(aes(y = killed/1640), color = "red")+
  geom_line(aes(y = serious_u/8300), color = "green")+
  geom_line(aes(y = slight_u/21490), color = "blue")

ggsave("drink_drive.png", path = "C:/Users/MBUTLER1/OneDrive - Department for Transport/Stats & R/R Plots/Drink driving", width = 6, height = 6, dpi=700)


