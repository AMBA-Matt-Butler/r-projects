setwd("C:/Users/MBUTLER1/OneDrive - Department for Transport/Stats & R/R Plots/Road Length")
library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(tidyr)
library(forcats)
library(janitor)
data <- read_csv("rdl0103.csv", skip =7)%>%clean_names()
data <- data[c(7:78), 1:14]
data$all_major_roads <- as.numeric(data$all_major_roads)

ggplot(data)+
	geom_area(aes(x=year, y=all_major_roads), fill = "#CECECE")+
	geom_area(aes(x=year, y=trunk_motorways))
