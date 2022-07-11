setwd("C:/Users/MBUTLER1/OneDrive - Department for Transport/Stats & R/R Plots/Average Speed")
library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(tidyr)
library(forcats)

#Reading required data
speed <- read_csv("CGN0503.csv", skip=4, n_max= 12)
speed <- speed[,2:8]

#Setting Column names
col_names <- c("Average", "Urban_Average", "Rural_Average", "Weekday_Morning", "Weekday_Inter", "Weekday_Evening", "Weekday_off_peak")
colnames(speed) <- col_names

#Creating month variable
speed$month <- as.numeric(seq(1:12))

months <- speed$month

##Start Here
ggplot(data = speed)+
	geom_line(aes(x = month, y = Average))

#multiple lines
ggplot(data = speed)+
	geom_line(aes(x = month, y = Average))+
	geom_line(aes(x = month, y = Weekday_Morning))+
	geom_line(aes(x = month, y = Weekday_Inter))+
	geom_line(aes(x = month, y = Weekday_Evening))+
	geom_line(aes(x = month, y = Weekday_off_peak))

#format average
ggplot(data = speed)+
	geom_line(aes(x = month, y = Average), colour = "black", linetype = "dotted")+
	geom_line(aes(x = month, y = Weekday_Morning))+
	geom_line(aes(x = month, y = Weekday_Inter))+
	geom_line(aes(x = month, y = Weekday_Evening))+
	geom_line(aes(x = month, y = Weekday_off_peak))

#colour all
ggplot(data = speed)+
	geom_line(aes(x = month, y = Average), colour = "black", linetype = "dotted")+
	geom_line(aes(x = month, y = Weekday_Morning), colour = "#4ac16d")+
	geom_line(aes(x = month, y = Weekday_Inter), colour = "#277f8e")+
	geom_line(aes(x = month, y = Weekday_Evening), colour = "#440154")+
	geom_line(aes(x = month, y = Weekday_off_peak), colour = "#a0da39")



ggplot(data = speed)+
	geom_line(aes(x = month, y = Average), colour = "black", linetype = "dotted")+
	geom_line(aes(x = month, y = Weekday_Morning), colour = "#4ac16d")+
	geom_line(aes(x = month, y = Weekday_Inter), colour = "#277f8e")+
	geom_line(aes(x = month, y = Weekday_Evening), colour = "#440154")+
	geom_line(aes(x = month, y = Weekday_off_peak), colour = "#a0da39")+
	labs(title = "Average Speeds on A-Roads on the SRN in 2021",
			 y = "Average Speed (mph)",
			 x = "Month")
	
ggplot(data = speed)+
	geom_line(aes(x = month, y = Average), colour = "black", linetype = "dotted")+
	geom_line(aes(x = month, y = Weekday_Morning), colour = "#4ac16d")+
	geom_line(aes(x = month, y = Weekday_Inter), colour = "#277f8e")+
	geom_line(aes(x = month, y = Weekday_Evening), colour = "#440154")+
	geom_line(aes(x = month, y = Weekday_off_peak), colour = "#a0da39")+
	labs(title = "Average Speeds on A-Roads on the SRN in 2021",
			 y = "Average Speed (mph)",
			 x = "Month")+
	annotate("text", x = 12, y= 21, label="Weekday Evening", colour = "#440154" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 27.8, label="Weekday Off-Peak", colour = "#a0da39", size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 24.1, label="Weekday Morning", colour = "#4ac16d" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 22.55, label="Weekday Inter-Peak", colour = "#277f8e" , size = 3, hjust=-0.1)+
	coord_cartesian(clip = "off")+
	theme(plot.margin = unit(c(0.5,3,0,0), unit = "cm"))

ggplot(data = speed)+
	geom_line(aes(x = month, y = Average), colour = "black", linetype = "dotted")+
	geom_line(aes(x = month, y = Weekday_Morning), colour = "#4ac16d")+
	geom_line(aes(x = month, y = Weekday_Inter), colour = "#277f8e")+
	geom_line(aes(x = month, y = Weekday_Evening), colour = "#440154")+
	geom_line(aes(x = month, y = Weekday_off_peak), colour = "#a0da39")+
	labs(title = "Average Speeds on A-Roads on the SRN in 2021",
			 y = "Average Speed (mph)",
			 x = "Month")+
	annotate("text", x = 12, y= 21, label="Weekday Evening", colour = "#440154" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 27.8, label="Weekday Off-Peak", colour = "#a0da39", size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 24.1, label="Weekday Morning", colour = "#4ac16d" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 22.55, label="Weekday Inter-Peak", colour = "#277f8e" , size = 3, hjust=-0.1)+
	coord_cartesian(clip = "off")+
	scale_x_continuous(expand =c(0,0), breaks = 1:12)+
	theme(plot.margin = unit(c(0.5,3,0,0), unit = "cm"))

ggplot(data = speed)+
	geom_line(aes(x = month, y = Average), colour = "black", linetype = "dotted")+
	geom_line(aes(x = month, y = Weekday_Morning), colour = "#4ac16d")+
	geom_line(aes(x = month, y = Weekday_Inter), colour = "#277f8e")+
	geom_line(aes(x = month, y = Weekday_Evening), colour = "#440154")+
	geom_line(aes(x = month, y = Weekday_off_peak), colour = "#a0da39")+
	labs(title = "Average Speeds on A-Roads on the SRN in 2021",
			 y = "Average Speed (mph)",
			 x = "Month")+
	annotate("text", x = 12, y= 21, label="Weekday Evening", colour = "#440154" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 27.8, label="Weekday Off-Peak", colour = "#a0da39", size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 24.1, label="Weekday Morning", colour = "#4ac16d" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 22.55, label="Weekday Inter-Peak", colour = "#277f8e" , size = 3, hjust=-0.1)+
	coord_cartesian(clip = "off")+
	scale_y_continuous(expand = c(0,0), breaks = c(seq(from = 0, to = 35, by = 5)), limits = c(0, 35))+
	scale_x_continuous(expand =c(0,0), breaks = 1:12)+
	theme(plot.margin = unit(c(0.5,3,0,0), unit = "cm"))

ggplot(data = speed)+
	geom_line(aes(x = month, y = Average), colour = "black", linetype = "dotted")+
	geom_line(aes(x = month, y = Weekday_Morning), colour = "#4ac16d")+
	geom_line(aes(x = month, y = Weekday_Inter), colour = "#277f8e")+
	geom_line(aes(x = month, y = Weekday_Evening), colour = "#440154")+
	geom_line(aes(x = month, y = Weekday_off_peak), colour = "#a0da39")+
	labs(title = "Average Speeds on A-Roads on the SRN in 2021",
			 y = "Average Speed (mph)",
			 x = "Month")+
	annotate("text", x = 12, y= 21, label="Weekday Evening", colour = "#440154" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 27.8, label="Weekday Off-Peak", colour = "#a0da39", size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 24.1, label="Weekday Morning", colour = "#4ac16d" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 22.55, label="Weekday Inter-Peak", colour = "#277f8e" , size = 3, hjust=-0.1)+
	coord_cartesian(clip = "off")+
	scale_y_continuous(expand = c(0,0), breaks = c(seq(from = 0, to = 35, by = 5)), limits = c(0, 35))+
	scale_x_continuous(expand =c(0,0), breaks = 1:12, labels = months)+
	theme(plot.margin = unit(c(0.5,3,0,0), unit = "cm"))
	
ggplot(data = speed)+
	geom_line(aes(x = month, y = Average), colour = "black", linetype = "dotted")+
	geom_line(aes(x = month, y = Weekday_Morning), colour = "#4ac16d")+
	geom_line(aes(x = month, y = Weekday_Inter), colour = "#277f8e")+
	geom_line(aes(x = month, y = Weekday_Evening), colour = "#440154")+
	geom_line(aes(x = month, y = Weekday_off_peak), colour = "#a0da39")+
	annotate("text", x = 12, y= 21, label="Weekday Evening", colour = "#440154" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 27.8, label="Weekday Off-Peak", colour = "#a0da39", size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 24.1, label="Weekday Morning", colour = "#4ac16d" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 22.55, label="Weekday Inter-Peak", colour = "#277f8e" , size = 3, hjust=-0.1)+
	coord_cartesian(clip = "off")+
	scale_y_continuous(expand = c(0,0), breaks = c(seq(from = 0, to = 35, by = 5)), limits = c(0, 35))+
	scale_x_continuous(expand =c(0,0), breaks = 1:12, labels = months)+
	labs(title = "Average Speeds on A-Roads \non the SRN in 2021",
			 y = "Average Speed (mph)",
			 x = "Month")+
	theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.3),
				axis.title.y =  element_text(margin = margin(r = 0.15, unit = "cm")),
				plot.title = element_text(hjust = 0.5),
				plot.margin = unit(c(0.5,3,0,0), unit = "cm"))

ggplot(data = speed)+
	geom_line(aes(x = month, y = Average), colour = "black", linetype = "dotted")+
	geom_line(aes(x = month, y = Weekday_Morning), colour = "#4ac16d")+
	geom_line(aes(x = month, y = Weekday_Inter), colour = "#277f8e")+
	geom_line(aes(x = month, y = Weekday_Evening), colour = "#440154")+
	geom_line(aes(x = month, y = Weekday_off_peak), colour = "#a0da39")+
	annotate("text", x = 12, y= 21, label="Weekday Evening", colour = "#440154" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 27.8, label="Weekday Off-Peak", colour = "#a0da39", size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 24.1, label="Weekday Morning", colour = "#4ac16d" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 22.55, label="Weekday Inter-Peak", colour = "#277f8e" , size = 3, hjust=-0.1)+
	coord_cartesian(clip = "off")+
	scale_y_continuous(expand = c(0,0), breaks = c(seq(from = 0, to = 35, by = 5)), limits = c(0, 35))+
	scale_x_continuous(expand =c(0,0), breaks = 1:12, labels = months)+
	labs(title = "Average Speeds on A-Roads \non the SRN in 2021",
			 y = "Average Speed (mph)",
			 x = "Month")+
	theme_economist()+
	theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.3),
				axis.title.y =  element_text(margin = margin(r = 0.15, unit = "cm")),
				plot.title = element_text(hjust = 0.5),
				plot.margin = unit(c(0.5,3,0,0), unit = "cm"))

ggplot(data = speed)+
	geom_line(aes(x = month, y = Average), colour = "black", linetype = "dotted")+
	geom_line(aes(x = month, y = Weekday_Morning), colour = "#4ac16d")+
	geom_line(aes(x = month, y = Weekday_Inter), colour = "#277f8e")+
	geom_line(aes(x = month, y = Weekday_Evening), colour = "#440154")+
	geom_line(aes(x = month, y = Weekday_off_peak), colour = "#a0da39")+
	annotate("text", x = 12, y= 21, label="Weekday Evening", colour = "#440154" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 27.8, label="Weekday Off-Peak", colour = "#a0da39", size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 24.1, label="Weekday Morning", colour = "#4ac16d" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 22.55, label="Weekday Inter-Peak", colour = "#277f8e" , size = 3, hjust=-0.1)+
	coord_cartesian(clip = "off")+
	scale_y_continuous(expand = c(0,0), breaks = c(seq(from = 0, to = 35, by = 5)), limits = c(0, 35))+
	scale_x_continuous(expand =c(0,0), breaks = 1:12, labels = months)+
	labs(title = "Average Speeds on A-Roads \non the SRN in 2021",
			 y = "Average Speed (mph)",
			 x = "Month")+
	theme_stata()+
	theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.3),
				axis.title.y =  element_text(margin = margin(r = 0.15, unit = "cm")),
				plot.title = element_text(hjust = 0.5),
				plot.margin = unit(c(0.5,3,0,0), unit = "cm"))

ggplot(data = speed)+
	geom_line(aes(x = month, y = Average), colour = "black", linetype = "dotted")+
	geom_line(aes(x = month, y = Weekday_Morning), colour = "#4ac16d")+
	geom_line(aes(x = month, y = Weekday_Inter), colour = "#277f8e")+
	geom_line(aes(x = month, y = Weekday_Evening), colour = "#440154")+
	geom_line(aes(x = month, y = Weekday_off_peak), colour = "#a0da39")+
	annotate("text", x = 12, y= 21, label="Weekday Evening", colour = "#440154" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 27.8, label="Weekday Off-Peak", colour = "#a0da39", size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 24.1, label="Weekday Morning", colour = "#4ac16d" , size = 3, hjust=-0.1)+
	annotate("text", x = 12, y= 22.55, label="Weekday Inter-Peak", colour = "#277f8e" , size = 3, hjust=-0.1)+
	coord_cartesian(clip = "off")+
	scale_y_continuous(expand = c(0,0), breaks = c(seq(from = 0, to = 35, by = 5)), limits = c(0, 35))+
	scale_x_continuous(expand =c(0,0), breaks = 1:12, labels = months)+
	labs(title = "Average Speeds on A-Roads \non the SRN in 2021",
			 y = "Average Speed (mph)",
			 x = "Month")+
	theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.3),
				axis.title.y =  element_text(margin = margin(r = 0.15, unit = "cm")),
				plot.title = element_text(hjust = 0.5),
				plot.margin = unit(c(0.5,3,0,0), unit = "cm"), 
				axis.line = element_line(colour = "black"),
				panel.background = element_blank())

# Plotly
speed_t <- speed %>%
	pivot_longer(!month, names_to = "category", values_to = "Speed")


#Factor levels for legend
speed_t[c(seq(2, 79, by= 7)), 2] = "Urban Average"
speed_t[c(seq(3, 80, by= 7)), 2] = "Rural Average"
speed_t[c(seq(4, 81, by= 7)), 2] = "Weekday Morning"
speed_t[c(seq(5, 82, by= 7)), 2] = "Weekday Inter-Peak"
speed_t[c(seq(6, 83, by= 7)), 2] = "Weekday Evening"
speed_t[c(seq(7, 84, by= 7)), 2] = "Weekday Off-Peak"


speed_t$category <- as.factor(speed_t$category)
speed_t$category <- factor(speed_t$category, levels = c("Weekday Off-Peak", "Average", "Weekday Morning", "Weekday Inter-Peak", "Weekday Evening", "Urban Average", "Rural Average"))

ggplot()+
	geom_line(data = speed_t %>% filter(category != "Average" & category != "Urban Average" & category != "Rural Average"), aes(x = month, y = Speed, group = category, colour = category))+
	geom_line(data = speed_t %>% filter(category == "Average"), aes(x = month, y = Speed, group = category, colour = category), linetype = "dotted")+
	scale_color_manual(values = c("#CECECE", "#440154", "#277f8e", "#4ac16d", "#a0da39"))+
	scale_y_continuous(expand = c(0,0), breaks = c(seq(from = 0, to = 35, by = 5)), limits = c(0, 35))+
	scale_x_continuous(expand =c(0,0), breaks = 1:12, labels = months)+
	labs(title = "Average Speeds on A-Roads \non the SRN in 2021",
			 y = "Average Speed (mph)",
			 x = "Month")+
	theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.3),
				axis.title.y =  element_text(margin = margin(r = 0.15, unit = "cm")),
				plot.title = element_text(hjust = 0.5),
				axis.line = element_line(colour = "black"),
				panel.background = element_blank(),
				legend.title = element_blank(),
				legend.text = element_text(size=7))

ggplotly(tooltip = c("text", "Speed")) %>% 
	layout(margin = list(l = 50))
	
				 