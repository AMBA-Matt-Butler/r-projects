install.packages("readODS")
library(readODS)
library(ggplot2)
library(janitor)

data <-read_ods("roro0101.ods", skip = 8)
tail(data, n = 15)
data <- data[1:157,]
data[,3] <- as.numeric(data[,3])
data[,5] <- as.numeric(data[,5])
data[,1] <- as.numeric(seq(from = 1983.25, to= 2022.25, by = 0.25))
data <- clean_names(data)

ggplot(data = data)+
	geom_area(aes(x = year, y = total4))+
	geom_line(aes(x = year, y = total4), color = "red")+
	geom_area(aes(x = year, y = foreign), fill = "dark grey")+
	geom_area(aes(x = year, y = uk), fill = "light grey")+
	labs(y = "Trips (thousands)",
			 x = "Year",
			 title = "Road Goods Vehicles Travelling to Europe")+
	scale_x_continuous(expand = c(0,0), limits = c(2010, 2023))+
	scale_y_continuous(expand = c(0,0), limits = c(0, 1000))
