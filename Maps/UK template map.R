library(ggplot2)
library(dplyr)

mapdata <- map_data(map = "world", region = "UK")

ggplot(mapdata, aes( x = long, y = lat, group=group)) +
	geom_polygon(aes(fill = subregion), color = "black")+
	theme(axis.text.x = element_blank(),
				axis.text.y = element_blank(),
				axis.ticks = element_blank(),
				axis.title.y=element_blank(),
				axis.title.x=element_blank(),
				rect = element_blank())

