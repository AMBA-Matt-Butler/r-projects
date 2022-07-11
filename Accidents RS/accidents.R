library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

casualties <- read_csv("dft-road-casualty-statistics-accident-provisional-mid-year-unvalidated-2021.csv")

cas <- casualties %>%
	select(location_easting_osgr,
				 location_northing_osgr, 
				 police_force, 
				 accident_severity, 
				 number_of_vehicles,
				 number_of_casualties,
				 date,
				 day_of_week,
				 time,
				 speed_limit,
				 weather_conditions,
				 road_surface_conditions)



cas$location_easting_osgr <- as.numeric(cas$location_easting_osgr)
cas$location_northing_osgr <- as.numeric(cas$location_northing_osgr)
cas$date <- as.Date(dmy(casualties$date))
cas <- na.omit(cas)


casualties[list,]
