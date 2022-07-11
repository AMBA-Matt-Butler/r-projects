library(ggplot2)

energy <- read_ods("Total_Energy_ODS.ods", sheet = 3)

#dropping extra
energy <- energy[-c(1:3, 6,7),]
energy <- energy[ ,-c(3:11)]
row.names(energy) <- NULL
energy[2,1] <- "Year"
energy[2,2] <- "Month"
energy[2,6] <- "Natural_Gas"
energy[2,7] <- "Bio_and_waste"
energy[2,9] <- "WSH"
energy[2,10] <- "NetImports"
energy <- energy[-c(1),]

#making top row the column names
names(energy) <- as.matrix(energy[1, ])
energy <- energy[-1, ]
energy <- energy[-c(322:323), ]

energy[] <- lapply(energy, function(x) type.convert(as.character(x)))

#Date
energy <- cbind(energy, seq(as.Date('1995-01-01'),as.Date('2021-09-01'),by = "1 month"))
names(energy)[11] <- "Date"
energy<- energy[,c(11,1:10)]
energy<-energy[ ,-c(2,3)]

energy1998 <- energy[-c(1:38),]
row.names(energy1998) <- NULL


energy1998$WSH<-as.numeric(energy1998$WSH)
energy1998$NetImports<-as.numeric(energy1998$NetImports)
#plots
ggplot(energy1998, aes(colour=Date))+
  geom_line(aes(x=Date, y=Coal))+
  geom_line(aes(x=Date, y=Petroleum))+
  geom_line(aes(x=Date, y=Natural_Gas))+
  geom_line(aes(x=Date, y=Bio_and_waste))+
  geom_line(aes(x=Date, y=Nuclear))+
  geom_line(aes(x=Date, y=WSH))+
  geom_line(aes(x=Date, y=NetImports))+
  scale_fill_viridis(discrete = TRUE)+
  scale_colour_identity()+
  theme_bw()

ggplot(energy1998)+
  geom_line(aes(x=Date, y=Coal, colour = "dark grey"))+
  geom_line(aes(x=Date, y=Petroleum, colour="purple"))+
  geom_line(aes(x=Date, y=Natural_Gas, colour="blue"))+
  geom_line(aes(x=Date, y=Bio_and_waste, colour="brown"))+
  geom_line(aes(x=Date, y=Nuclear, colour="dark green"))+
  geom_line(aes(x=Date, y=WSH, colour="orange"))+
  geom_line(aes(x=Date, y=NetImports))+
  scale_fill_viridis(discrete = TRUE)+
  scale_colour_identity()+
  theme_bw()
  



