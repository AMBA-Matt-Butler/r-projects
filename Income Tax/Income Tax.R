library(dplyr)
library(ggplot2)
library(scales)
library(readxl)
library(plyr)

setwd("C:/Users/MBUTLER1/OneDrive - Department for Transport/Stats & R/R Plots/Income Tax")


#Tax Rates in England and Wales

#Select Range, Above 162,00 is top 1% according to IFS
annual_salary_low <- as.numeric(0)

#wage growth: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/bulletins/averageweeklyearningsingreatbritain/january2022
wage_growth_since_dec2015 <- (582-487)/487
wage_growth_dec2018 <- (582-527)/527
wage_growth_since_march2019 <- (582-533)/533
income_95_perc <- 124367.81

cpi_since_march2020 <- (112.1-108)/108

#Scale Up by inflation Index of Private Housing Rental Prices indices England -> March 2018: ONS 106.1, Aug 2021 111.3
rent_inflation_since_march2018 <- (111.3-106.1)/106.1

annual_salary_high <- as.numeric(income_95_perc)
#Keep at 100 for rent
annual_salary_increment <- as.numeric(100)
#

name <- as.factor(c("PA", "BR", "HR", "AR"))
rate <- as.numeric(c(0, 0.2, 0.4, 0.45))
income <- as.numeric(c(seq(annual_salary_low, annual_salary_high, by = annual_salary_increment)))
income_tax <- data.frame(income = income) %>%
  mutate(tax_band = as.factor(case_when(income < 12570 ~ "PA",
                                    income >= 12570 & income < 50270 ~ "BR",
                                    income >= 50271 & income <= 150000 ~ "HR",
                                    income > 150000  ~ "AR")))%>%
  mutate(marginal_tax_rate = as.numeric(case_when(income < 12570 ~ 0,
                                     income >= 12570 & income < 50270 ~ 0.2,
                                     income >= 50271 & income <= 150000 ~ 0.4,
                                     income > 150000  ~ 0.45)))
#create three new columns, actual tax paid, and then average, AND disposable
income_tax <- income_tax %>%
  mutate(raw_tax = case_when(income < 12570 ~ income * 0,
                             income >= 12570 & income < 50270 ~ 0 + (income-12570)*0.2 ,
                             income >= 50271 & income <= 150000 ~ 0 + (income-12570)*0.2 + (income-50270)*0.2,
                             income > 150000  ~ 0 + (income-12570)*0.2 + (income-50270)*0.2+ (income-150000)*0.05))%>%
  mutate(average_tax = raw_tax / income)

#National Insurance Class 1 - Employees earning money
ni_thresholds_c1 <- as.numeric(c(184*52, 50270))

ni_tax <- data.frame(income = income) %>%
  mutate(nic_band = as.factor(case_when(income < 9568 ~ "None",
                                       income >= 9568 & income < 50270 ~ "ST",
                                       income >= 50270  ~ "UEL")))%>%
  mutate(rate_nic = as.numeric(case_when(income < 9568 ~ 0,
                                        income >= 9568 & income < 50270 ~ 0.12,
                                        income >= 50270  ~ 0.02)))%>%
  mutate(raw_nic = as.numeric(case_when(income < 9568 ~ 0,
                                        income >= 9568 & income < 50270 ~ (income-9568)*0.12,
                                        income >= 50270  ~ (income-9568)*0.12-(income-50270)*0.12  + (income-50270)*0.02))) %>%
  mutate(average_nic = raw_nic/income)

#Combine for reductions table
reductions <- cbind(income_tax, ni_tax[,2:5])

#Interpolation

#Prep for area decile
#Make deciles, then scale up from ONS https://www.ons.gov.uk/peoplepopulationandcommunity/personalandhouseholdfinances/incomeandwealth/adhocs/11663incomebygrossincomedecilegroupukfinancialyearending2019
#add bills and then COUNCIL TAX

reductions <- reductions %>%
  mutate(decile = case_when(income < 12687.22 ~ "D1", 
  													 income >= 12687.22 & income < 18666.03 ~ "D2", 
  													 income >= 18666.03 & income < 24545.48 ~ "D3", 
  													 income >= 24545.48 & income < 31180.08 ~ "D4", 
  													 income >= 31180.08 & income < 38813.99 ~ "D5", 
  													 income >= 38813.99 & income < 47716.90 ~ "D6", 
  													 income >= 47716.90 & income < 58538.94 ~ "D7", 
  													 income >= 58538.94 & income < 73554.10 ~ "D8", 
  													 income >= 73554.10 & income < 103436.54 ~ "D9", 
  													 income >= 103436.54  ~ "D10"))

red1d <- reductions %>%
  filter(decile == "D1")%>%
  mutate(percentile = 10*(income - 0)/12687.22)%>%
  mutate(raw_rent = 0+2326.5*(percentile - 0)/10)%>%
	mutate(raw_fnd = 0+2043*(percentile - 0)/10)%>%
	mutate(raw_gne = 0+3152*(percentile - 0)/10)

red2d <- reductions %>%
	filter(decile == "D2")%>%
	mutate(percentile = 10+10*(income - 12687.22)/(18666.03-12687.22))%>%
	mutate(raw_rent = 2326.5+927.3*(percentile - 10)/10)%>%
	mutate(raw_fnd = 2043+381*(percentile - 10)/10)%>%
	mutate(raw_gne = 3152+475*(percentile - 10)/10)

red3d <- reductions %>%
	filter(decile == "D3")%>%
	mutate(percentile = 20+10*(income - 18666.03)/(24545.48-18666.03))%>%
	mutate(raw_rent = 3253.8+1309.2*(percentile - 20)/10)%>%
	mutate(raw_fnd = 2424+351*(percentile - 20)/10)%>%
	mutate(raw_gne = 3627+470*(percentile - 20)/10)

red4d <- reductions %>%
	filter(decile == "D4")%>%
	mutate(percentile = 30+10*(income - 24545.48)/(31180.08-24545.48))%>%
	mutate(raw_rent = 4562.9+1279.2*(percentile - 30)/10)%>%
	mutate(raw_fnd = 2774+362*(percentile - 30)/10)%>%
	mutate(raw_gne = 4097+119*(percentile - 30)/10)

red5d <- reductions %>%
	filter(decile == "D5")%>%
	mutate(percentile = 40+10*(income - 31180.08)/(38813.99-31180.08))%>%
	mutate(raw_rent = 5842.1+1301*(percentile - 40)/10)%>%
	mutate(raw_fnd = 3136+273*(percentile - 40)/10)%>%
	mutate(raw_gne = 4216+399*(percentile - 40)/10)

red6d <- reductions %>%
	filter(decile == "D6")%>%
	mutate(percentile = 50+10*(income - 38813.99)/(47716.90-38813.99))%>%
	mutate(raw_rent = 7143.1+1336.4*(percentile - 50)/10)%>%
	mutate(raw_fnd = 3409+327*(percentile - 50)/10)%>%
	mutate(raw_gne = 4615+281*(percentile - 50)/10)

red7d <- reductions %>%
	filter(decile == "D7")%>%
	mutate(percentile = 60+10*(income - 47716.90)/(58538.94-47716.90))%>%
	mutate(raw_rent = 8479.5+927.3*(percentile - 60)/10)%>%
	mutate(raw_fnd = 3735+337*(percentile - 60)/10)%>%
	mutate(raw_gne = 4896-5*(percentile - 60)/10)

red8d <- reductions %>%
	filter(decile == "D8")%>%
	mutate(percentile = 70+10*(income - 58538.94)/(73554.10-58538.94))%>%
	mutate(raw_rent = 9406.8+990*(percentile - 70)/10)%>%
	mutate(raw_fnd = 4072+232*(percentile - 70)/10)%>%
	mutate(raw_gne = 4890+11*(percentile - 70)/10)

red9d <- reductions %>%
	filter(decile == "D9")%>%
	mutate(percentile = 80+10*(income - 73554.10)/(103436.54-73554.10))%>%
	mutate(raw_rent = 10396.8+3507.4*(percentile - 80)/10)%>%
	mutate(raw_fnd = 4305+588*(percentile - 80)/10)%>%
	mutate(raw_gne = 4901+739*(percentile - 80)/10)

#Going up to 95%, for 95% using ONS data (redo with ONS 95% number)
red10d <- reductions %>%
	filter(decile == "D10")%>%
	mutate(percentile = 90+5*(income - 103436.54)/(124367.81-103436.54))%>%
	mutate(raw_rent = 13904+2727.4*(percentile - 90)/5)%>%
	mutate(raw_fnd = 4893+435*(percentile - 90)/5)%>%
	mutate(raw_gne = 5640+745*(percentile - 90)/5)

#Max level inferred from this: https://www.earnest.com/blog/rent-and-the-30-percent-rule/
reductions <- rbind(red1d, red2d, red3d, red4d, red5d, red6d, red7d, red8d, red9d, red10d)

#Making percentages

reductions <- reductions %>%
  mutate(raw_red = raw_tax + raw_nic + raw_rent) %>%
  mutate(average_red = raw_red / income) 

#Making percentages
reductions <- reductions %>%
  mutate(income_minus_rent = income - raw_rent)%>%
	mutate(income_minus_rent_gne = income_minus_rent-raw_gne)%>%
	mutate(income_minus_rent_gne_fnd = income_minus_rent_gne - raw_fnd) %>%
  mutate(income_minus_rent_gne_fnd_NIC = income_minus_rent_gne_fnd - raw_nic)%>%
  mutate(income_minus_rent_gne_fnd_NIC_tax = income_minus_rent_gne_fnd_NIC - raw_tax)

reductions <- reductions %>%
  mutate(per_income_minus_rent = income_minus_rent/income)%>%
	mutate(per_income_minus_rent_gne = income_minus_rent_gne/income)%>%
	mutate(per_income_minus_rent_gne_fnd = income_minus_rent_gne_fnd/income)%>%
  mutate(per_income_minus_rent_gne_fnd_NIC = income_minus_rent_gne_fnd_NIC/income)%>%
  mutate(per_income_minus_rent_gne_fnd_NIC_tax = income_minus_rent_gne_fnd_NIC_tax/income)


#Idea: after rental, add distribution of income as a marginal histogram and income deciles

#Tidying reductions
reductions$decile = as.factor(reductions$decile)
levels(reductions$decile) <- c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10")
#reductions <- reductions[,c(1, 10:23, 2:9)]

#reductions[1, c(7, 12:15, 19, 23)] = 0


#Make % plot
yalabs = c("0%", "25%", "50%", "75%", "100%")


ggplot(reductions)+
  geom_area(aes(x=income, y=income/income), fill = "#CECECE")+
  geom_area(aes(x=income, y=(1-per_income_minus_rent_gne_fnd_NIC_tax)), fill = "#7ad151")+
  geom_area(aes(x=income, y=(1-per_income_minus_rent_gne_fnd_NIC)), fill = "#22a884")+
  geom_area(aes(x=income, y=(1-per_income_minus_rent_gne_fnd)), fill = "#2a788e")+
	geom_area(aes(x=income, y=(1-per_income_minus_rent_gne)), fill = "#414487")+
	geom_area(aes(x=income, y=(1-per_income_minus_rent)), fill = "#440154")+
	geom_rect(xmin = 0, xmax = 12687.22, ymin = 0, ymax = 1, alpha=0.00, color="white", linetype ="dashed", size=0.1)+
	#geom_rect(xmin = 12687.22, xmax = 18666.03, ymin = 0, ymax = 1, alpha=0.0005, color="white", linetype ="dashed", size=0.1)+
	geom_rect(xmin = 18666.03, xmax = 24545.48, ymin = 0, ymax = 1, alpha=0.001, color="white", linetype ="dashed", size=0.1)+
	#geom_rect(xmin = 24545.48, xmax = 31180.08, ymin = 0, ymax = 1, alpha=0.0015, color="white", linetype ="dashed", size=0.1)+
	geom_rect(xmin = 31180.08, xmax = 38813.99, ymin = 0, ymax = 1, alpha=0.002, color="white", linetype ="dashed", size=0.1)+
	#geom_rect(xmin = 38813.99, xmax = 47716.90, ymin = 0, ymax = 1, alpha=0.0025, color="white", linetype ="dashed", size=0.1)+
	geom_rect(xmin = 47716.90, xmax = 58538.94, ymin = 0, ymax = 1, alpha=0.0030, color="white", linetype ="dashed", size=0.1)+
	#geom_rect(xmin = 58538.94, xmax = 73554.10, ymin = 0, ymax = 1, alpha=0.0035, color="white", linetype ="dashed", size=0.1)+
	geom_rect(xmin = 73554.10, xmax = 103436.54, ymin = 0, ymax = 1, alpha=0.004, color="white", linetype ="dashed", size=0.1)+
	#geom_rect(xmin = 103436.54, xmax = 124367.81, ymin = 0, ymax = 1, alpha=0.0045, color="white", linetype ="dashed", size=0.1)+
  scale_x_continuous(expand =  c(0,0), labels = label_number(scale = 1e-3))+
  scale_y_continuous(expand =  c(0,0), labels = yalabs)+
	annotate("text", x = annual_salary_high/2, y = 0.75, label = "Disposable Income", size = 4)+
	annotate("text", x= 1.3*annual_salary_high/2, y = 0.45, label = "Income Tax", size = 4, colour = "black")+
  annotate("text", x = 41000, y = 0.43, label = "NIC", size = 4, colour = "white")+
	annotate("text", x = 7500, y = 0.49, label = "Food &\nDrink", size = 3, colour = "white")+
	annotate("text", x = 17500, y = 0.27, label = "Bills", size = 4, colour = "white")+
  annotate("text", x = annual_salary_high/2, y = 0.075, label = "Rent", colour = "white", size = 4)+
  labs(title = "Where does your money go?",
       subtitle = "Note: For bottom 10%, spending is assumed to be flat due to lack of data.",
       y = "Percentage",
       x = "Income (£000s)")+
  theme(plot.subtitle=element_text(size=7, hjust=0, face="italic", color="#333333"),
        plot.margin = margin(5,20,0,0))

#To improve find a better estimate of high income rent
ggsave("AreaPlot.jpeg", path = "C:/Users/MBUTLER1/OneDrive - Department for Transport/Stats & R/R Plots/Income Tax")
ggsave("AreaPlot.png", path = "C:/Users/MBUTLER1/OneDrive - Department for Transport/Stats & R/R Plots/Income Tax", height = 6, width = 6,  dpi = 700)


#Base Tax Plot
ggplot(income_tax, aes(x=income))+
  geom_line(aes(y = average_tax))+
  geom_line(aes(y = marginal_rate))+
  scale_y_continuous(expand = c(0,0), labels = ylabel, limits = c(0, 0.5))+
  scale_x_continuous(expand = c(0,0))+
  labs(title = "Income Tax",
       subtitle= "As Marginal Tax (black) is below Average Tax (colour) the tax is progressive.\nFor every extra £ earned, the average tax increases.",
       y = "Tax Rate",
       x = "Income (£s)")+
  theme(plot.subtitle=element_text(size=8, hjust=0, face="italic", color="#333333"),
        plot.margin = margin(0,20,0,0))



#Base NIC plot
ggplot(ni_tax, aes(x=income))+
  geom_line(aes(y = average_nic))+
  geom_line(aes(y = ni_rate))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  labs(title = "National Insurance Contributions",
       subtitle= "Focus only on employee contributions (21/22 Values)",
       y = "Tax Rate",
       x = "Income (£s)")+
  theme(plot.subtitle=element_text(size=8, hjust=0, face="italic", color="#333333"),
        plot.margin = margin(0,20,0,0))

#Base Combined Plot
ggplot()+
  geom_line(data = reductions, aes(x=income, y = average_red), color= "black")+
  geom_line(data = reductions, aes(x=income, y = marginal_red), color= "black")+
  geom_line(data = ni_tax, aes(x=income, y = average_nic), color= "blue", alpha =0.3)+
  geom_line(data = ni_tax, aes(x=income, y = ni_rate), color= "blue", alpha =0.3)+
  geom_line(data = income_tax, aes(x=income, y = average_tax), color= "green", alpha =0.3)+
  geom_line(data = income_tax, aes(x=income, y = marginal_rate), color= "green", alpha =0.4)+
  scale_y_continuous(expand = c(0,0), labels = ylabel, limits = c(0, 0.5))+
  scale_x_continuous(expand = c(0,0))+
  labs(title = "Income Tax + NI Contributions",
       y = "Rate",
       x = "Income (£s)")+
  theme(plot.subtitle=element_text(size=8, hjust=0, face="italic", color="#333333"),
        plot.margin = margin(0,20,0,0))

# ggsave("IncomeTax&NICGraph.png", path = "C:/Users/MBUTLER1/OneDrive - Department for Transport/Stats & R/R Plots/Income Tax", height = 6, width = 12,  dpi = 700)



#Raw Values
ggplot(data = reductions)+
  geom_line(aes(x=income, y = raw_red), color= "black")+
  geom_line(aes(x=income, y = raw_nic), color= "blue", alpha =0.3)+
  geom_line(aes(x=income, y = raw_tax), color= "green", alpha =0.3)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  labs(title = "Income Tax + NI Contributions",
       y = "Total Contributions",
       x = "Income (£s)")+
  theme(plot.subtitle=element_text(size=8, hjust=0, face="italic", color="#333333"),
        plot.margin = margin(0,20,0,0))

#Average values
ggplot(data = reductions)+
  geom_line(aes(x=income, y = raw_red/income), color= "black")+
  geom_line(aes(x=income, y = raw_nic/income), color= "blue", alpha =0.3)+
  geom_line(aes(x=income, y = raw_tax/income), color= "green", alpha =0.3)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  labs(title = "Income Tax + NI Contributions",
       y = "Average",
       x = "Income (£s)")+
  theme(plot.subtitle=element_text(size=8, hjust=0, face="italic", color="#333333"),
        plot.margin = margin(0,20,0,0))

#Make area plot
ggplot(reductions)+
  geom_area(aes(x=income, y=income), fill = "#CECECE")+
  geom_area(aes(x=income, y=raw_red), fill = "#DDDDDD")+
  geom_area(aes(x=income, y=raw_nic), fill = "red")+
  scale_x_continuous(expand =  c(0,0))+
  scale_y_continuous(expand =  c(0,0))+
  labs(title = "Tax + NI Contributions",
       subtitle = "Where does my money go?",
       y = "Income (£s)",
       x = "Income (£s)")+
  theme(plot.subtitle=element_text(size=8, hjust=0, face="italic", color="#333333"),
        plot.margin = margin(0,20,0,0))
  
#ggsave("AreaPlot.png", path = "C:/Users/MBUTLER1/OneDrive - Department for Transport/Stats & R/R Plots/Income Tax", height = 6, width = 6,  dpi = 700)




#Make % plot
ggplot(reductions)+
  geom_area(aes(x=income, y=income_per), fill = "#CECECE")+
  geom_area(aes(x=income, y=income_per_minus_red), fill = "#3b528b")+
  geom_area(aes(x=income, y=income_per_minus_nic), fill = "#440154")+
  geom_area(aes(x=income, y=income_per_minus_rent), fill = "red")+
  scale_x_continuous(expand =  c(0,0), labels = label_number(scale = 1e-3))+
  scale_y_continuous(expand =  c(0,0), labels = yalabs)+
  annotate("text", x= annual_salary_high/2, y = 0.2, label = "Income Tax", size = 4, colour = "white")+
  annotate("text", x = 45000, y = 0.045, label = "NIC", size = 4, colour = "white")+
  annotate("text", x = annual_salary_high/2, y = 0.65, label = "Take-Home Pay", size = 4)+
  labs(title = "Where does your money go?",
       subtitle = "Graph shows the percentage of money going towards Income Tax and National Insurance for different income levels.",
       y = "Percentage",
       x = "Income (£000s)")+
  theme(plot.subtitle=element_text(size=8, hjust=0, face="italic", color="#333333"),
        plot.margin = margin(5,20,0,0))

ggsave("AreaPlot.png", path = "C:/Users/MBUTLER1/OneDrive - Department for Transport/Stats & R/R Plots/Income Tax", height = 6, width = 6,  dpi = 700)
