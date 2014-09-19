# nstall and import libraries
install.packages('ggplot2')
library(ggplot2)

install.packages('dplyr')
library(dplyr)

install.packages('zoo')
library(zoo)

# load mock data
mock_data <- read.csv("~/Desktop/Eric/MockData_6a.csv")
str(mock_data)
View(mock_data)

# add additional column for date in Month-year format
# this will also summarize different dates in a month into just the month
# (we add an additional column so we keep the original data)
mock_data$Date2 <- as.yearmon(mock_data$Date, format = '%m/%d/%y')
str(mock_data)

# covert Date2 column to POSIXlt format
mock_data$Date2 <- as.Date(mock_data$Date2) 
str(mock_data)

### create dataframe to group by date and Geography
Date_Mon_Yr <- group_by(mock_data, Date2, Geography)
sum_data <- summarize(Date_Mon_Yr, Resignations = sum(Resignations))
View(sum_data)

### Create data for moving average
# group all data by months
mock_data_months <- group_by(sum_data, Date2)

# create data frame for sum of resignations per month
mock_data_res <- summarize(mock_data_months, Resignations = sum(Resignations))
View(mock_data_res)
str(mock_data_res)

# calculate historical 6-month average
mock_data_res$MA <- rollmean(mock_data_res$Resignations, 6, align = 'right', fill = c(NA, NULL, NA))
View(mock_data_res)
str(mock_data_res)

### Plot ggplot
#base layer
ggplot(sum_data, aes(Date2, Resignations)) +
  
  # layer for bars of Geography
  geom_bar(stat='identity', position = 'dodge', aes(fill = Geography)) + 
  
  # layer for moving average
  geom_line(data = mock_data_res, size = 1, aes(y = MA/4, width = 10)) + 
  
  # labeling titles, axes, and legend
  labs(title = "Plot of Resignations by Geography across time", x = "Date (Months)", y = "No. of Resignations", fill=guide_legend(title="Geography"), size = 20) + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"), title=element_text(size=14,face="bold")) 

