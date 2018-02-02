library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(highcharter)
library(leaflet)

traindata <- read_tsv('chimps_16091-2010-08-03_17-08-31/flight_edges.tsv', 
                      col_names = c('Origin_airport',
                                    'Destination_airport',
                                    'Origin_city',
                                    'Destination_city',
                                    'Passengers',
                                    'Seats',
                                    'Flights',
                                    'Distance',
                                    'Fly_date',
                                    'Origin_population',
                                    'Destination_population'))

#changing date format and assuming that all flights took place on 01 date.
traindata$Fly_date <- as.character(traindata$Fly_date)
traindata$Fly_date <- paste(traindata$Fly_date, '01',sep = '')
traindata$Fly_date <- as.Date(traindata$Fly_date, '%Y%m%d')

#choosing less time for better performance
traindata <- traindata[year(traindata$Fly_date) >= 2008,]

traindata$Origin_code <- sub('.*[, ]', '', traindata$Origin_city)
traindata$Destination_code <- sub('.*[, ]', '',traindata$Destination_city)


#Before coming down to visualisation let's see summary of the data first
summary(traindata)
unique(traindata$Origin_city)
unique(traindata$Destination_city)

#Visualization

#no. of missing values in the data
as.data.frame(table(is.na(traindata))) %>%
  hchart('column',hcaes(Var1, Freq)) %>% hc_title(text = 'No. of Missing Values') %>%
  hc_add_theme(hc_theme_google())


#Lets look at some box plots of the data

plot_ly(type = 'box') %>%
        add_boxplot(y = ~traindata$Passengers, name = 'Passengers') %>%
add_boxplot(
        y = ~traindata$Seats, name = 'Seats')

plot_ly(type= 'box') %>%
add_boxplot(
        y = ~traindata$Flights, name = 'Flights')

plot_ly(type = 'box') %>%
  add_boxplot( y = ~traindata$Distance, name = 'Distance' )

#lets add limits to take a closer look at these box plots

# plot_ly(type = 'box') %>%
#   add_boxplot(y = ~traindata$Passengers, name = 'Passengers') %>% 
#   add_boxplot(y = ~traindata$Seats, name = 'Seats') %>% 
#   layout(yaxis = list(range = c(1,11000), title = ''))
# 
# plot_ly(type = 'box') %>%
#   add_boxplot(y = ~traindata$Flights, name = 'Flights') %>%
#   layout(yaxis = list(range = c(1,140), title = ''))
# 
# plot_ly(type = 'box') %>%
#   add_boxplot(y = ~traindata$Distance, name = 'Distance') %>%
#   layout(yaxis = list(range = c(1,2000), title = ''))


#Origin and destination cities
temp <- traindata[!duplicated(traindata[,'Origin_city']),]
temp <- aggregate(temp$Destination_population, 
                  by = list(temp$Origin_code), sum)

temp$x <- temp$x/1000000

hcmap("countries/us/us-all", data = temp, value = "x",
      joinBy = c("hc-a2", "Group.1"), name = "State",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(valueDecimals = 2, valueSuffix = ' Million')) %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0,90,by = 20),100))) %>%
  hc_legend(layout = 'vertical', align = 'right', floating = TRUE, valueDecimals = 0,
            valueSuffix = 'M') %>%
  hc_title(text = 'Population in different states')


temp <- traindata[!duplicated(traindata[,'Origin_city']),]
temp <- aggregate(temp$Origin_population, 
                  by = list(temp$Origin_code), sum)
colnames(temp)[2] <- 'Population'
passenger_population <- aggregate(traindata$Passengers, 
                                  by = list(traindata$Origin_code),
                                  sum)
passenger_population <- left_join(passenger_population, temp, by = "Group.1")
colnames(passenger_population)[2] <- 'Passengers'
passenger_population$Group.1 <- factor(passenger_population$Group.1, levels = passenger_population$Group.1[order(passenger_population$Population)])

plot_ly(passenger_population, x= ~Group.1, y= ~Passengers, type ='bar', name = 'Passengers') %>%
  add_bars( y = ~Population, name = 'Population') %>%
  layout(yaxis = list(title = 'Number of People', barmode = 'group'),
         title = 'Origin_population vs Passengers boarded from Origin' )

temp <- traindata[!duplicated(traindata[,'Destination_city']),]
temp <- aggregate(temp$Destination_population, 
                  by = list(temp$Destination_code), sum)
colnames(temp)[2] <- 'Population'
passenger_population <- aggregate(traindata$Passengers, 
                                  by = list(traindata$Destination_code),
                                  sum)
passenger_population <- left_join(passenger_population, temp, by = "Group.1")
colnames(passenger_population)[2] <- 'Passengers'
passenger_population$Group.1 <- factor(passenger_population$Group.1, levels = passenger_population$Group.1[order(passenger_population$Passengers)])

plot_ly(passenger_population, x= ~Group.1, y= ~Passengers, type ='bar', name = 'Passengers') %>%
  add_bars( y = ~Population, name = 'Population') %>%
  layout(yaxis = list(title = 'Number of People', barmode = 'group'),
         title = 'Destination_population vs Passengers boarded to Destination' )

#which months is most active month for flights

traindata$month <- months(traindata$Fly_date)
temp <- aggregate(traindata$Flights, 
                  by = list(factor(traindata$month, levels= month.name),
                            factor(year(traindata$Fly_date))),
                  sum)
temp <- ts(temp$x, start = c(2008,1), end=c(2009,12), frequency = 12)

hchart(temp, 
       name = 'No. of flights') %>%
  hc_add_theme(hc_theme_google())


#passengers in different months
temp <- aggregate(traindata$Passengers, 
                  by = list(factor(traindata$month, levels= month.name),
                            year(traindata$Fly_date)),
                  sum)
Passengers <- ts(temp$x, start = c(2008,1), end=c(2009,12), frequency = 12)
hchart(Passengers) %>%
  hc_add_theme(hc_theme_db())

#Locations of the airport
locations <- read_csv('Locations.csv')
locations$Longitude <- as.numeric(locations$Longitude)
locations$Latitude <- as.numeric(locations$Latitude)

temp <- data_frame(Address = unique(traindata$Destination_airport))
locations <- right_join(locations, temp, by = 'Address')

plot_geo(sizes = c(20,100)) %>%
  add_markers(x = ~locations$Longitude, y = ~locations$Latitude,
              alpha = 0.7, text = ~locations$Address
  ) %>%
  layout(geo = list(
    scope = 'north america', showland = TRUE, landcolor = toRGB("gray95")
    
  ),title = 'Locations of the Airports') 

#Top 10 Flights on map
temp <- aggregate(traindata$Passengers,
                  by = list(traindata$Origin_airport,traindata$Destination_airport),
                  sum) %>% arrange(desc(x))



topflights <- head(temp, n = 30L)
index <- NA
for(i in 1:30){
  for(j in i:30)
  if(topflights$Group.1[i] ==topflights$Group.2[j] & topflights$Group.2[i] == topflights$Group.1[j]){
    index <- c(index,i+1)
  }
}

topflights <- topflights[-index[-1],]


topflights <- left_join(topflights,
                        read_csv('Locations.csv', col_names = c('Group.1',
                                                                'Origin_Latitude',
                                                                'Origin_Longitude')),
                        by = 'Group.1')

topflights <- left_join(topflights,
                        read_csv('Locations.csv', col_names = c('Group.2',
                                                                'Destination_Latitude',
                                                                'Destination_Longitude')),
                        by = 'Group.2')



plot_geo(locationmode = 'USA-states', sizes = c(100,250)) %>%
  add_markers(x = ~topflights$Origin_Longitude,y = ~topflights$Origin_Latitude,
              size = ~topflights$x, 
              text = ~paste(topflights$Group.1,'<br />',topflights$x/1e6,' million')) %>%
  add_markers(x = ~topflights$Destination_Longitude, y =~topflights$Destination_Latitude,
              size = ~topflights$x, 
              text = ~paste(topflights$Group.2,'<br />',topflights$x/1e6,' million')) %>%
  add_segments(x = ~topflights$Origin_Longitude, xend= ~topflights$Destination_Longitude,
               y = ~topflights$Origin_Latitude, yend = ~topflights$Destination_Latitude,
               color = ~topflights$Group.1
              ) %>%
  layout(geo = list(
    scope = 'usa', showland = TRUE, landcolor = toRGB("gray95")
  ))






#LAST 10 flights on map

temp <- aggregate(traindata$Passengers,
                  by = list(traindata$Origin_airport,traindata$Destination_airport),
                  sum) %>% arrange(desc(x))



lastflights <- tail(temp, n = 15L)

lastflights <- left_join(lastflights,
                        read_csv('Locations.csv', col_names = c('Group.1',
                                                                'Origin_Latitude',
                                                                'Origin_Longitude')),
                        by = 'Group.1')

lastflights <- left_join(lastflights,
                        read_csv('Locations.csv', col_names = c('Group.2',
                                                                'Destination_Latitude',
                                                                'Destination_Longitude')),
                        by = 'Group.2')



plot_geo(sizes = c(1,30)) %>%
  add_markers(x = ~lastflights$Origin_Longitude,y = ~lastflights$Origin_Latitude,
              size = ~lastflights$x, 
              text = ~paste(lastflights$Group.1,'<br />',lastflights$x/1e6,' million')) %>%
  add_markers(x = ~lastflights$Destination_Longitude, y = ~lastflights$Destination_Latitude,
              size = ~lastflights$x, 
              text = ~paste(lastflights$Group.1,'<br />',lastflights$x/1e6,' million')) %>%
  add_segments(x = ~lastflights$Origin_Longitude, xend= ~lastflights$Destination_Longitude,
               y = ~lastflights$Origin_Latitude, yend = ~lastflights$Destination_Latitude,
               color = ~lastflights$Group.1
  ) %>%
  layout(geo = list(
    scope= 'north america',showland = TRUE, landcolor = toRGB("gray95"),showframe = FALSE
  ))


#Longest flights 
temp <- aggregate(traindata$Distance, by = list(traindata$Origin_airport,
                                                traindata$Destination_airport)
                  ,max) %>% arrange(desc(x))


longflights <- head(temp, 15L)

longflights <- left_join(longflights,
                         read_csv('Locations.csv', col_names = c('Group.1',
                                                                 'Origin_Latitude',
                                                                 'Origin_Longitude')),
                         by = 'Group.1')

longflights <- left_join(longflights,
                         read_csv('Locations.csv', col_names = c('Group.2',
                                                                 'Destination_Latitude',
                                                                 'Destination_Longitude')),
                         by = 'Group.2')



plot_geo(sizes = c(1,250)) %>%
  add_markers(x = ~longflights$Origin_Longitude,y = ~longflights$Origin_Latitude,
              size = ~longflights$x, 
              text = ~paste(longflights$Group.1,'<br />',longflights$x,' miles')) %>%
  add_markers(x = ~longflights$Destination_Longitude, y = ~longflights$Destination_Latitude,
              size = ~longflights$x, 
              text = ~paste(longflights$Group.1,'<br />',longflights$x,' miles')) %>%
  add_segments(x = ~longflights$Origin_Longitude, xend= ~longflights$Destination_Longitude,
               y = ~longflights$Origin_Latitude, yend = ~longflights$Destination_Latitude,
               color = ~longflights$Group.1
  ) %>%
  layout(geo = list(
    scope = 'usa',
    showland = TRUE, landcolor = toRGB("gray95"),showframe = FALSE
  ),title = 'Longest Flights (In terms of Distance)'
  )




#Passengers to seat ratio

temp <- aggregate(traindata$Passengers, 
                  by=list(factor(traindata$month, levels = month.name),
                          year(traindata$Fly_date)), sum)
temp1 <- aggregate(traindata$Seats, 
                  by= list(factor(traindata$month, levels = month.name),
                           year(traindata$Fly_date)), sum)
Passengers<- ts(temp$x, start = c(2008,1),end = c(2009,12), frequency = 12)
Seats <- ts(temp1$x, start=c(2008,1),end = c(2009,12), frequency = 12)
hchart(cbind(Passengers,Seats)) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_title(text = 'Passengers vs Seats in Specific month ( Hover for more info)')



#most empty flights in USA
traindata$left_seats <- traindata$Seats - traindata$Passengers
temp <- aggregate(traindata$left_seats, 
                  by= list(traindata$Origin_airport,
                           traindata$Destination_airport),max) %>% arrange(desc(x))

colnames(temp) <- c('Origin_airport','Destination_airport','left_seats')
temp <- left_join(temp, traindata, by = c('Origin_airport','Destination_airport','left_seats'))
temp$left_seats <- temp$left_seats/temp$Flights
index <- NA
topemptyflights <- head(temp, n = 30L)
for(i in 1:30){
  for(j in i:30)
    if(topemptyflights$Origin_airport[i] == topemptyflights$Destination_airport[j] & topemptyflights$Origin_airport[j] == topemptyflights$Destination_airport[i]){
      index <- c(index,i+1)
    }
}

topemptyflights <- topemptyflights[-index[-1],]


topemptyflights <- left_join(topemptyflights,
                         read_csv('Locations.csv', col_names = c('Origin_airport',
                                                                 'Origin_Latitude',
                                                                 'Origin_Longitude')),
                         by = 'Origin_airport')

topemptyflights <- left_join(topemptyflights,
                         read_csv('Locations.csv', col_names = c('Destination_airport',
                                                                 'Destination_Latitude',
                                                                 'Destination_Longitude')),
                         by = 'Destination_airport')



plot_geo(sizes = c(1,250)) %>%
  add_markers(x = ~topemptyflights$Origin_Longitude,y = ~topemptyflights$Origin_Latitude,
              size = ~topemptyflights$left_seats, 
              text = ~paste(
                topemptyflights$Origin_airport,
                '<br />',
                format(round(topemptyflights$left_seats,2), nsmall = 2),
                ' seats per flight','<br />',
                topemptyflights$Flights, ' flights')) %>%
  add_markers(x = ~topemptyflights$Destination_Longitude, y = ~topemptyflights$Destination_Latitude,
              size = ~topemptyflights$left_seats, 
              text = ~paste(
                topemptyflights$Destination_airport,
                '<br />',
                format(round(topemptyflights$left_seats,2), nsmall = 2),
                ' seats per flight', '<br />',
                topemptyflights$Flights, ' flights')) %>%
  add_segments(x = ~topemptyflights$Origin_Longitude, xend= ~topemptyflights$Destination_Longitude,
               y = ~topemptyflights$Origin_Latitude, yend = ~topemptyflights$Destination_Latitude,
               color = ~topemptyflights$Origin_airport
  ) %>%
  layout(geo = list(
    scope = 'north america',
    showland = TRUE, landcolor = toRGB("gray95"),showframe = FALSE
  ),title = 'Top empty flights (Hover for more Info)'
  )


temp <- aggregate(traindata$Passengers, 
                  by = list(traindata$Distance,
                            traindata$Origin_airport,
                            traindata$Destination_airport),
                  sum) %>% arrange(desc(Group.1))

colnames(temp) <- c("Distance",'Origin_airport', 'Destination_airport', 
                    'Passengers')

plot_ly(temp, x =~Distance, y =~Passengers, type = 'scatter',
        color = ~Distance, size = ~Distance,
        text = ~paste('Distance :',temp$Distance,'<br />',
                      'Passengers :',temp$Passengers)) %>% hide_colorbar()

library(psych)

pairs.panels(sample_n(traindata,10000)[,c(5:8,10,11)])












  




















