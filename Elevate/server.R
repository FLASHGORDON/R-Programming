library(shiny)
library(shinydashboard)
library(plotly)
library(quantmod)
library(highcharter)
library(lubridate)
library(dplyr)
library(purrr)
library(RColorBrewer)
library(RCurl)
library(data.table)



shinyServer(function(input, output) {
  
  options(digits = 10)
    
  id <- "1NzHMlWurOFurUBgZS5n1mVDFF6jlPY0_" # google file ID
  data <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id),
                   col.names = c('Date','PPM','Dust'))
  
  data$PPM <- as.numeric(as.character(data$PPM))
  data$Dust <- as.numeric(as.character(data$Dust))
  data$Date <- as.character(data$Date)

  data <- data[grep('^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:', data$Date),]
  
  data$posix <- as.POSIXct(data$Date)
  
  data$hour <- hour(data$posix)
  
  data <- filter(data, PPM >= median(data$PPM, na.rm = T)-50 & PPM <= median(data$PPM, na.rm = T)+200 )
  data <- filter(data, Dust <= median(data$Dust, na.rm = T)+40 & Dust > 0)

  a <- c(77.049654,28.5033158,77.0496565,28.5032897,77.049667,28.5032546,77.0496914,28.5032897,77.049654,28.5033158,77.049667,28.5032546,77.0495868,28.5031389,77.0494286,28.503128,77.0495224,28.5030705,77.0496377,28.5031059,77.0496833,28.5032002,77.049667,28.5032546,77.0494634,28.5034406,77.0493873,28.5034261,77.049509,28.503417,77.049654,28.5033158,77.0496485,28.5033746,77.0495841,28.5034217,77.0494634,28.5034406,77.0494286,28.503128,77.0493802,28.5031577,77.0492193,28.5032143,77.0491638,28.5032492,77.049163,28.5031931,77.0493802,28.5031247,77.0494286,28.503128,77.0491638,28.5032492,77.0491657,28.503384,77.0493873,28.5034261,77.0492569,28.5034359,77.0491013,28.5034312,77.0490879,28.5032968,77.0491638,28.5032492)
  b <- c(77.049619,28.5033062,77.0495895,28.5033416,77.0495412,28.5033439,77.0495626,28.5033769,77.0495385,28.5034076,77.0495063,28.5034029,77.0494983,28.5033746,77.0494527,28.5034099,77.0494527,28.5034429,77.0494232,28.5034359,77.0494151,28.5034029,77.0493749,28.5034076,77.0493454,28.5034288,77.0493454,28.5034052,77.0492944,28.5034052,77.049273,28.5033722,77.0492998,28.5033652,77.0492595,28.5033463,77.0492193,28.5033887,77.0492515,28.5034217,77.0492193,28.5034312,77.0491898,28.5033887,77.0492032,28.5033557,77.0491683,28.5033463,77.0491335,28.5033392,77.0491737,28.5033133,77.0491657,28.5032756,77.0491657,28.5032567,77.0491764,28.5032332,77.0491844,28.5032025,77.0491952,28.5031742,77.04923,28.5031648,77.0492515,28.5031459,77.0492622,28.5031294,77.0492998,28.5031294,77.0492998,28.5031294,77.0493481,28.5031342,77.0494044,28.5031436,77.0494044,28.5031059,77.04945,28.5031294,77.0494902,28.5031742,77.0494232,28.5031978,77.049391,28.5032355,77.0494741,28.5032261,77.049509,28.5031954,77.0495331,28.5031624,77.0495278,28.5031412,77.0495787,28.5031601,77.0496136,28.5031766,77.0496351,28.5031554,77.0496646,28.5032002,77.0496377,28.5032237,77.0496485,28.5032662,77.049619,28.5033062,77.0496163,28.503351,77.0495975,28.5033652,77.049576,28.5034147,77.0495492,28.5034312,77.0495063,28.5034406,77.0494527,28.5034618,77.0494232,28.5034618,77.0493722,28.5034547,77.0493454,28.5034594,77.0492971,28.5034642,77.0492461,28.5034547,77.0492193,28.5034477,77.0491871,28.5034453,77.0491791,28.5034288,77.0491549,28.5034052,77.0491335,28.5033958,77.0491281,28.5033769,77.0491335,28.5033392,77.0491362,28.5033133,77.0491335,28.5032921,77.0491335,28.5032709,77.0491442,28.5032426,77.049163,28.5032025,77.0491952,28.5031742,77.0491952,28.5031436,77.0492059,28.5031177,77.0492381,28.5030917,77.0492756,28.503087,77.0493105,28.503087,77.0493427,28.503087,77.0493829,28.503087,77.0494258,28.5030917,77.0494553,28.5031012,77.0494848,28.5031059,77.0495331,28.5031082,77.0495653,28.5031177,77.0496082,28.5031318,77.0496351,28.5031554,77.049686,28.5031742,77.0497021,28.5032261,77.0496967,28.5032614,77.0496833,28.5033062,77.0496726,28.5033604,77.0496565,28.5033958,77.0496431,28.5034194,77.0496082,28.5034453,77.0495573,28.5034642,77.0495251,28.5034524,77.0494768,28.5034264,77.0494527,28.5034099,77.0493937,28.503384,77.0493615,28.5033746,77.0492998,28.5033652,77.0492595,28.5033463,77.0492381,28.5033227,77.0492032,28.5033133,77.0491362,28.5033133,77.0490798,28.5033675,77.0491013,28.5034123,77.0491308,28.5034429,77.0491496,28.5034618,77.0491898,28.5034759,77.0492435,28.5034901,77.0492917,28.5034854,77.0493481,28.5034877,77.0493776,28.5034924,77.0494258,28.5035019,77.0494741,28.5035066,77.0495009,28.5035042,77.0495278,28.5034948,77.0490772,28.5033227,77.0490611,28.5033062,77.0490262,28.5032944,77.0490181,28.5033439,77.0490047,28.5034005,77.0489886,28.5034524,77.0489886,28.5034972,77.0490262,28.5035207,77.0490825,28.5035254,77.0491201,28.5035184,77.0490798,28.5034547,77.0490369,28.5034359,77.0490342,28.5033722,77.0489699,28.5032874,77.0489699,28.5032473,77.0490396,28.5032402,77.049104,28.5032332,77.0490047,28.5031436,77.048986,28.5031035,77.0490181,28.5030611,77.0491228,28.5030187,77.0492059,28.5030163,77.0492998,28.5030116,77.0493615,28.5030116,77.0494366,28.503014,77.0494956,28.503021,77.049576,28.503021,77.0496458,28.5030446,77.0496833,28.5030965,77.0496914,28.5031153)
  Lat <- b[seq(0,length(b),2)]
  Long <- b[seq(1,length(b),2)]
  
  data$Lat <- cbind(data$PPM, Lat)[,2]
  data$Long <- cbind(data$PPM, Long)[,2]
  
  new_data <- data[8000:8193,]
  
  data <- arrange(data,by = posix)
  output$meanPlotPPM <- renderHighchart({
    temp <- as.data.table(data[,c(2,3,5)])
    ans <- temp[,.(mean(PPM), mean(Dust)), by = .(hour)]
    ans <- arrange(ans, hour)
    colnames(ans) <- c('hour', 'Mean PPM', 'Mean Dust Density')
    hchart(ans,'column', hcaes(x= hour, y = `Mean PPM`), color = 'red', name ='PPM') %>%
      hc_title(
        text = 'Mean CO2 PPM Per Hour'
      )
    
  })
  
  output$meanPlotDust <- renderHighchart({
    temp <- as.data.table(data[,c(2,3,5)])
    ans <- temp[,.(mean(Dust)), by = .(hour)]
    ans <- arrange(ans,hour)
    colnames(ans) <- c('hour', 'Mean Dust Density')
    hchart(ans, 'column', hcaes(x = hour, y= `Mean Dust Density`), name = 'Dust') %>%
      hc_title(
        text = 'Mean Dust Density Per Hour'
      )
  })    
    output$histogram <- renderPlotly({
      plot_ly(data, type='scatter', x = ~posix,y = ~Dust, mode = 'line', name = 'Dust Density',
              line = list(color = 'rgb(205, 12, 24)', width = 3)) %>%
        layout(xaxis = list(
          rangeslider = list(type = 'date'),
          title = 'Time'
        ),
        hovermode = 'compare')
    })
    
    output$histogram2 <- renderPlotly({
      plot_ly(data, type='scatter', x = ~posix, y = ~PPM, mode = 'line', name = 'PPM',
              line = list(color = 'rgb(22, 96, 167)', width = 3)) %>%
        layout(xaxis = list(
          rangeslider = list(type = 'date'),
          title = 'Time'
        ),
        hovermode = 'compare')
    })
    output$msgOutput <- renderMenu({
      temp <- read.csv('Sample.csv')
      msgs <- apply(temp,1,
                    function(row){
                      messageItem(from = row[['from']], message = row[['message']])
                    })
      
      dropdownMenu(type = 'messages', .list = msgs)
    })
    
    output$PollutionMean <- renderInfoBox({
      avg <- mean(data$PPM, na.rm = T)
      infoBox('Pollution Mean', paste(format(round(avg,2),nsmall = 2),'ppm'), icon = icon('bar-chart-o'), color = 'red')
    })
    
    output$DustMean <- renderInfoBox({
      avg <- mean(data$Dust, na.rm = T)
      infoBox('Dust Density Mean', paste(format(round(avg,2),nsmall = 2),'ug/mm3'), icon = icon('industry'), color = 'red')
    })
    
    output$HourTravelled <- renderInfoBox({
      x <- data$posix
      travel <- tail(x,1) - head(x,1)
      infoBox('Drone Uptime', format(round(travel,2), nsmall = 2), icon = icon('angellist'), color = 'red')
        
    })
    
    #Map
    output$fancyPlot <- renderHighchart({
      n=194
      z <- new_data$PPM
      seq <- map2(1:n,z,function(x,y){ifelse(x==1:n,y,0)})

      df <- data.frame(
        lat = new_data$Lat,
        lon = new_data$Long,
        z= z,
        color = colorize(z),
        sequence = seq
      )

      hcmap() %>%
        hc_add_series(data = df, type = "mapbubble",
                      minSize = 0, maxSize = 30)

    })

    sliderValues <- reactive({
      temp <- new_data[new_data$PPM >= input$PPM[1] & new_data$PPM <= input$PPM[2],]
      
    })
    
    output$anotherPlot <- renderLeaflet({
      leaflet(new_data) %>% addTiles() %>%
        fitBounds(~min(Long), ~min(Lat), ~max(Long), ~min(Lat))
    })

    colorpal <- reactive({
      colorNumeric(input$colors,data$PPM)
    })

    observe({
      pal <- colorpal()
      leafletProxy('anotherPlot', data= sliderValues()) %>%
        clearShapes() %>%
        addCircles(radius = ~0.02*PPM, color = '#777777', weight = 1,fillColor = ~pal(PPM), fillOpacity = 0.7,
                   popup = ~paste('PPM: ',PPM))
    })


    observe({
      proxy <- leafletProxy("anotherPlot", data = sliderValues())
      proxy %>% clearControls()
      if (input$legend) {
        pal <- colorpal()
        proxy %>% leaflet::addLegend(pal = pal, values = ~PPM)
      }
    })
    
})
