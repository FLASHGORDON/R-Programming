library(shiny)
library(shinydashboard)
library(plotly)
library(highcharter)
library(leaflet)
library(RColorBrewer)
library(whisker)



shinyUI(
  dashboardPage( 
    skin = 'red',
    
    dashboardHeader(title = 'Elevate', 
                    dropdownMenuOutput('msgOutput')
                    ),
    dashboardSidebar(
      
      sidebarMenu(
        sidebarSearchForm('searchText','buttonSearch',
                          'search'),
      menuItem('Statistics',
               tabName = 'Graphical_Representation',
               icon= icon('dashboard')),
      menuItem('Average Pollution',
               tabName = 'Raw_Data', icon = icon('dashboard')),
      menuItem('Pollution',
               tabName = 'New_tab',icon= icon('dashboard'))
      )
      
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'Graphical_Representation',
                fluidRow(
                  infoBoxOutput('PollutionMean'),
                  infoBoxOutput('DustMean'),
                  infoBoxOutput('HourTravelled')
                ),
                fluidRow(
                  column(12,
                    plotlyOutput('histogram'))
                ),
                fluidRow(
                  column(12,
                    plotlyOutput('histogram2'))
                )
                
                ),
        tabItem(tabName = 'Raw_Data',
                fluidRow(
                    highchartOutput('meanPlotPPM')
                  
                ),
                fluidRow(
                  highchartOutput('meanPlotDust')
                )  
                ),
        tabItem(tabName = 'New_tab',
                column(
                  9,
                  leafletOutput('anotherPlot')
                ),
                column(
                  2,
                  absolutePanel(
                    sliderInput(
                      'PPM', 'PPM :',min = 50, max = 500,
                      value = c(50,500)
                    ),
                    selectInput("colors", "Color Scheme",
                                rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                    ),
                    checkboxInput("legend", "Show legend", TRUE)
                  )

                )
                   
              )
            
            )
           
                
      )
      
    )
)
