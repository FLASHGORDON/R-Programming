# R-Programming
This repository contains all the projects that I have built using R. These projects are not mere codes written in R language
but they portray some useful insights extracted out of the data using statistics and graphic tools. R is a great language to 
extract meaning and trends out of the data. Here is some quick introduction about each project in this repository:

## Amazon Fine Food Reviews
Amazon is a huge complation of tons of data which in turn can provide great information about the users and sellers. This project
is a sentiment analysis of reviews of food sold at Amazon. 'tm' package is used to process these reviews to structured text corpus 
which only contains words that are meaningful for sentiment analysis. I have used 'sentimentR' package to calculate polarity of each
word and then conduct sentiment analysis to observe the sentiment of reviews of food section at Amazon.

## Domestic US-Flights
This dataset is a record of 3.5 Million+ US Domestic Flights from 1990 to 2009. It has been taken from OpenFlights website which have 
a huge database of different travelling mediums across the globe. I have done exploratory data analysis on this dataset to display 
underlying trends and information in the data.

## Elevate
Elevate is an Autonomous Pollution Surveillance Drone which can monitor pollution in a specified area without any human interference. 
Data is collected by sensors (CO2 level sensor and PM 2.5 level sensor) which are connected to raspberry pi. This raw data is then 
processed using R to extract some useful information which is displayed on the project's website using Ggplot2, Highcharter and Plotly 
graphics libraries. 

## Wine Reviews EDA
This kernel performs Exploratory Data Analysis with the help of two very powerful tools R i.e dplyr and ggplot2. These libraries are 
an essential part of R as they are easy to learn and very helpful in data analysis and visualisation.
Wine Reviews dataset contains reviews and descriptions of wines sold across the globe. This dataset consists of 10 attributes out of 
which 2 are numeric and others are characters. We’ll know more about this dataset as we go further in this kernel. I have also added 2 
new datasets for better visualisation in different plots. For some visualisations (mainly geographic plot) I have added a bunch of 
coordinates to pinpoint the location of the data. But for now let’s begin with loading libraries and datasets into the memory."
