## RQ1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 


## Load packages
library(tidyverse)

## Reading in the data will likely take a few seconds. Please be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Reshape the data
NEI_total <- NEI %>% 
  group_by(year) %>%
  summarize(total_emissions = sum(Emissions)/1000000)

## Construct the plot; save to a PNG file with dimensions of 480 x 480 pixels.
png("plot1.png", width=600, height=600)

with(NEI_total, plot(year, total_emissions, 
              type='l',
              col='red',
              xlab="Year",
              ylim=c(0, 8),
              ylab="Total PM2.5 Emissions (in million tons)", 
              main="Total U.S. Emissions of PM2.5: 1999 to 2008"
              )
     )
with(NEI_total, points(year, total_emissions, pch=19, col='steelblue'))

## Close graphics device
dev.off()
