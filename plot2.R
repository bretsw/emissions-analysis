## RQ2. Have total emissions from PM2.5 decreased in Baltimore City, Maryland 
## (fips == "24510") from 1999 to 2008? 


## Load packages
library(tidyverse)

## Reading in the data will likely take a few seconds. Please be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Reshape the data
NEI_baltimore <- NEI %>% 
  filter(fips == "24510") %>%
  group_by(year) %>%
  summarize(total_emissions = sum(Emissions))

## Construct the plot; save to a PNG file with dimensions of 480 x 480 pixels.
png("plot2.png", width=600, height=600)

with(NEI_baltimore, plot(year, total_emissions, 
                     type='l',
                     col='red',
                     #las=2,
                     xlab="Year",
                     ylim=c(0, 3500),
                     ylab="Total PM2.5 Emissions", 
                     main="Total Baltimore Emissions of PM2.5: 1999 to 2008"
                     )
     )
with(NEI_baltimore, points(year, total_emissions, pch=19, col='steelblue'))

## Close graphics device
dev.off()
