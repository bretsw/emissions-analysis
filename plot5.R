## RQ5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?


## Load packages
library(tidyverse)

## Reading in the data will likely take a few seconds. Please be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Reshape the data
NEI_SCC <- NEI %>%
  left_join(SCC, by='SCC')

#table(NEI_SCC$EI.Sector) %>% as.data.frame()

NEI_SCC_balt_mv <- NEI_SCC %>% 
  filter(fips == "24510",
         EI.Sector == "Mobile - On-Road Diesel Heavy Duty Vehicles" |
           EI.Sector == "Mobile - On-Road Diesel Light Duty Vehicles" |
           EI.Sector == "Mobile - On-Road Gasoline Heavy Duty Vehicles" |
           EI.Sector == "Mobile - On-Road Gasoline Light Duty Vehicles"
         ) %>%
  group_by(year) %>%
  mutate(total_emissions = sum(Emissions)) %>%
  ungroup()

#NEI_SCC_balt_mv$EI.Sector %>% table() %>% as.data.frame()

## Construct the plot; save to a PNG file with dimensions of 480 x 480 pixels.
png("plot5.png", width=600, height=600)

ggplot(data=NEI_SCC_balt_mv) +
  geom_point(color='steelblue', size=3, alpha=0.1, aes(x=year, y=log(Emissions))) +
  geom_smooth(method='lm', color='red',linetype=1, size=1, se=FALSE, aes(x=year, y=log(Emissions))) +
  facet_wrap(~ EI.Sector, nrow=2) +
  theme_bw(base_family='Times') +
  coord_cartesian(ylim=c(-15,15)) +
  xlab("Year") +
  ylab("PM2.5 Emissions (log scale)") +
  ggtitle("Baltimore Emissions of PM2.5 from Motor Vehicles: 1999 to 2008")

## Close graphics device
dev.off()
