## RQ6. Compare emissions from motor vehicle sources in Baltimore City (fips == "24510")
## with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
## Which city has seen greater changes over time in motor vehicle emissions?


## Load packages
library(tidyverse)

## Reading in the data will likely take a few seconds. Please be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Reshape the data
NEI_SCC <- NEI %>%
  left_join(SCC, by='SCC')

NEI_SCC_balt_la_mv_total <- NEI_SCC %>% 
  filter(fips == "24510" | fips == "06037",
         EI.Sector == "Mobile - On-Road Diesel Heavy Duty Vehicles" |
           EI.Sector == "Mobile - On-Road Diesel Light Duty Vehicles" |
           EI.Sector == "Mobile - On-Road Gasoline Heavy Duty Vehicles" |
           EI.Sector == "Mobile - On-Road Gasoline Light Duty Vehicles"
  ) %>%
  mutate(city = ifelse(fips == "24510", "Baltimore", "Los Angeles")) %>%
  group_by(city, year) %>%
  summarize(total_emissions = sum(Emissions))

## Construct the plot; save to a PNG file
png("plot6.png", width=600, height=600)

ggplot(data=NEI_SCC_balt_la_mv_total) +
  geom_point(color='steelblue', size=3, alpha=1, aes(x=year, y=total_emissions)) +
  geom_smooth(method='lm', color='red', linetype=1, size=1, se=FALSE, aes(x=year, y=total_emissions)) +
  facet_wrap(~ city, nrow=1) +
  theme_bw(base_family='Times') +
  xlab("Year") +
  ylab("PM2.5 Emissions") +
  ggtitle("Baltimore vs. Los Angeles Emissions of PM2.5 from Motor Vehicles: 1999 to 2008")

## Close graphics device
dev.off()



################################################################################
## Expanded version
################################################################################

## Reshape the data
NEI_SCC_balt_la_mv <- NEI_SCC %>% 
  filter(fips == "24510" | fips == "06037",
         EI.Sector == "Mobile - On-Road Diesel Heavy Duty Vehicles" |
           EI.Sector == "Mobile - On-Road Diesel Light Duty Vehicles" |
           EI.Sector == "Mobile - On-Road Gasoline Heavy Duty Vehicles" |
           EI.Sector == "Mobile - On-Road Gasoline Light Duty Vehicles"
  ) %>%
  mutate(city = ifelse(fips == "24510", "Baltimore", "Los Angeles")) %>%
  group_by(year) %>%
  mutate(total_emissions = sum(Emissions)) %>%
  ungroup()

## Construct the plot; save to a PNG file
png("plot6b.png", width=600, height=600)

ggplot(data=NEI_SCC_balt_la_mv) +
  geom_point(color='steelblue', size=3, alpha=0.1, aes(x=year, y=log(Emissions))) +
  geom_smooth(method='lm', color='red', linetype=1, size=1, se=FALSE, aes(x=year, y=log(Emissions))) +
  facet_grid(city ~ EI.Sector) +
  theme_bw(base_family='Times') +
  coord_cartesian(ylim=c(-15,15)) +
  xlab("Year") +
  ylab("PM2.5 Emissions (log scale)") +
  ggtitle("Baltimore vs. Los Angeles Emissions of PM2.5 from Motor Vehicles: 1999 to 2008")

## Close graphics device
dev.off()
