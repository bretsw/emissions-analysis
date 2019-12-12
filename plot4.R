## RQ4. Across the United States, how have emissions from coal combustion-related sources 
## changed from 1999–2008?


## Load packages
library(tidyverse)

## Reading in the data will likely take a few seconds. Please be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Reshape the data
NEI_SCC <- NEI %>%
  left_join(SCC, by='SCC')

#table(NEI_SCC$EI.Sector) %>% as.data.frame()

NEI_SCC_coal <- NEI_SCC %>% 
  filter(EI.Sector == "Fuel Comb - Comm/Institutional - Coal") %>%
  group_by(year) %>%
  mutate(total_emissions = sum(Emissions)) %>%
  ungroup()

## Construct the plot; save to a PNG file with dimensions of 480 x 480 pixels.
png("plot4.png", width=600, height=600)

ggplot(data=NEI_SCC_coal) +
  geom_point(color='steelblue', size=3, alpha=0.1, aes(x=year, y=log(Emissions))) +
  geom_smooth(method='lm', color='red', linetype=1, size=0.5, aes(x=year, y=log(Emissions)), se=FALSE) +
  geom_point(color='black', size=8, alpha=1, aes(x=year, y=log(total_emissions))) +
  geom_smooth(method='lm', color='red', linetype=1, size=0.5, aes(x=year, y=log(total_emissions)), se=FALSE) + 
  theme_bw(base_family='Times') +
  coord_cartesian(ylim=c(-10,10)) +
  xlab("Year") +
  ylab("PM2.5 Emissions (log scale)") +
  ggtitle("U.S. Emissions of PM2.5 from Coal Combustion: 1999 to 2008")

## Close graphics device
dev.off()
