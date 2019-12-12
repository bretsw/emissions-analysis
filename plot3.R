## RQ3. Of the four types of sources indicated by the `type`
## (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen 
##decreases in emissions from 1999–2008 for Baltimore City? 
## Which have seen increases in emissions from 1999–2008? 


## Load packages
library(tidyverse)

## Reading in the data will likely take a few seconds. Please be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Reshape the data
NEI_SCC <- NEI %>%
  left_join(SCC, by='SCC')

NEI_SCC_baltimore <- NEI_SCC %>% 
  filter(fips == "24510") %>%
  group_by(year, type) %>%
  summarize(total_emissions = sum(Emissions)) %>%
  mutate(type = as.factor(type))

## Construct the plot; save to a PNG file with dimensions of 480 x 480 pixels.
png("plot3.png", width=600, height=600)

ggplot(data=NEI_SCC_baltimore, aes(x=year, y=total_emissions)) +
  geom_point(size=6, alpha=1.0, aes(color=type)) +
  geom_smooth(linetype=3, size=3, aes(color=type), se=FALSE) +
  geom_smooth(method='lm', linetype=1, size=0.5, aes(color=type), se=FALSE) +
  theme_bw(base_family='Times') +
  labs(color = "Source Type") +
  xlab("Year") +
  ylab("PM2.5 Emissions") +
  ggtitle("Baltimore Emissions of PM2.5: 1999 to 2008")

## Close graphics device
dev.off()
