install.packages("tidyverse")
library(tidyverse)

# Loading in the csv files containing the data
plato24 <- read_csv("GFC21_003_24Plato_Data.csv")
plato16 <- read_csv("GFC21_003_16Plato_Data.csv")

# Filtering the data to contain either our products or competitors
plato24_comp <- plato24 %>% 
  filter(experiment %in% c("A","B","C","Hybrid"))
plato24_MEHSHG <- plato24 %>% 
  filter(!experiment %in% c("A","B","C","Hybrid"))
plato16_comp <- plato16 %>% 
  filter(experiment %in% c("A","B","C","Hybrid","ME Hard Selzter HG Low Dose"))
plato16_MEHSHG <- plato16 %>% 
  filter(!experiment %in% c("A","B","C","Hybrid","ME Hard Selzter HG Low Dose"))

# Plot looking at alcohol production between our product and competitors at 24 Plato over time
ggplot(data=plato24,aes(x=day,y=alcohol))+
  geom_smooth(data=plato24_MEHSHG,aes(color="ME Hard Seltzer HG"))+
  geom_line(data=plato24_comp,aes(color=experiment))+
  labs(title="24°Plato Fermentation Curve",
       x="Days",
       subtitle="Alcohol",
       y="Alcohol (%)",
       colour="Product")

# Plot looking at alcohol production between our product and competitors at 16 Plato over time
ggplot(data=plato16,aes(x=day,y=alcohol))+
  geom_smooth(data=plato16_MEHSHG,aes(color="ME Hard Seltzer HG"))+
  geom_line(data=plato16_comp,aes(color=experiment))+
  labs(title="16°Plato Fermentation Curve",
       x="Days",
       subtitle="Alcohol",
       y="Alcohol (%)",
       colour="Product")
