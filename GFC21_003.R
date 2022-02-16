install.packages("tidyverse")
library(tidyverse)

plato24 <- read_csv("GFC21_003_24Plato_Data.csv")
plato16 <- read_csv("GFC21_003_16Plato_Data.csv")

ggplot(data=plato24,aes(x=day,y=alcohol))+
  geom_smooth(data=plato24 %>% filter(!experiment %in% "A",!experiment %in% "B",!experiment %in% "C",!experiment %in% "Hybrid"),aes(color="ME Hard Seltzer HG"))+
  geom_line(data=plato24 %>% filter(!experiment %in% "11",!experiment %in% "12",!experiment %in% "13",!experiment %in% "14",!experiment %in% "15",!experiment %in% "16"),aes(color=experiment))+
  labs(title="24°Plato Fermentation Curve",x="Days",subtitle="Alcohol",y="Alcohol (%)",colour="Product")
