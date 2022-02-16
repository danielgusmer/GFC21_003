install.packages("tidyverse")
library(tidyverse)

# Loading in the csv files containing the data
plato24 <- read_csv("GFC21_003_24Plato_Data.csv")
plato16 <- read_csv("GFC21_003_16Plato_Data.csv")

# Filtering the data to contain either our products or competitors
plato24_comp <- plato24 %>% 
  filter(product %in% c("A","B","C","Hybrid"))
plato24_MEHSHG <- plato24 %>% 
  filter(product %in% c("ME Hard Seltzer HG"))
plato16_comp <- plato16 %>% 
  filter(product %in% c("A","B","C","Hybrid","ME Hard Selzter HG Low Dose"))
plato16_MEHSHG <- plato16 %>% 
  filter(product %in% c("ME Hard Seltzer HG"))

# Colorblind-friendly palette with black
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Plot looking at alcohol production between our product and competitors at 24 Plato over time
ggplot(data=plato24,aes(x=day,y=alcohol*100,color=product,linetype=product))+
  geom_smooth(data=plato24_MEHSHG)+
  geom_line(data=plato24_comp,size=.75)+
  labs(title="24°Plato Fermentation Curve",
       subtitle="Alcohol",
       x="Days",
       y="Alcohol (%)",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=c("dashed","twodash","longdash","dotdash","solid","dotted"))+
  annotate(geom="text",x=10,y=12.5,label="Hybrid is a yeast/nutrient combination")

ggsave("24p_compreveiw_alcohol.png",width=8.5,height=5)

# Plot looking at alcohol production between our product and competitors at 16 Plato over time
ggplot(data=plato16,aes(x=day,y=alcohol*100,color=product,linetype=product))+
  geom_smooth(data=plato16_MEHSHG)+
  geom_line(data=plato16_comp,size=.75)+
  labs(title="16°Plato Fermentation Curve",
       subtitle="Alcohol",
       x="Days",
       y="Alcohol (%)",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=c("dashed","twodash","longdash","dotdash","solid","dotted"))+
  annotate(geom="text",x=10,y=2.5,label="Hybrid is a yeast/nutrient combination")+
  annotate(geom="text",x=10.5,y=10,label="Low dosage of ME Hard Seltzer HG")+
  annotate(geom="text",x=9.95,y=9.5,label="results in similar performance")+
  annotate(geom="text",x=9.15,y=9,label="to closest competitor")

ggsave("16p_compreview_alcohol.png",width=8.5,height=5)
