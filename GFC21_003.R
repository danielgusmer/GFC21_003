install.packages("tidyverse")
library(tidyverse)
install.packages("rmarkdown")
library(rmarkdown)
install.packages("ggrepel")
library(ggrepel)

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

# Loading in the csv file containing Jasmin's 28P benchtop data
plato28 <- read_csv("GFC21_003_28Plato_Data.csv")

# Plot looking at alcohol production at 16, 24, and 28P
ggplot(data=plato28,aes(x=day,y=alcohol*100))+
    geom_smooth(data=plato16_MEHSHG,aes(color="16°Plato"))+
    geom_smooth(data=plato24_MEHSHG,aes(color="24°Plato"))+
    geom_smooth(data=plato28,aes(color="28°Plato"),se=F)+
    labs(title="Fermentation Curve",
         subtitle="Alcohol",
         x="Days",
         y="Alcohol (%)",
         colour="Degrees Plato")+
    scale_color_manual(name="Degrees Plato",
                       breaks=c("16°Plato","24°Plato","28°Plato"),
                       values=cbPalette)+
    scale_y_continuous(breaks=seq(0,21,2))

ggsave("pds_fermcurve_alcohol.png",width=8.5,height=5)

# Plot looking at plato drop at 16, 24, and 28P
ggplot(data=plato28,aes(x=day,y=plato))+
    geom_smooth(data=plato16_MEHSHG,aes(color="16°Plato"))+
    geom_smooth(data=plato24_MEHSHG,aes(color="24°Plato"))+
    geom_smooth(data=plato28,aes(color="28°Plato"),se=F)+
    labs(title="Fermentation Curve",
         subtitle="Plato",
         x="Days",
         y="°Plato",
         colour="Degrees Plato",)+
    scale_color_manual(name="Degrees Plato",
                       breaks=c("16°Plato","24°Plato","28°Plato"),
                       values=cbPalette)

ggsave("pds_fermcurve_plato.png",width=8.5,height=5)

# Loading in csv file containing data from Viva Toxicity study
toxicity <- read_csv("GFC21_003_Round3.csv")
toxic_dataend <- toxicity %>% 
    group_by(Run) %>% 
    top_n(1,Day)

# Plot looking at toxicity runs alcohol production
ggplot(data=toxicity,mapping=aes(x=Day,y=Alcohol*100))+
    geom_point(aes(color=as.factor(Run)))+
    geom_line(aes(color=as.factor(Run)),size=.75)+
    scale_color_manual(labels=c("24°P","25°P","26°P","27°P","28°P","29°P","30°P","31°P"),values=cbPalette)+
    labs(title="Fermentation Curve - Viva Toxicity Study",
         subtitle="Alcohol",
         x="Days",
         y="Alcohol (%)",
         colour="Run")+
    scale_y_continuous(breaks=seq(0,21,2))+
    geom_text_repel(aes(label=Alcohol*100),data=toxic_dataend,size=3)

ggsave("toxicity_alcohol.png",width=8.5,height=5)

# Plot looking at toxicity runs plato reduction
ggplot(data=toxicity,mapping=aes(x=Day,y=Plato))+
    geom_point(aes(color=as.factor(Run)))+
    geom_line(aes(color=as.factor(Run)),size=.75)+
    scale_color_manual(labels=c("24°P","25°P","26°P","27°P","28°P","29°P","30°P","31°P"),values=cbPalette)+
    labs(title="Fermentation Curve - Viva Toxicity Study",
         subtitle="Plato",
         x="Days",
         y="°Plato",
         colour="Run")

ggsave("toxicity_plato.png",width=8.5,height=5)

# Quick plot made at request of Nate for 3/11 meeting
ggplot(data=plato24_MEHSHG,mapping=aes(x=day,y=alcohol*100))+
    geom_smooth()+
    labs(title="24°Plato Fermentation Curve",
         subtitle="Alcohol",
         x="Days",
         y="Alcohol (%)")+
    scale_x_continuous(breaks=seq(0,10,1))

ggsave("24P_alcohol.png",width=8.5,height=5)

# 24P plot for sugar degradation
ggplot(data=plato24_MEHSHG,mapping=aes(x=day,y=plato))+
    geom_smooth()+
    labs(title="24°Plato Fermentation Curve",
         subtitle="Plato",
         x="Days",
         y="Plato")+
    scale_x_continuous(breaks=seq(0,10,1))

#filtering data to only contain 24-26 Plato Toxicity Runs
toxic_low <- toxicity %>% 
    filter(Run %in% c("33","34","35"))
toxic_low_dataend <- toxic_low %>% 
    group_by(Run) %>% 
    top_n(1,Day)

# Plot looking at alcohol production in 24-26 plato
ggplot(data=toxic_low,mapping=aes(x=Day,y=Alcohol*100))+
    geom_point(aes(color=as.factor(Run)))+
    geom_line(aes(color=as.factor(Run)),size=.75)+
    scale_color_manual(labels=c("24°P","25°P","26°P"),values=cbPalette)+
    labs(title="Fermentation Curve - Viva Toxicity Study",
         subtitle="Alcohol - 24 to 26°Plato",
         x="Days",
         y="Alcohol (%)",
         colour="Run")+
    scale_y_continuous(breaks=seq(0,21,2))+
    geom_text_repel(aes(label=Alcohol*100),data=toxic_low_dataend,size=3)

ggsave("toxic_low_alcohol.png",width=8.5,height=5)

# Plot looking at plato reduction in 24-26 plato
ggplot(data=toxic_low,mapping=aes(x=Day,y=Plato))+
    geom_point(aes(color=as.factor(Run)))+
    geom_line(aes(color=as.factor(Run)),size=.75)+
    scale_color_manual(labels=c("24°P","25°P","26°P"),values=cbPalette)+
    labs(title="Fermentation Curve - Viva Toxicity Study",
         subtitle="Plato - 24 to 26°Plato",
         x="Days",
         y="°Plato",
         colour="Run")+
    scale_y_continuous(breaks=seq(0,27,3))+
    geom_text_repel(aes(label=Plato),data=toxic_low_dataend,size=3)

ggsave("toxic_low_plato.png",width=8.5,height=5)
