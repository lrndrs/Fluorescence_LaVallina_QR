### Plot for discussion: variability in drip water fluorescence

## Load Library

library(ggplot2)
library(lubridate)
library(tidyverse)
library(ggpubr)
library(tidypaleo)
library(tidyr)
library(dplyr)
library(cowplot)

library(ggbiplot)
library(factoextra)
library(tinytex)
library(readxl)

#load data
df_climate2 <- readRDS(file="03_Rdata/dfclimate2_allindicesCaveClimaERA5.rds")
thick <- read_excel(path ="02_CaveData/Geology/POR_Thickness.xlsx") 
thick$loc <- thick$loc_abbr

df_climate2_thick <- merge(df_climate2,thick,by='loc')



df_climate2_thick$loc <- as.factor(df_climate2_thick$loc)

library(scico)
p1 <- df_climate2_thick %>% mutate(loc = fct_relevel(loc, 
                                                     "FOR","SNO", "GLO","GRA","PLA","SKY","MUS")) %>%  
  ggplot(aes(x=as.factor(thickness),y=Comp.1+Comp.5,colour=loc))+
  geom_boxplot() + theme_classic2() +labs(x="Cover thickness [m]")+
  scale_color_scico_d(palette="romaO",begin = 0.1,
                      end = 0.9)



## values to calc
allfluo <- df_climate2_thick %>% mutate(loc = fct_relevel(loc, 
                                               "FOR","SNO", "GLO","GRA","PLA","SKY","MUS")) %>% 
  select(loc,allfluo,Comp.1,Comp.2,Comp.3,Comp.4,Comp.5,drip_d18O) %>%  aggregate(. ~ loc,mean) 

sddf <- df_climate2_thick %>% mutate(loc = fct_relevel(loc, 
                                               "FOR","SNO", "GLO","GRA","PLA","SKY","MUS")) %>% 
 
  select(loc,allfluo,Comp.1,Comp.2,Comp.3,Comp.4,Comp.5,drip_d18O) %>%  aggregate(. ~ loc,sd) 

toti <- cbind(allfluo,sddf)

toti

#color palette to drop individual ones

df_climate2_thick$d18O_locvar <- NA
df_climate2_thick$drip_locvar <- NA
for (i in c("FOR","SNO", "GLO","GRA","PLA","SKY","MUS")){
df_climate2_thick$d18O_locvar[df_climate2_thick$loc==i] <- sddf %>% filter(loc==i) %>% select(drip_d18O) %>% as.numeric()
#df_climate2_thick$drip_locvar[df_climate2_thick$loc==i] <- sddf %>% filter(loc==i) %>% select(driprate) %>% as.numeric()

}
library(latex2exp) 
df_climate2_thick$C3_C1C5ratio <- df_climate2_thick$Comp.3 / (df_climate2_thick$Comp.5+df_climate2_thick$Comp.1)
p2b <- df_climate2_thick %>% mutate(loc = fct_relevel(loc, 
                                                      "FOR","SNO", "GLO","GRA","PLA","SKY","MUS")) %>%
  filter(loc %in% c("FOR","GLO","GRA","SKY","PLA")) %>%
  ggplot(aes(x=as.numeric(d18O_locvar),y=C3_C1C5ratio,colour=loc))+
  geom_boxplot() + theme_minimal(base_size = 10) +labs(x=TeX("$\\delta ^{18}O$ standard deviation"),y="C3:(C1+C5) ratio",colour="drip site") +
  scale_color_scico_d(palette="romaO",begin = 0.1,
                      end = 0.9,drop=FALSE)+ theme_classic2()

p2c <- df_climate2_thick %>% mutate(loc = fct_relevel(loc, 
                                                      "FOR","SNO", "GLO","GRA","PLA","SKY","MUS")) %>%
  filter(loc %in% c("FOR","GLO","GRA","SKY","PLA")) %>%
  ggplot(aes(x=as.numeric(d18O_locvar),y=Comp.1+Comp.5,colour=loc))+
  geom_boxplot() + theme_minimal(base_size = 10) +labs(x=TeX("$\\delta ^{18}O$ standard deviation"),y="Comp.1+Comp.5",colour="drip site") +
  scale_color_scico_d(palette="romaO",begin = 0.1,
                      end = 0.9,drop=FALSE)+ theme_classic2()


library(patchwork)

#APlot <- wrap_plots(LegendList,ncol = 2,nrow = 7)
#LegendPlot <- wrap_plots(LegendList,ncol = 2,nrow = 5)
#APlot + plot_layout(guides = "collect") 
wrap_plots(p1+theme(legend.position="none",axis.text.x=element_text(angle=60, hjust=1)),
           p2c +theme(axis.text.x=element_text(angle=60, hjust=1)),
           p2b +theme(axis.text.x=element_text(angle=60, hjust=1)),
           guides="collect") + plot_annotation(tag_levels = 'A')

ggsave(filename="04_Output/Figures_Paper/Endres_Fig5_var.tiff",width=200*1.3,height=80*1.3,units='mm',dpi=600)

###
wrap_plots(p1+theme(legend.position="none",axis.text.x=element_text(angle=60, hjust=1)),
           #p2c +theme(axis.text.x=element_text(angle=60, hjust=1)),
           p2b +theme(axis.text.x=element_text(angle=60, hjust=1)),
           guides="collect") + plot_annotation(tag_levels = 'A')

ggsave(filename="04_Output/Figures_Paper/Endres_Fig5.tiff",width=160*1.3,height=80*1.3,units='mm',dpi=600)
