library(extrafont)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(maps)
library(viridis)
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")
library(readxl)
library(tidyverse)
library(sf)
library(ggsn)
library(iNEXT)
library(vegan)
library(ade4)


##### Figura 1 ######
setwd("~/google-drive/Articulo/Maestria/bin/")

sh_ps<-st_read("../Metapoblaciones_Ghirsutum/pacifico_sur.shp")
sh_py<-st_read("../Metapoblaciones_Ghirsutum/peninsula_yucatan.shp")
sh_pc<-st_read("../Metapoblaciones_Ghirsutum/pacifico.shp")
world <- ne_countries(scale = "medium", returnclass = "sf")

points<-read_excel("../data/mapa_gh_mc.xlsx")

p1<-ggplot()+
  geom_sf(data = world, size = .1, color = "#d9d9d9",fill= "#f0f0f0") +
  geom_sf(data = sh_pc, size = .1, color = "#3288bd",fill= "#3288bd")+ 
  geom_sf(data = sh_ps, size = 1, color = "#66c2a5", fill = "#66c2a5")+ 
  geom_sf(data = sh_py, size = 0, color = "#f46d43", fill = "#f46d43")+ 
  coord_sf(xlim = c(-117, -86.12), ylim = c(13, 33), expand = FALSE) + xlab("Longitude") + 
  ylab("Latitude") +  ggtitle("a) Wild cotton associated arthropod collection sites") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.2),
        panel.background = element_rect(fill = "white"))+
  scalebar(sh_ps, dist = 250, dist_unit = "km",
           transform = TRUE, model = "WGS84", location = "bottomleft", st.size=3, height=0.02, 
           box.color = "grey", box.fill = c("white", "grey"))

p1

# ggsave("../figuras/mapa_colectas.png", dpi = 300)


ev_18_zoom <- ggplot()+
  geom_sf(data = world, size = .1, color = "#d9d9d9",fill= "#f0f0f0") +
  geom_sf(data = sh_pc, size = .1, color = "grey50",fill= "grey50")+ 
  geom_sf(data = sh_ps, size = 1, color = "grey50", fill = "grey50")+ 
  geom_sf(data = sh_py, size = 0, color = "grey50", fill = "grey50") + 
  coord_sf(xlim = c(-130, -60), ylim = c(-20, 70), expand = FALSE) + xlab("") + 
  ylab("") +  ggtitle("") +theme_minimal()+
  theme(panel.grid.major = element_line(color = "white", linetype = "dashed", size = 0.2),
        panel.background = element_rect(fill = "white"))+
  north(sh_ps, symbol = 1, location = "topright")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ev_18_zoom

library(cowplot)

mc<-ggdraw() +
  draw_plot(p1) +
  draw_plot(ev_18_zoom, x = 0.055, y = .5, width = .3, height = .2, scale = 1.7, hjust = -0.2, vjust = 1.4)
mc

ggsave("../figures/figure_1.png", dpi = 300, width = 10, height = 7)
  

### Figura 2 ##########
#setwd("~/google-drive/Articulo/Maestria/bin/")
#names(df)

df<-read.csv("../data/base_bruto_fjpl.csv") %>% 
  unite(id_muestras, c("category", "metapopulation", "site_neighborhood",
                       "site","traps", "plots","time_new"), remove = F)

X <- split(df$abundance, df$id_muestras)
#out<-iNEXT(X, q=2, datatype="abundance")
out<-iNEXT(X, q=1, datatype="abundance")
#out<-iNEXT(X, q=0, datatype="abundance")

df2<-out$AsyEst %>% separate(Site, c("category", "metapopulation", "site_neighborhood",
                                     "site","traps", "plots","time_new"), sep = "_")%>% 
  filter(Diversity == "Shannon diversity")

df4<-df %>% group_by(id_muestras) %>% 
  summarise(abundance= sum(abundance),
            Riqueza= n()) %>% mutate(Hills = df2$Observed)%>% 
  separate(id_muestras, c("category", "metapopulation", "site_neighborhood",
                          "site","traps", "plots","time_new"), sep = "_")%>%
  gather(Indicator, valor,8:10)

df4$Indicator[df4$Indicator == "Riqueza"] <- "a) Richness (p > 0.05)"
df4$Indicator[df4$Indicator == "abundance"] <- "b) Abundance (p > 0.05)"
df4$Indicator[df4$Indicator == "Hills"] <- "c) True diversity (p > 0.05)"

comp<- df4 %>% filter(plots == "C")%>%
  ggplot(aes(fill=metapopulation, y=valor, x= metapopulation)) + 
  geom_boxplot(alpha=0.5,
               # custom outliers
               outlier.colour="grey40",
               outlier.size=1) +
  xlab("")+
  facet_wrap(vars(Indicator), 
             strip.position = "top", ncol = 1,
             scales="free_y")+
  labs(y= "", x = "")+
  scale_fill_manual(values = c("#3288bd",
                               "#66c2a5",
                               "#f46d43"),name = "")+
  theme(legend.position = "top")+ theme(strip.text.x = element_text(size=8, angle=0),
                                        strip.background = element_rect(colour="#e7e1ef", fill="#e7e1ef", linetype="solid"),
                                        panel.grid.major.y = element_line(colour = "grey90", linetype="dashed",size=0.5),
                                        panel.background = element_rect(fill = "white", colour = "grey50",
                                                                        size = 2, linetype = "solid"),
                                        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                        colour = "white"), 
                                        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                                        colour = "white"))

### pcoa ###
df<-read.csv("../data/base_bruto_fjpl.csv") %>% filter(plots == "C") %>% 
  unite(id_muestras, c("metapopulation", 
                       "category", "site", "plots", "traps"), remove = F)

df7<- df %>% reshape2::dcast(id_muestras+metapopulation+category+site+plots+traps ~ otu,
                             value.var = "abundance", fun= sum)

dat_sp<-df7[,9:length(df7)] # spp
dat_env<-df7[,1:8] %>% 
  separate(id_muestras, c("Metapoblacion","Categoria", "category",
                          "var1", "site", "Sitio_1", "plots", "traps"), sep = "_")

library(vegan)
library(ade4)
pcos1<-vegdist(dat_sp, method= "bray", binary=F)
pcos1
plot(hclust(pcos1,"ward.D2"))
PCOpol<-dudi.pco(sqrt(pcos1),scannf=F, nf=3) # sqrt para ponerlo en un espacio euclidiano, el cluster no lo crea en el espacio
scatter(PCOpol)# lo ubica en espacio euclidiano

un<-qplot(data = PCOpol$li, x = A1, y = A2, colour = dat_env$Metapoblacion, alpha=.1) + 
  stat_ellipse(geom = "polygon", alpha = .3, aes(fill = dat_env$Metapoblacion))+
  theme_minimal()+ scale_colour_manual(values = c("#3288bd",
                                                  "#66c2a5",
                                                  "#f46d43"))+ 
  scale_fill_manual(values = c("#3288bd",
                               "#66c2a5",
                               "#f46d43"))+labs(x= "NMDS1", y = "NMDS2")+
  theme(
    #legend.position = "none",
    plot.title = element_text(color="black", size=10))+ ggtitle(" d) Composition")

library(ggpubr) # plor conjunto 
legend <- get_legend(comp)
blankPlot <- ggplot()+geom_blank(aes(1,1)) + cowplot::theme_nothing()
comp <- comp + theme(legend.position="none")
un<- un + theme(legend.position="none")

ggarrange(legend, blankPlot, comp, un, labels = c("", "", "", ""),
          widths = c(2.3, 3), heights = c(0.2, 2.5))

ggsave("../figures/figure_2.png",dpi = 300, height = 7.5, width = 7.3)


# Figura 3 #######
df<-read.csv("../data/base_bruto_fjpl.csv") %>% 
  unite(id_muestras, c("category", "metapopulation", "site_neighborhood",
                       "site","traps", "plots","time_new"), remove = F)

#X <- split(df$abundance, df$id_muestras)
#out<-iNEXT(X, q=2, datatype="abundance")
#out<-iNEXT(X, q=1, datatype="abundance")
#out<-iNEXT(X, q=0, datatype="abundance")

df2<-out$AsyEst %>% separate(Site, c("category", "metapopulation", "site_neighborhood",
                                     "site","traps", "plots","time_new"), sep = "_")%>% 
  filter(Diversity == "Shannon diversity")

df4<-df %>% group_by(id_muestras) %>% 
  summarise(abundance= sum(abundance),
            Riqueza= n()) %>% mutate(Hills = df2$Observed)%>% 
  separate(id_muestras, c("category", "metapopulation", "site_neighborhood",
                          "site","traps", "plots","time_new"), sep = "_")%>%
  gather(Indicator, valor,8:10)

df4$Indicator[df4$Indicator == "Riqueza"] <- "a) Richness (p < 0.05)"
df4$Indicator[df4$Indicator == "abundance"] <- "b) Abundance (p < 0.05)"
df4$Indicator[df4$Indicator == "Hills"] <- "c) True diversity (p > 0.05)"

comp<- df4 %>% filter(category %in% c("Negative", "Positive")  )%>%
  ggplot(aes(fill=category, y=valor, x= category)) + 
  geom_boxplot(alpha=0.5,
               # custom outliers
               outlier.colour="grey40",
               outlier.size=1) +
  xlab("")+
  facet_wrap(vars(Indicator), 
             strip.position = "top", ncol = 1,
             scales="free_y")+
  labs(y= "", x = "")+
  scale_fill_manual(values = c("#add58a",
                               "#fdc578"),name = "")+
  theme(legend.position = "top")+ theme(strip.text.x = element_text(size=8, angle=0),
                                        strip.background = element_rect(colour="#e7e1ef", fill="#e7e1ef", linetype="solid"),
                                        panel.grid.major.y = element_line(colour = "grey90", linetype="dashed",size=0.5),
                                        panel.background = element_rect(fill = "white", colour = "grey50",
                                                                        size = 2, linetype = "solid"),
                                        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                        colour = "white"), 
                                        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                                        colour = "white"))

comp

### pcoa ###
df<-read.csv("../data/base_bruto_fjpl.csv") %>% filter(category %in% c("Negative", "Positive")) %>% 
  unite(id_muestras, c("metapopulation", 
                       "category", "site", "plots", "traps"), remove = F)


df7<- df %>% reshape2::dcast(id_muestras+metapopulation+category+site+plots+traps ~ otu,
                             value.var = "abundance", fun= sum)

dat_sp<-df7[,9:length(df7)] # spp

dat_env<-df7[,1:8] %>% 
  separate(id_muestras, c("metapopulation", 
                          "category", "site", "plots", "traps"), sep = "_")

pcos1<-vegdist(dat_sp, method= "bray", binary=F)
plot(hclust(pcos1,"ward.D2"))
PCOpol<-dudi.pco(sqrt(pcos1),scannf=F, nf=3) # sqrt para ponerlo en un espacio euclidiano, el cluster no lo crea en el espacio
scatter(PCOpol)# lo ubica en espacio euclidiano

un<-qplot(data = PCOpol$li, x = A1, y = A2, colour = dat_env$category, alpha=.1) + 
  stat_ellipse(geom = "polygon", alpha = .3, aes(fill = dat_env$category))+
  theme_minimal()+ scale_colour_manual(values = c("#add58a",
                                                  "#fdc578"))+ 
  scale_fill_manual(values = c("#add58a",
                               "#fdc578"))+labs(x= "NMDS1", y = "NMDS2")+
  theme(
    #legend.position = "none",
    plot.title = element_text(color="black", size=10))+ ggtitle("d) Composition (p < 0.05)")
un

library(ggpubr) # plor conjunto 
legend <- get_legend(comp)
blankPlot <- ggplot()+geom_blank(aes(1,1)) + cowplot::theme_nothing()
comp <- comp + theme(legend.position="none")
un<- un + theme(legend.position="none")

ggarrange(legend, blankPlot, comp, un, labels = c("", "", "", ""),
          widths = c(2.3, 3), heights = c(0.2, 2.5))

ggsave("../figures/figure_3.png",dpi = 300, height = 7.5, width = 7.3)


######## Figura 4 #########
library(iNEXT)
setwd("~/google-drive/Articulo/Maestria/bin/")

df<-read.csv("../data/base_bruto_fjpl.csv") %>% 
  unite(id_muestras, c("category", "metapopulation", "site_neighborhood",
                       "site","traps", "plots","time_new" , "gpo_funcional"), remove = F)

X <- split(df$abundance, df$id_muestras)
#out<-iNEXT(X, q=2, datatype="abundance")
out<-iNEXT(X, q=1, datatype="abundance")
#out<-iNEXT(X, q=0, datatype="abundance")

df2<-out$AsyEst %>% separate(Site, c("category", "metapopulation", "site_neighborhood",
                                     "site","traps", "plots","time_new","gpo_funcional" ), sep = "_")%>% 
  filter(Diversity == "Shannon diversity")

df4<-df %>% group_by(id_muestras) %>% 
  summarise(abundance= sum(abundance),
            Riqueza= n()) %>% mutate(Hills = df2$Observed)%>% 
  separate(id_muestras, c("category", "metapopulation", "site_neighborhood",
                          "site","traps", "plots","time_new", "gpo_funcional"), sep = "_")%>%
  gather(Indicator, valor,9:11)

df4<-as.data.frame(df4)

df4$Indicator[df4$Indicator == "Riqueza"] <- "Richness (p < 0.05)"
df4$Indicator[df4$Indicator == "abundance"] <- "Abundance (p < 0.05)"
df4$Indicator[df4$Indicator == "Hills"] <- "True diversity (p < 0.05)"
df4$gpo_funcional[df4$gpo_funcional == "Predador"] <- "Predator"
df4$gpo_funcional[df4$gpo_funcional == "Herbivoros"] <- "Herbivorous"

comp<- df4 %>% filter(plots != "C")%>% 
  filter(gpo_funcional %in% c("Herbivorous", "Predator")) %>%
  ggplot(aes(fill= category, y=valor, x= category))+
  xlab("")+facet_wrap(~gpo_funcional+Indicator, scales = "free_y", ncol = 3) + 
  geom_boxplot(alpha=0.5,
               # custom outliers
               outlier.colour="grey40",
               outlier.size=1)+
  labs(y= "", x = "")+
  scale_fill_manual(values = c("#add58a",
                               "#fdc578"),name = "")+
  theme(legend.position = "top")+ theme(strip.text.x = element_text(size=8, angle=0),
                                        strip.background = element_rect(colour="white", fill="white", linetype="solid"),
                                        panel.grid.major.y = element_line(colour = "grey90", linetype="dashed",size=0.5),
                                        panel.background = element_rect(fill = "white", colour = "grey50",
                                                                        size = 2, linetype = "solid"),
                                        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                        colour = "white"), 
                                        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                                        colour = "white"))


comp

### pcoa ###
df<-read.csv("../data/base_bruto_fjpl.csv") %>% filter(category %in% c("Negative", "Positive")) %>% 
  unite(id_muestras, c("metapopulation", 
                       "category", "site", "plots", "traps", "gpo_funcional"), remove = F, sep = "/") %>%
  filter(gpo_funcional %in% c("Herbivorous", "Predator"))

df7<- df %>% reshape2::dcast(id_muestras+metapopulation+category+site+plots+traps+ gpo_funcional ~ otu,
                             value.var = "abundance", fun= sum)
dat_sp<-df7[,9:length(df7)] # spp
dat_env<-df7[,1:8] %>% 
  separate(id_muestras, c("metapopulation", 
                          "category", "site", "plots", "traps", "gpo_funcional"), sep = "/")
library(vegan)
library(ade4)
pcos1<-vegdist(dat_sp, method= "bray", binary=F)
pcos1
plot(hclust(pcos1,"ward.D2"))
PCOpol<-dudi.pco(sqrt(pcos1),scannf=F, nf=3) # sqrt para ponerlo en un espacio euclidiano, el cluster no lo crea en el espacio
scatter(PCOpol)# lo ubica en espacio euclidiano

un<-qplot(data = PCOpol$li, x = A1, y = A2, colour = dat_env$category, alpha=.1) + 
  stat_ellipse(geom = "polygon", alpha = .3, aes(fill = dat_env$category))+
  theme_minimal()+ scale_colour_manual(values = c("#add58a",
                                                  "#fdc578"))+ 
  scale_fill_manual(values = c("#add58a",
                               "#fdc578"))+labs(x= "NMDS1", y = "NMDS2")+
  theme(
    #legend.position = "none",
    plot.title = element_text(color="black", size=12))+ ggtitle("Composition (p < 0.05)")+
  facet_wrap(~dat_env$gpo_funcional, scales = "free")+
  theme(legend.position = "top")+ theme(strip.text.x = element_text(size=8, angle=0),
                                        strip.background = element_rect(colour="white", fill="white", linetype="solid"),
                                        panel.grid.major.y = element_line(colour = "grey90", linetype="dashed",size=0.5),
                                        panel.background = element_rect(fill = "white", colour = "grey50",
                                                                        size = 2, linetype = "solid"),
                                        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                        colour = "white"), 
                                        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                                        colour = "white"))


un

library(ggpubr) # plor conjunto 
legend <- get_legend(comp)
blankPlot <- ggplot()+geom_blank(aes(1,1)) + cowplot::theme_nothing()
comp <- comp + theme(legend.position="none")
un<- un + theme(legend.position="none")

ggarrange(legend, blankPlot, comp, un, labels = c("", "", "", ""),
          widths = c(4, 3), heights = c(0.2, 2.5))

ggsave("../figures/figure_4.png",dpi = 300, height = 6, width = 12)

### END ####
