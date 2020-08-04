

#### Tabla resumen por orden ####

setwd("~/google-drive/Articulo/Maestria/bin/")

df<-read.csv("../data/base_bruto_fjpl.csv") %>% 
  unite(id_muestras, c("category", "metapopulation", "site_neighborhood",
                       "site","traps", "plots","time_new"), remove = F)

names(df)


al<- df %>% filter(category != "Neighborhood") %>% group_by(category, Orden) %>% 
  summarise(abundance= sum(abundance),
            Riqueza= unique(otu),
            Total= abundance+Riqueza) %>%
  reshape2::dcast(Orden ~ category, value.var = "Riqueza") %>%
  arrange(desc(Negative)) %>% mutate( Proporcion = c(Positive/Negative )-1)


al %>% gather(category , valor, 2:3) %>% 
  filter(Orden %in% c("Araneae", "Hymenoptera", "Coleoptera", "Hemiptera", "Lepidoptera", "Diptera", "Orthoptera")) %>%
  ggplot(aes(x= reorder(Orden, Proporcion), y=Proporcion, label=Proporcion)) + 
  geom_point(stat='identity', fill="black", size=6, color="#00AFBB", alpha= .6)  +
  geom_segment(aes(y = 0, 
                   x = Orden, 
                   yend = Proporcion, 
                   xend = Orden), 
               color = "grey") +
  labs(title="Riqueza de morfoespecies en algodón con transgenes")  +
  coord_flip()+ scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  theme(legend.position = "top")+ theme(strip.text.x = element_text(size=8, angle=0),
                                        strip.background = element_rect(colour="white", fill="white", linetype="solid"),
                                        panel.grid.major.y = element_line(colour = "grey90", linetype="dashed",size=0.5),
                                        panel.background = element_rect(fill = "white", colour = "grey50",
                                                                        size = 2, linetype = "solid"),
                                        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                        colour = "white"), 
                                        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                                        colour = "white"))+ 
  geom_hline(yintercept=0, color = "grey80")+xlab("")+
  geom_segment(aes(x=0, y=0, xend=7, yend=0), colour="blue")+
  geom_segment(aes(x=0, y=0, xend=4, yend=0), colour="#E69F00")



##### los disminuidos ####

names(df)

df<-read.csv("../data/base_bruto_fjpl.csv") %>% 
  unite(id_muestras, c("category", "metapopulation", "site_neighborhood",
                       "site","traps", "plots","time_new"), remove = F)


names(df)
levels(as.factor(df$plots))

df %>% filter(plots %in% c( "A", "B")) %>% group_by(category, Orden) %>% summarise(Total= sum(abundance)) %>% 
  reshape2::dcast(Orden ~ category, value.var = "Total", fun= sum)%>% 
  mutate(Total= Negative+Positive)%>%
  mutate(freNeg= Negative*100/Total,
         frePos= Positive*100/Total) %>% 
  arrange(desc(freNeg))


df %>% group_by(Orden) %>% summarise(Total= sum(abundance)) %>% arrange(desc(Total))

names(bsa)

levels(as.factor(df$category))

co_neg<-df %>% filter(Orden == "Coleoptera"  & category == "Negative" & plots != "C")
co_pos<-df %>% filter(Orden == "Coleoptera"  & category == "Positive" & plots != "C")
co_ves<-df %>% filter(Orden == "Coleoptera"  & category == "Neighborhood")

#riqueza#
n_distinct(co_neg$otu) # Num. de spp distintas
n_distinct(co_pos$otu)  # Num. de spp distintas
n_distinct(co_ves$otu)  # Num. de spp distintas

(93*100/82)-100 # REDUCCI?N DE 57.14 % EN LA RIQUEZA DE otu DE Coleopteros EN ALGODON INTROGRESADO

#abundance#
sum(co_neg$abundance)
sum(co_pos$abundance)
sum(co_ves$abundance)

(476*100/338)-100 # AUMENTO DE 4.28 % EN LA abundance DE otu DE Ort?pteros EN ALGOD?N INTROGRESADO






