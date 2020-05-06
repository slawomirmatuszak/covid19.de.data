library(tidyverse)
library(maptools)
library(rgeos)
library(rgdal)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(ggthemes)
library(scales)
library(ggrepel)
library(gridExtra)
library(forcats)
library(lubridate)

shp1 <- readOGR("E:/R/covid19.de.data/mapa", layer = "Niemcy")
#zmieniamy format danych
shp1f <- fortify(shp1, region = "FID")


srednia.DE <- ludnosc %>%
  ungroup()%>%
  filter(date==max(date))%>%
  summarise(
    srednia = sum(cases.cum)*100000/sum(Population)
  )%>%
  pull()
  
a <- ludnosc %>%
  filter(date==max(date))%>%
  mutate(test = if_else(zach.100>=srednia.DE, paste("powyżej średniej"), paste("poniżej średniej")))
png("niemcy1.png", units="in", width=7, height=9, res=600)
ggplot() + 
  geom_map(data=a, aes(map_id=NUTS_3.code, fill=test), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.5) + 
  coord_map(projection = "mercator") + 
  scale_fill_manual(values = c("poniżej średniej" = "forestgreen", "powyżej średniej" = "red4")) +
  labs(fill= "", title = "Poziom zakażeń w powiatach na 100 tys. mieszkańców",
       caption = "Źródło - Robert Koch-Institut") +
  #geom_label(data=a, aes(x=long, y=lat), label=a$cases, size=3) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "top",
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))
dev.off()

a <- ludnosc %>%
  filter(date==max(date))%>%
  mutate(test = if_else(zach.100>=162.12810, paste("powyżej mediany"), paste("poniżej mediany")))

png("niemcy2.png", units="in", width=7, height=9, res=600)
ggplot() + 
  geom_map(data=a, aes(map_id=NUTS_3.code, fill=test), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.5) + 
  coord_map(projection = "mercator") + 
  scale_fill_manual(values = c("poniżej mediany" = "forestgreen", "powyżej mediany" = "red4")) +
  labs(fill= "", title = "Poziom zakażeń w powiatach na 100 tys. mieszkańców",
       caption = "Źródło - Robert Koch-Institut") +
  #geom_label(data=a, aes(x=long, y=lat), label=a$cases, size=3) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "top",
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))
dev.off()

a <- ludnosc %>%
  filter(date==max(date))%>%
  mutate(test = if_else(zach.100<quantile(a$zach.100, probs = 0.25), paste("niska"),
                        if_else(zach.100>=quantile(a$zach.100, probs = 0.25)&zach.100<quantile(a$zach.100, probs = 0.5), paste("średnia"),
                                if_else(zach.100>=quantile(a$zach.100, probs = 0.5)&zach.100<quantile(a$zach.100, probs = 0.75), paste("wysoka"),
                                paste("bardzo wysoka")))))

a$test <- factor(a$test, levels = c("niska", "średnia", "wysoka", "bardzo wysoka"))

png("niemcy3.png", units="in", width=7, height=9, res=600)
ggplot() + 
  geom_map(data=a, aes(map_id=NUTS_3.code, fill=test), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.5) + 
  coord_map(projection = "mercator") + 
  scale_fill_manual(values = c("niska" = "lightgreen", "średnia" = "forestgreen", "wysoka"="red1", "bardzo wysoka"="red4")) +
  labs(fill= "", title = "Poziom zakażeń w powiatach na 100 tys. mieszkańców",
       caption = "Źródło - Robert Koch-Institut") +
  #geom_label(data=a, aes(x=long, y=lat), label=a$cases, size=3) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "top",
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))
dev.off()