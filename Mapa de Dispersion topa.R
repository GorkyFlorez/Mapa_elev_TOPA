
#  Cargamos las Librerias ----------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(ggnewscale)
library(raster)
library(extrafont)      # custom font
library(hrbrthemes)     # to use import_roboto_condensed()
library(ggthemes)
library(elevatr)
library(ggspatial)
library(tmap)
library(ggpubr)
library(ggrepel)
library(ggforce)
library(grid)
library(png)
library(ggimage)

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric <- st_transform(SurAmerica  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Peru          <- getData('GADM', country='Peru', level=0) %>% st_as_sf()
Panama        <- getData('GADM', country='Panama', level=0) %>% st_as_sf()

library(rgbif)
Ochroma_pyramidale<- occ_search(scientificName="Ochroma pyramidale")
Ochroma           <- subset(Ochroma_pyramidale$data , scientificName == "Ochroma pyramidale (Cav.) Urb.")
Ochroma$image <- "PNG/Imagen.png"

img <- readPNG("PNG/Imagen.png", FALSE)
g <- rasterGrob(img, x = unit(0.8, "npc"),y = unit(0.3, "npc"), width = unit(0.4, "npc"))


suu= st_intersection(SurAmeric, Peru)
suu <- st_union(SurAmeric, Peru)

elev = get_elev_raster(SurAmeric , z=8)
plot(elev)
Poligo_alt    <- crop(elev, suu)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, suu)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")
library(ggnewscale)


SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Peru , fill="white", color="black", size=0.01)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1, 
           label = "Atlantic ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -10, hjust = 0, vjust = 1, 
           label = "Peru",size = 3, family="serif", color = 
             "black",  fontface="italic")
SurA
SurA.grob  <- ggplotGrob(SurA)


Final=ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(0,800,1500,2500,6000),
                       na.value = 'white',
                       labels = c("[0 - 799] ","[800 - 1499]", "[1500 - 2499]",
                                  "[2500 - 2999]", "[3000 - 6000]"),
                       name='Elevacion \n(msnm)') +
  geom_sf(data = SurAmeric, fill=NA, color="black")+
  geom_sf(data = Peru , fill=NA, color="black")+
  geom_sf(data = Panama , fill="white", color="black")+
  geom_image(data = Ochroma, aes( x=decimalLongitude, y = decimalLatitude, image = image), size = 0.04)+

  geom_sf_label(data = SurAmeric, aes(label=pais), size=3)+
  geom_sf_label(data = Peru, aes(label=NAME_0), size=3)+
  geom_sf_label(data = Panama, aes(label=NAME_0), size=3)+
  annotation_custom(g)+
  coord_sf(xlim = c(-81.007, -65), ylim = c(-10,12))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x  = element_text(face="bold", color="black", size=10,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=10),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=9, family="serif"),
        legend.title = element_text(size=9, family="serif"),
        legend.key.size = unit(0.2, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.5,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "#a9def9"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Densidad \n(miles)',  x = 'Longitud', y = 'Latitud')+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -80, y = 5, hjust = 0, vjust = 1, 
           label = "Oeano \nPacifico",size = 3, family="serif", color = 
             "#03045e",  fontface="italic")+
  annotate(geom = "text", x = -78, y = 11, hjust = 0, vjust = 1, 
           label = "Oeano \nAtlantico",size = 3, family="serif", color = 
             "#03045e",  fontface="italic")+
  annotation_custom(SurA.grob, xmin = -70, xmax = -63, ymin =7, ymax=13)+
  annotate(geom = "text", x = -80, y = -10, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo      Gorky Florez Castillo  Gorky  Florez Castillo  Gorky Florez Castillo  Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)



ggsave(plot=Final,"Mapa/Mapa de dispersion3.png",units = "cm",width = 29, #alto
       height = 21, #ancho
       dpi=1200)

















































