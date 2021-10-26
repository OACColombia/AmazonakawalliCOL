#Código para filtrar datos de eBird

library(dplyr)
library(auk)

setwd("~/Dropbox/Amazona kawalli")
auk_set_ebd_path("/Dropbox/Amazona kawalli",overwrite=TRUE)


Amazkawa<-read_ebd("ebd_kawpar1_relMar-2021.txt")

write.csv(Amazkawa,"Amazkawa_eBird_filtrado.csv")

#Movimientos intratropicales?

library(ggplot2)
library(gganimate)
library(sf)

data<-read.csv("GBIF_eBird_anexo.csv")
data<-subset(data,Mes!="")

data1<-within(data,Estacion <- ifelse(Mes=="diciembre","Lluvias",
                                  ifelse(Mes=="enero","Lluvias",
                                  ifelse(Mes=="febrero","Lluvias",
                                  ifelse(Mes=="marzo","Lluvias",
                                  ifelse(Mes=="abril","Lluvias",
                                  ifelse(Mes=="mayo","Lluvias",
                                  ifelse(Mes=="junio","No lluvias",
                                  ifelse(Mes=="julio","No lluvias",
                                  ifelse(Mes=="agosto","No lluvias",
                                  ifelse(Mes=="septiembre","No lluvias",
                                  ifelse(Mes=="octubre","No lluvias",
                                  ifelse(Mes=="noviembre","No lluvias",NA)))))))))))))
#Change character to factor
data1<-data1%>%
  mutate_if(is.character, as.factor)
summary(data1)

data1$Mes = factor(data1$Mes, levels=c('enero','febrero','marzo',
                                      'abril','mayo','junio',
                                      'julio','agosto','septiembre',
                                      'octubre','noviembre','diciembre'))
data1$Leyenda = factor(data1$Leyenda, levels=c('Este estudio',
                                               'Espécimen físico o tejido',
                                               'Espécimen multimedia',
                                               'Observación'))

#Este estudio #d7191c
#Espécimen físico o tejido #fec980
#Espécimes multimedia #c7e9ad
#Observación #2b83ba

#para el mapa
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

anim<-ggplot()+
  geom_sf(data=world)+
  coord_sf(xlim = c(-71.5, -50), ylim = c(-15.336,3.5), expand = FALSE)+
  geom_point(data=data1,aes(x=decimalLongitude,y=decimalLatitude, colour=Leyenda),
             show.legend = FALSE, alpha = 0.7, size=4)+
  scale_color_manual(values = c("#d7191c","#fec980","#c7e9ad","#2b83ba"))+
  theme_bw()+
  labs(title="Mes de registro: {closest_state}", x="Longitud decimal",y="Latitud decimal")+
  transition_states(Mes, transition_length = 2,state_length = 1)+
  shadow_wake(wake_length = 0.1, alpha = FALSE)+
  enter_fade() +
  exit_fade()

animate(anim, fps=20, duration=15)  
anim_save("Amazona~Mes.gif",animation = last_animation())

#Contraste estacional 2 grupos

str(data1$Fecha)
library(lubridate)

data1$Fecha2=as.POSIXct(data1$Fecha,format="%d/%m/%y")
summary(data1$Fecha2)

data1$Month<-month(as.POSIXlt(data1$Fecha2,format="%d/%m/%y"))

Latitud<-ggplot(data1,aes(x=Month,y=decimalLatitude, colour=Leyenda))+
  geom_jitter(alpha=0.4)+
  scale_color_manual(values = c("#d7191c","#fec980","#c7e9ad","#2b83ba"))+
  scale_y_continuous(limits = c(-15,3))+
  scale_x_continuous(breaks = c(0:12))+
  geom_smooth(color="black", fullrange=T)+ 
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="Mes de registro",y="Latidud decimal")+
  coord_polar(start = 0.22)
  
Longitud<-ggplot(data1,aes(x=Month,y=decimalLongitude, colour=Leyenda))+
  geom_jitter(alpha=0.4)+
  scale_color_manual(values = c("#d7191c","#fec980","#c7e9ad","#2b83ba"))+
  scale_y_continuous(limits = c(-72,-51))+
  scale_x_continuous(breaks = c(1:12))+
  geom_smooth(color="black")+ 
  theme_bw()+
  theme(legend.position = "none")+
  coord_flip()+
  labs(x="Mes de registro",y="Longitud decimal")+
  coord_polar(start=0.22)

library(cowplot)
ggdraw()+
  draw_plot(Latitud, x = 0, y = 0, width = 0.5, height = 1)+
  draw_plot(Longitud, x = 0.5, y = 0, width = 0.5, height = 1)
#Guardar esta figura en buena resolución .tiff a 300 dpi 
ggsave("AmazonaB.tiff", units="in", width=6, height=3, dpi=300, compression = 'lzw')
