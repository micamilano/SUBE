#                Transporte tarjeta SUBE

# 1 ----

# 1.1 ----
# librarys 
rm(list = ls())
library(readr)
library(tidyverse)
library(sf)
library(dplyr)
library(lubridate)
# fonts ---
#install.packages("showtext")
library(showtext)
font_add_google('Bodoni Moda')
font_add_google('Roboto', 'sans-serif')
showtext_auto()


# 1.2----
getwd()
setwd("C:/Users/micaa/OneDrive/Documentos/R/UNTREF/Script mica/SUBE")

# 2 ---- 
# Datasets
sube_datos_filtro<- read.csv("datos/sube_datos_filtro_final.csv")  

class(sube_datos_filtro$DIA_TRANSPORTE) 
class(sube_datos_filtro$CANTIDAD) 

#transformando datos de fecha
sube_datos_filtro<- sube_datos_filtro%>% 
  mutate(fecha=ymd(DIA_TRANSPORTE)) 

class(sube_datos_filtro$fecha) 

#2.3 ----
sube_datos_filtro<-sube_datos_filtro %>% 
  mutate(ANIO=year(fecha),  #separe anio y mes en columnas aparte
         MES=month(fecha)) %>% 
  mutate(FECHA2 = paste(ANIO, MES,"01", sep = "/"))%>% #puse a todas las fechas el dia 01 para juntar por mes
  mutate(FECHA = as.Date(FECHA2))

# para agrupar por mes los totales
sube_datos_totales<-sube_datos_filtro %>% 
  select(FECHA,TIPO_TRANSPORTE,CANTIDAD, PROVINCIA) %>% 
  group_by(FECHA,TIPO_TRANSPORTE) %>% 
  summarise(T_MES = sum(CANTIDAD[TIPO_TRANSPORTE=="COLECTIVO"|TIPO_TRANSPORTE=="SUBTE"|TIPO_TRANSPORTE=="TREN"]))

# Sacar notacion cientifica
options(scipen=999)

# 3 ----
#grafico 
ggplot(data = sube_datos_totales, aes(x =FECHA , y=T_MES, fill=TIPO_TRANSPORTE)) +
  geom_col(position = "dodge")+
  #escala de colores ----
scale_fill_brewer(direction = -1,
                  palette = "BuPu",
                  guide = "legend") + 
  #etiquetas ----
labs(x = "Mes", 
     y = "Total",
     title = "SUBE: cantidad de transacciones",
     subtitle= "Ene 2020 - Ene 2022",
     caption= "Fuente: Dirección Nacional de Desarrollo Tecnológico - Ministerio de Transporte",
     fill="Transporte")+
  #grafico ----
theme(axis.line=element_blank(),
      #axis.ticks.length=unit(0.5, "lines"),
      #axis.ticks.margin=unit(0.5, "lines"),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(color = "white"),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 10, face = "bold"),
      legend.background=element_rect(fill="black", colour=NA),
      legend.key=element_rect(colour=NA),
      legend.key.size=unit(1, "lines"),
      legend.position="right",
      legend.text=element_text(color = "white",size=rel(1),family = "serif"),
      legend.title=element_text(color = "white",size=rel(1.2), face="bold",family = "serif", hjust=0),
      panel.background = element_rect(fill = "black"),
      panel.border=element_blank(),
      panel.grid.major=element_line(color = "#636363"),
      panel.grid.minor = element_blank(),
      #panel.spacing = unit(5, "lines"),
      plot.background = element_rect(fill="black"),
      plot.margin=unit(c(1, 0.5, 0.5, 0.5), "lines"),
      plot.title = element_text(hjust = 0.5, size = 32,color = "white",face="bold", family = "serif"),
      plot.subtitle = element_text(hjust = 0.5,size = 16,color = "white",family = "serif"),
      plot.caption = element_text(hjust = 2,color = "white"))+
  #escala y breaks ----
scale_y_continuous(limits = c(0,276436319), 
                   breaks = c(seq(0,276436319, by=20000000)),
                   labels = scales::label_number(big.mark ="."))+ #puntito
  scale_x_date(date_breaks = "2 month",
               limits = as.Date(c("2019-12-15","2022-01-15")), 
               date_labels = "%b")


#4 ----
