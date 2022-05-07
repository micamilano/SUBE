#                Transporte tarjeta SUBE

# 1 data 21 ----
# Cargado de dataframes y librarys

rm(list = ls())
library(readr)
library(tidyverse)
library(sf)
library(dplyr)
library(lubridate)
#install.packages("showtext")      # fonts
library(showtext)
font_add_google('Bodoni Moda')
font_add_google('Roboto', 'sans-serif')
showtext_auto()


# datos_sube<- read.csv("UNTREF/datos/Transporte/tranpsorte_filtro.csv")
# 
# datos_filtro<- datos_sube %>%        # filtro
#    filter(!is.na(CANTIDAD))
# 
# datos_filtro<- datos_filtro %>%
#    mutate(tipo_transporte = datos_filtro$ï..TIPO_TRANSPORTE) %>%
#    select(DIA,CANTIDAD,tipo_transporte)

#guardo en un nuevo dataframe
write.csv(x = datos_filtro, file = "tranpsorte_filtro.csv", row.names = FALSE)
datos_filtro<- read.csv("UNTREF/datos/SUBE/tranpsorte_filtro.csv")


# 1.1 colectivos ---- 
# separo info colectivos para probar

colectivos<- datos_filtro%>% 
  filter(tipo_transporte =="Colectivo") %>% 
  mutate(fecha=dmy_hms(DIA)) %>% 
  select(tipo_transporte,fecha,CANTIDAD)

unique(datos_sube$tipo_transporte)

colectivo_mes<-colectivos %>% 
  mutate(anio=year(fecha),
         mes=month(fecha)) %>% 
  mutate(fecha_bien = paste(anio, mes,"01", sep = "/"))%>% 
  select(tipo_transporte,fecha_bien,CANTIDAD) %>% 
  mutate(fecha = as.Date.factor(fecha_bien)) %>% 
  select(tipo_transporte,fecha,CANTIDAD)


colectivo_mes<- colectivo_mes %>%      #agrupado de datos
  group_by(fecha) %>% 
  summarise(total_mes = sum(CANTIDAD))
 
colectivo_mes<-colectivo_mes %>% 
  mutate(fecha2= as.IDate(fecha)) %>% 
  select(fecha2,total_mes)


class(colectivo_mes$fecha2)

options(scipen=999)              # Sacar notacion cientifica


# grafico points 
ggplot(data = colectivo_mes , aes(x = fecha2, 
                                  y = total_mes)) +
  geom_point(color = "#84b6f4", size=4) +
  theme_minimal()+
  labs(x = "Mes", 
       y = "Cantidad",
       title = "Viajes con tarjeta SUBE",
       subtitle= "nov 2020- oct 2021",
       caption= "Fuente: Ministerio de Transporte")+
  theme_minimal()+
  theme(axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 1, size = 18, face = "italic"))+
  scale_y_continuous(limits = c(0,89000000), #escala continua
                     breaks = c(seq(0,90000000, by=8000000)),
                     labels = scales::label_number(big.mark ="."))+ #puntito de los n
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b")

# grafico barras 
# ggplot(data = colectivo_mes , aes(x = factor(fecha2), 
#                                   y = total_mes)) +
#   geom_col(fill = '#84b6f4') +
#   theme_minimal()+
#   labs(x = "Mes", 
#        y = "Cantidad",
#        title = "Viajes con tarjeta SUBE",
#        subtitle= "nov 2020- oct 2021",
#        caption= "Fuente: Ministerio de Transporte") +
#   theme(axis.title.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.text.y = element_text(size = 10),
#         axis.text.x = element_text(size = 10),
#         plot.title = element_text(hjust = 1, size = 18, face = "italic"))+
#   scale_y_continuous(limits = c(0,89000000), #escala continua
#                      breaks = c(seq(0,90000000, by=8000000)),
#                      labels = scales::label_number(big.mark ="."))


# 1.2 ---- 
# Aplico transformaciones de datos para todos los transportes

transporte<- datos_filtro%>% 
  mutate(fecha=dmy_hms(DIA)) %>% 
  select(tipo_transporte,fecha,CANTIDAD)

transporte<-transporte %>% 
  mutate(anio=year(fecha),  #separe anio y mes en columnas aparte
         mes=month(fecha)) %>% 
  mutate(fecha_bien = paste(anio, mes,"01", sep = "/"))%>% #puse a todas las fechas el dia 01 para juntar por mes
  select(tipo_transporte,fecha_bien,CANTIDAD) %>% 
  mutate(fecha = as.Date(fecha_bien)) %>% 
  select(tipo_transporte,fecha,CANTIDAD)%>% 
  group_by(tipo_transporte,fecha) %>% #agrupe por tipo de transporte y fecha
  summarise(t_mes_transp = sum(CANTIDAD[tipo_transporte=="Colectivo"|tipo_transporte=="Subte"|tipo_transporte=="Tren"]))
  #sume las cantidades segun tipo de transporte respetando las fechas

class(transporte$fecha)
unique(transporte$tipo_transporte)

# 2 tabla ---- 
# Agrupado y transformacion de la tabla para presentar 

# transporte_final<- transporte %>%
#   group_by(fecha, tipo_transporte) %>%
#   summarise(total_mes = sum(CANTIDAD[tipo_transporte=="Colectivo" |tipo_transporte=="Subte" |tipo_transporte=="Tren" ])) %>%
#   pivot_wider(names_from = tipo_transporte,
#              values_from = total_mes)
#   

# 3 grafico---- 

ggplot(data = transporte , aes(x = fecha , y=t_mes_transp, fill=tipo_transporte,)) +
  geom_col(position = "dodge")+
  #escala de colores
  scale_fill_brewer(direction = -1,
                    palette = "BuPu",
                    guide = "legend") + 
  #etiquetas
  labs(x = "Mes", 
       y = "Total",
       title = "SUBE: cantidad de transacciones",
       subtitle= "Nov 2020 - Oct 2021",
       caption= "Fuente: Dirección Nacional de Desarrollo Tecnológico - Ministerio de Transporte",
       fill="Transporte") +
  #grafico
  theme(axis.line=element_blank(),
        axis.ticks.length=unit(0.3, "lines"),
        axis.ticks.margin=unit(0.5, "lines"),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size = 12,color = "black"),
        axis.title.x = element_text(size = 12,color = "black"),
        axis.text = element_text(color = "white"),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 10, face = "bold"),
        legend.background=element_rect(fill="black", colour=NA),
        legend.key=element_rect(colour=NA),
        legend.key.size=unit(1.5, "lines"),
        legend.position="right",
        legend.text=element_text(color = "white",size=rel(1)),
        legend.title=element_text(color = "white",size=rel(1.2), face="bold", hjust=0),
        panel.background = element_rect(fill = "black"),
        panel.border=element_blank(),
        panel.grid.major=element_line(color = "#636363"),
        panel.grid.minor = element_blank(),
        panel.margin=unit(0, "lines"),
        plot.background = element_rect(fill="black"),
        plot.margin=unit(c(1, 0.5, 0.5, 0.5), "lines"),
        plot.title = element_text(hjust = 0.5, size = 18,color = "white"),
        plot.subtitle = element_text(hjust = 0.5,color = "white"),
        plot.caption = element_text(hjust = 1.5,color = "white"),
        strip.background=element_rect(fill="grey90", colour="grey50"),
        strip.text.x=element_text(size=rel(0.8)),
        strip.text.y=element_text(size=rel(0.8), angle=-90))+
  #escala y breaks
  scale_y_continuous(limits = c(0,89000000), 
                     breaks = c(seq(0,89000000, by=8000000)),
                     labels = scales::label_number(big.mark ="."))+ #puntito
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b")


  # Sacar notacion cientifica
  options(scipen=999)

  
  
  # 4 data 20/21/22 ----
  
  #4.1 ----
  # cargo datos  
  # usos2020 <-read.csv("UNTREF/datos/Transporte/Original/dat-ab-usos-2020.csv")
  # usos2021<- read.csv("UNTREF/datos/Transporte/Original/dat-ab-usos-2021.csv")  
  # usos2022<- read.csv("UNTREF/datos/Transporte/Original/dat-ab-usos-2022.csv") 
  # 
  # sube_20_21_22<- full_join(usos2020,usos2021)
  # sube_20_21_22<- full_join(sube_20_21_22,usos2022)
  # 
  # filtro
  # sube_20_21_22<- sube_20_21_22 %>%
  #   filter(!is.na(DIA_TRANSPORTE)) %>% 
  #   filter(TIPO_TRANSPORTE!="LANCHAS") %>% 
  #   select(DIA_TRANSPORTE,NOMBRE_EMPRESA,LINEA, AMBA,TIPO_TRANSPORTE,JURISDICCION,PROVINCIA,MUNICIPIO,CANTIDAD)
  #   
  # sube_20_21_22<- sube_20_21_22 %>% 
  #   filter(sube_20_21_22$CANTIDAD>=0)

  #guardo en una tabla aparte  
  #write.csv(x = sube_datos_filtro, file = "sube_datos_filtro_final.csv", row.names = FALSE)

  
#4.2 ----  
sube_datos_filtro<- read.csv("UNTREF/datos/SUBE/sube_datos_filtro_final.csv")

class(sube_datos_filtro$DIA_TRANSPORTE)
class(sube_datos_filtro$CANTIDAD)

#transformando datos de fecha
sube_datos_filtro<- sube_datos_filtro%>%
  mutate(fecha=ymd(DIA_TRANSPORTE))

class(sube_datos_filtro$fecha)
 
# #4.3 ----
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
 
# options(scipen=999)                                  # Sacar notacion cientifica


#4 data Baires ----
sube_linea_bsas<- read.csv("UNTREF/datos/SUBE/sube_datos_filtro_final.csv")

#4.1 ----
#filter 
sube_linea_bsas<- sube_linea_bsas%>%        
  select(DIA_TRANSPORTE,LINEA, TIPO_TRANSPORTE,CANTIDAD,PROVINCIA,MUNICIPIO) %>% 
  filter(PROVINCIA=="BUENOS AIRES") %>% 
  select(DIA_TRANSPORTE,LINEA,CANTIDAD,MUNICIPIO) 

#4.2 ----
#date transform
sube_linea_bsas<- sube_linea_bsas %>%       
  mutate(fecha=ymd(DIA_TRANSPORTE)) %>% 
  mutate(ANIO=year(fecha),  
         MES=month(fecha)) %>% 
  mutate(FECHA2 = paste(ANIO, MES,"01", sep = "/"))%>% 
  mutate(FECHA = as.Date(FECHA2)) %>% 
  select(FECHA,LINEA,CANTIDAD,MUNICIPIO) 

#group and summarise
sube_bsas<- sube_linea_bsas %>%            
  group_by(FECHA) %>%  
  summarise(TOTAL= sum(CANTIDAD))

#4.3 ----
#prueba para comprobar datos
 prueba<-sube_linea_bsas %>%                
   summarise(total=sum(CANTIDAD[FECHA=="2020-01-01"]))

#4.4 ----
#union de dataframe 
#transforming database to join
sube_bsas<- sube_bsas %>% 
  mutate(TIPO_TRANSPORTE= "COLECTIVO")

prueba<- sube_datos_totales %>% 
  left_join(sube_bsas,by=NULL)


# 5 Grafico ---- 
ggplot(data = prueba) +
  geom_point(x= prueba$FECHA, 
             y=prueba$TOTAL,
             color = "#fec859", 
             size=1)+
  geom_col(aes(x =FECHA , 
               y=T_MES, 
               fill=TIPO_TRANSPORTE),
           position = "dodge")+
  scale_fill_brewer(direction = -1,                #escala de colores
                    palette = "BuPu",
                    guide = "legend") + 
  labs(x = "Mes",                                  #etiquetas
       y = "Total",
       title = "SUBE: cantidad de transacciones",
       subtitle= "Ene 2020 - Ene 2022",
       caption= "\n Fuente: Dirección Nacional de Desarrollo Tecnológico - Ministerio de Transporte",
       fill="Transporte")+
  theme(axis.line=element_blank(),                #plot
        #axis.ticks.length=unit(0.5, "lines"),
        #axis.ticks.margin=unit(0.5, "lines"),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "white"),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 10, face = "bold"),
        legend.background=element_rect(fill="black", colour=NA),
        #legend.key=element_rect(colour=NA),
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
        plot.caption = element_text(hjust = 1.1,color = "white"))+
  scale_y_continuous(limits = c(0,276436319),                     #escala y breaks
                     breaks = c(seq(0,276436319, by=20000000)),
                     labels = scales::label_number(big.mark ="."))+       #puntito
  scale_x_date(date_breaks = "2 month",
                limits = as.Date(c("2019-12-15","2022-01-15")), 
                date_labels = "%b")



#6 ----
#grafico de marzo-abril
sube_marzo<- read.csv("UNTREF/datos/SUBE/sube_datos_filtro_final.csv") 

#6.1 ----
#transformando datos de fecha
sube_marzo<- sube_marzo%>%
  mutate(FECHA=ymd(DIA_TRANSPORTE))

class(sube_marzo$FECHA)

sube_marzo<-sube_marzo%>% 
select("DIA_TRANSPORTE","TIPO_TRANSPORTE","PROVINCIA","CANTIDAD","FECHA")  

sube_marzo<-sube_marzo %>%
  mutate(ANIO=year(FECHA),  
         MES=month(FECHA),
         DIA= day(FECHA)) 

#filtro año 2020 y meses 3/4/5
sube_marzo<-sube_marzo %>%
  filter(ANIO==2020) %>% 
  filter(MES=="3"|MES=="4"|MES=="5") 

#union fecha
sube_marzo<-sube_marzo %>%
  mutate(FECHA2 = paste(ANIO, MES,DIA, sep = "/"))%>% 
  mutate(FECHA = as.Date(FECHA2)) %>% 
  select("TIPO_TRANSPORTE","PROVINCIA","CANTIDAD","FECHA") 

unique(sube_marzo$TIPO_TRANSPORTE)
  
#6.2----
sube_marzo_t<-sube_marzo %>%
  group_by(TIPO_TRANSPORTE,FECHA) %>% 
  summarise(t_mes=sum(CANTIDAD[TIPO_TRANSPORTE=="COLECTIVO"| TIPO_TRANSPORTE=="TREN"|TIPO_TRANSPORTE=="SUBTE"]))
 
#7 Grafico----
ggplot(data = sube_marzo_t)+
  geom_col(aes(x =FECHA , 
               y=t_mes, 
               fill=TIPO_TRANSPORTE),
           position = "dodge")+
  scale_fill_brewer(direction = -1,                #escala de colores
                    palette = "BuPu",
                    guide = "legend") + 
  labs(x = "Fecha",                                  #etiquetas
       y = "Total",
       title= "10/03/20 - 26/04/20",
       fill="Transporte")+
  theme(axis.line=element_blank(),                #plot
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "white"),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 10, face = "bold"),
        legend.background=element_rect(fill="black", colour=NA),
        legend.key.size=unit(1, "lines"),
        legend.position="right",
        legend.text=element_text(color = "white",size=rel(1),family = "serif"),
        legend.title=element_text(color = "white",size=rel(1.2), face="bold",family = "serif", hjust=0),
        panel.background = element_rect(fill = "black"),
        panel.border=element_blank(),
        panel.grid.major=element_line(color = "#636363"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill="black"),
        plot.title = element_text(hjust = 0, size = 24,color = "white",family = "sans"),
        plot.margin=unit(c(1, 1, 1, 0.5), "lines"))+
  scale_y_continuous(limits = c(0,13182637),
                    breaks = c(seq(0,13182637, by=1000000)),
                     labels = scales::label_number(big.mark ="."))+       #puntito
  scale_x_date(date_breaks = "2 days",
               limits = as.Date(c("2020-03-12","2020-04-26")), 
               date_labels = "%e")





