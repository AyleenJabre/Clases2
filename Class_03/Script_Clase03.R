###Class 03 - Data Management & Visualization###
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez

# Desde el MINUTO 1:35:38 Empieza la clase 3.

#---- Part 1: Data Management  -------------------

# 1.- Reading an exporting data

library(readxl)
library(data.table)

casos<-data.table(read_excel("Class_02/2020-03-17-Casos-confirmados.xlsx",na = "—",trim_ws = TRUE,col_names = TRUE),stringsAsFactors = FALSE)
names(casos)

#  Los casos se pueden preguntar de dos manera para saber cómo está escrito en la base de datos 
casos[,table(Región)]
casos[,.N,by=(Región)]

casos<-casos[Región=="Metropolitana",]

saveRDS(casos,"Class_03/casosRM.rds") #Forma de guardar datos. File = Path dónde uno quiere guardar los datos 

# csv es el tipo de archivo más recomendado y es el más liviano. 

write.csv(casos,file = 'Class_03/CasosCovid_RM.csv',fileEncoding = 'UTF-8') #Esto también es para guardar la base de datos
 
install.packages("writexl")

writexl::write_xlsx(casos,path = "Class_03/CasosenExcel.xlsx") #Los dos puntos sirven para entrar dentro de otra función. Escribir una excel y tirar otro 

library(foreign)

write.dta

# FREAD - Rápida para leer cuando tenemos mucho datos, se puede seleccionar los datos que queremos 

casosRM<-fread("Class_03/CasosCovid_RM.csv",header = T, showProgress = T,data.table = T)

#---- CÓMO REEMPLAZAR DATOS ----

# Preguntamos cuantos casos de sexo hay y nos damos cuenta del terror y reemplazamos 

casosRM[,table(Sexo)]
casosRM[Sexo=="Fememino",Sexo:="Femenino"]

casosRM[`Centro de salud`=="Clínica Alemana",`Centro de salud`:="Clinica Alemana"]
casosRM[,.N,by=.(`Centro de salud`)]

# ---- CREANDO VARIABLES FACTORS (DUMMY) ----

# Creating (factor) variables, estas son de tipo string que tienen leyendas, etiquetas asociados. Por ej sexo puede ser 1 y 2.  

class(casosRM$Sexo)

casosRM[,Sexo:=factor(Sexo)] #Cambiamos el tipo de variable a factor. 

head(casosRM$Sexo) #Los niveles que tiene ahora la variable que la pasamos a factor 
head(as.numeric(casosRM$Sexo))
levels(casosRM$Sexo)

table(casosRM$Sexo)
casosRM[,.N,by=.(Sexo)] #Cuantos N hay por sexo 
casosRM[,.N,by=.(Sexo,`Centro de salud`)] #Cuantos N  hay por sexo y por centro 

# 2.- Collapsing by Centro de Salud 

#Agrupar las grandes categorias y sumar las observcaciones que no tiene. 

names(casosRM)
obj1<-casosRM[,.N,by=.(`Centro de salud`)] #creamos una variable que contiene agrupado todos los casos por clinica. (N es la cantidad de casos)

obj1[,sum(N,na.rm = T)]

obj1[,porc:=N/sum(N,na.rm = T)] #Forma de agregar nueva columna? Poporción de cosos 


# 3.- Collapsing (colapsar) by average age

A<-casosRM[,.(AvAge=mean(Edad,na.rm = T)),by=.(`Centro de salud`)] #Media de edad guardada en una nueva variable por centro de salud

B<-casosRM[,.(Total_centro=.N),by=.(`Centro de salud`)] #.N es el total de casos 

C<-casosRM[Sexo=="Femenino",.(Total_Centro_Mujeres=.N),by=.(`Centro de salud`)] #Aquí hay datos antes de la coma, qué segnifica? 

D<-casosRM[Sexo=="Masculino",.(Total_Centro_Hombres=.N),by=.(`Centro de salud`)]

dim(A)
casosRM[,.N,by=.('Centro de salud')] # Por qué no me funciona? 
dim(B)
dim(C)
dim(D)

# Me salen otras dimensiones, con uno menos. Creo que es porque yo si cambie el clinica y se hizo 1. 

# 4.-MERGING DATA SETS 

# Vincular dos bases de datos por una varible cable. 

AB<-merge(A,B,by = "Centro de salud",all = T,sort = F) #Quiero que vincule A con la base de datos B, por la columna clave centro de salud. Que se quede con todos los datos y que reorganice la base de datos eso es false. 


ABC<-merge(AB,C,by = "Centro de salud",all = T,sort = F) #Generar nueva base de datos que tengo las nuevas 3 columnas.
ABCD<-merge(ABC,D,by = "Centro de salud",all = T,sort = F)

ABCD[,porc_mujeres:=Total_Centro_Mujeres/Total_centro]

merge(A,)
?by

# 5.- Reshaping

E<-casosRM[,.(AvAge=mean(Edad,na.rm = T),`Casos confirmados`=.N),by=.(`Centro de salud`,Sexo)]

G<-reshape(E,direction = 'wide',timevar = 'Sexo',v.names = c('AvAge','Casos confirmados'),idvar = 'Centro de salud')

#---- Part 2: Visualization  -------------------

#Scatter plot
  #Base R 
plot(G$`Casos confirmados.Femenino`,G$`Casos confirmados.Masculino`)
text(x =G$`Casos confirmados.Femenino`,y=G$`Casos confirmados.Masculino`, G$`Centro de salud`,cex=0.5)

#ggplot2
p1<-ggplot(G,aes(x=`Casos confirmados.Femenino`,y=`Casos confirmados.Masculino`))+geom_point(aes(size=AvAge.Femenino,colour=AvAge.Masculino))+geom_text(aes(label=`Centro de salud`),size=2,check_overlap = T)
p1

#plotly
library(plotly)
ggplotly(p1)

# other useful ways to show data

#high charter
# http://jkunst.com/highcharter/index.html


#---- Part 3: Intro to Mapping  -------------------
#install.packages("chilemapas")
library(chilemapas)
library(data.table)
library(ggplot2)

zonas_censo<-data.table(censo_2017_zonas,stringsAsFactors = F)

poblacion_adulto_mayor_zonas<-zonas_censo[edad=="65 y mas",.(AdultosMayores=sum(poblacion)),by=.(geocodigo)]

zonas_valparaiso<-mapa_zonas[mapa_zonas$codigo_region=="05",]

zonas_valparaiso<-merge(zonas_valparaiso,codigos_territoriales[,c("codigo_comuna","nombre_comuna")],by="codigo_comuna",all.x=TRUE,sort=F)

zonas_valparaiso<-zonas_valparaiso[zonas_valparaiso$codigo_comuna%in%c("05101","05109"),]

zonas_valparaiso<-merge(zonas_valparaiso,poblacion_adulto_mayor_zonas,by="geocodigo",all.x=TRUE,sort=F)


#plotting
library(RColorBrewer)
paleta <- rev(brewer.pal(n = 9,name = "Reds"))


ggplot(zonas_valparaiso) + 
  geom_sf(aes(fill = AdultosMayores, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 años y más", subtitle = "Valparaíso y Viña del Mar") +
  theme_minimal(base_size = 11)

# creating a fake spatial distribution of adult population in space
zonas_valparaiso2<-cbind(zonas_valparaiso[,c("geocodigo","codigo_comuna","codigo_provincia","codigo_region","geometry")],"AdultosMayores"=sample(zonas_valparaiso$AdultosMayores,size = length(zonas_valparaiso$AdultosMayores)))


ggplot(zonas_valparaiso2) + 
  geom_sf(aes(fill = AdultosMayores, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 años y más", subtitle = "Valparaíso y Viña del Mar") +
  theme_minimal(base_size = 13)

#comparing histograms of the same variable

hist(zonas_valparaiso$AdultosMayores,main = "Histograma Adultos Mayores Viña-Valpo")

hist(zonas_valparaiso2$AdultosMayores,main = "Histograma Adultos Mayores Viña-Valpo")
