library(tidyverse)
library("pacman")
p_load("vroom", "dplyr", "dendextend")
library(readr)
library(cluster)
library(here)
library(spThin)
library(readr)
library("colorspace")
library(usethis) #credenciales
library(rgbif) #descargar datos de ocurrencias

library(sf) #manipulaci?n  de datos vectoriales
library(rworldxtra) #datos vectoriales de los paises del mundo
library(geodata) #datos geoespaciales complemenatarios
library(ggspatial)#auxiliar para visualizar datos espaciales
library(terra) #datos raster
library(tidyterra) #maniipulaci?n de raster
library(paletteer) #colores
library(ggcorrplot) #diagrama de correlaciones
library(ggridges) #gr?fico de ridges
library(plotly) #gr?ficos avanzados
library(patchwork) #organizar gr?ficos
library(magick) #para manejo
#----------Generación de credenciales------------

#p_load("usethis")

#usethis::edit_r_environ()
#gbif_user="usuario_gbif"
#gbif_pwd="contraseña_gbif"
#gbif_email="email_gbif"

#------------Consulta ----------------
consulta_1 <- name_backbone("Icterus auratus") 
consulta_2 <- name_suggest("Icterus")$data

# Buscar la clave taxonómica del género "Bursera"
keyid <- name_backbone("Icterus auratus")$usageKey

#keyid

#------------Descarga----------------

# asignar credenciales

usethis::edit_r_environ()
user <- Sys.getenv("gbif_user")
pwd <- Sys.getenv("gbif_pwd")
email <-Sys.getenv("gbif_email") 

# Solicitar la descarga de todas las ocurrencias georeferenciadas, sin problemas de georeferenciacion con estatus presente

#descarga_gbif  <- occ_download(
pred("taxonKey", keyid),
pred("hasCoordinate", TRUE),
pred("hasGeospatialIssue", FALSE),
pred("occurrenceStatus","PRESENT"),
user = user,
pwd = pwd,
email = email
)

#ver el estado de la solicitud
occ_download_wait('0048816-251009101135966')

#importar, 
df_gbif <- descarga_gbif%>%
  occ_download_get() %>% #descarga el zip
  occ_download_import() #descomprime

#df_gbif
#----------Limpieza------------

#install.packages("CoordinateCleaner")
library(CoordinateCleaner)

#value = "flagged"

#limpia coordenadas duplicadas y ejecuta 7 test más para limpiar errores comunes en las coordenadas - lo guarda en un csv
df_cleaning <- df_gbif %>% 
  cc_equ(test = "identical", #default es "absolute"
         value = "clean")%>% 
  clean_coordinates(tests = c("capitals", "centroids", "gbif", "institutions", "outliers", "seas",
                              "zeros"),
                    value = "clean",
                    verbose = TRUE)%>% #informe de proceso
  write_csv("R_Icterus/I_data/I_abellei_2.csv")

#exploración y limpieza por país, base de datos e institución
unique(df_cleaning$countryCode)
unique(df_cleaning$basisOfRecord)
unique(df_cleaning$institutionCode)

#gráfico por institución
df_cleaning %>% 
  ggplot(aes(x= institutionCode, fill= institutionCode))+
  geom_bar()+
  coord_flip()+
  theme(legend.position = "none")

#instituciones, quita NA e iNaturalist y puntos duplicados
df_clean <- df_cleaning %>%
  filter(!is.na(institutionCode), institutionCode != "iNaturalist") %>%
  distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE) %>% #elimina puntos duplicados, keep conserva todas las columnas
  
  select(species, decimalLatitude, decimalLongitude, month, year, gbifID) %>% #selecciona solo las columnas a usar, en el orden dado 
  write_csv("R_Icterus/I_data/I_abellei_2.csv") #guarda en un csv

#df_clean

#----------Mapa simple------------

#converte el dataframe a un archivo sf (vectorial), se definen  las columnas con las coordenadas y el CRS (Coordinate Reference System) 4326 que corresponde a WGS 84 - WGS84 - Sistema Geodésico Mundial 1984
shp_icter <- df_clean %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs= 4326)

#capa altura 
alt <- worldclim_global(var="elev", res=0.5, path=tempdir())

#cargo archivos vectoriales de la nube y asigna a un objeto

data(countriesHigh)
Mundo <- st_as_sf(countriesHigh) 

#Mapa simple 
ggplot()+
  geom_spatraster(data= alt)+
  geom_sf(data= shp_icter, aes(col = species), col="red4")+
  coord_sf(xlim = c(-120, -77), ylim = c(32, 10))+
  scale_fill_paletteer_c("grDevices::terrain.colors",
                         limits = c(0, 5000),
                         na.value = "transparent")

#----------Extracción de variables bioclimáticas------------


#descargar datos ambientales de worldclim (variables bioclimáticas)

env <- worldclim_global(var = "bio", res = 0.5, path = "datos_wc")

names(env)

#renombrar las variables para mayor claridad
#capa altura
alt <- alt %>% rename(elev_20 = wc2.1_30s_elev)

#vector vacio para loop
#i con valores del 1 al 19, llegando al 19 se detiene. En cada vuelta se pega bio_ y dos dígitos consecutivos
v_names <- vector()
for(i in 1:19){
  v_names[i] <- paste0("bio_", sprintf("%02d", i)) #sprintf(formato de 2 dígitos)
}

#asigna nombre con el vector generado
names(env) <- v_names


#extraer datos en los puntos de ocurrencia de cada especie

env_icter <- extract(env, shp_icter)  #extract, capa ambiental y el lugar al que queremos enviar la información
env_icter <- extract(alt, env_icter)  







#----------Generación de kmeans------------

df_clean <- read_csv(here("R_Icterus", "I_data", "I_abellei_2.csv")) 








#----------Generación de dendograma------------
I10_dominicensis <- read_csv(here("R_Icterus", "I_data", "10_I_dominicensis_R.csv"))
limpio <- I10_dominicensis %>% select(-fid, -gbifID, -species, -month, -year)
escalado <- scale( x = limpio) %>% as_tibble()
distancia <- dist( x = escalado, method = "euclidean")
clustering <- hclust( d= distancia, method = "average")
dendo <- as_dendrogram( clustering) 

#inicia el proceso de exportacion del plot
png(file = "C:/Users/Yas/OneDrive/Maestría/proyecto/QGis/layers/Icterus_proyect/R_Icterus/I_data/graphics/I_10dendo.png", width = 5000, height = 2000)   

dendo %>% set( what = "labels_cex", value = 0.3) %>% set( "branches_k_color", k = 1, value = c( "orange2")) %>% plot()





#------------Similitud-------
#altura máxima del dendo para calcular
dendo
#asignar un objeto con la altura máxima para la visualización y el cálculo de abline
altura_max <- as.numeric(c(32.45111))

#Etiqueta del eje X para mejor visualización--------------

#Superponer un nuevo eje en porcentaje en el lado derecho
axis(side = 4,                                # 4 = lado derecho del gráfico
     at = seq(0, altura_max, length.out = 11), # Posiciones en el eje original
     labels = paste0(seq(100, 0, by = -10), "%")) # Etiquetas en porcentaje


#temina el proceso de exportacion del plot
dev.off()   


#calcular el 75%
h <- ((25*altura_max)/100)
h
dendo <- abline(h= h) 





#---------Poda del dendograma (separación de clústers)--------

#decidir en base a abline el valor k (numero de clusters)
dendo <- as.dendrogram( clustering)
clusters <-cutree(dendo, k= 5, order_clusters_as_data = FALSE) 

#Opcional -----dendo <-rect.dendrogram(dendo, k = 21)


#opcional
plot(color_branches(dendo, k=5),leaflab="none") #genera un dendograma con color con el numero K de clusters indicado





#crear SÓLO el número de clústers encontrado en abline
clusters_df <- data.frame(ID = names(clusters), cluster = clusters)
cluster1 <- filter(clusters_df, cluster == 1)$ID
cluster2 <- filter(clusters_df, cluster == 2)$ID
cluster3 <- filter(clusters_df, cluster == 3)$ID
cluster4 <- filter(clusters_df, cluster == 4)$ID
cluster5 <- filter(clusters_df, cluster == 5)$ID



table(clusters)  #imprime el numero de obserbaciones que integran cada cluster

#preparación de datos para el archivo txt con filas a descartar (eliminados__dendo_I1)
cat(as.character(cluster1[1]), quote=FALSE,sep="\n") 
cat(as.character(cluster2[1]), quote=FALSE,sep="\n")
cat(as.character(cluster3[1:17]), quote=FALSE,sep="\n")
cat(as.character(cluster4[1:33]), quote=FALSE,sep="\n")

#--OPCIONAL-----------------codigo para crear un dendo con la nueva base de datos-----------------------
sub.trees <- cut(dendo, h = 15)
sub.trees$lower
clusterLow <- sub.trees$lower[[2]]
clusterLow %>% set( what = "labels_cex", value = 0.6) %>% set( "branches_k_color", k = 1, value = c( "orange2")) %>% plot()
#-*******--------------------------------------




#-------------creacion de nueva base de datos con filas eliminadas en el filtro dendo-------------------------------------


I_spthin <- read_csv(here("R_Icterus", "I_data", "10_I_dominicensis_R.csv"))
numeros_eliminar <- scan(here("R_Icterus", "I_data", "eliminados__dendo_I10.txt"))

I_thin <- I_spthin[-numeros_eliminar, ]
I_thin


#-----------DENDOGRAMA print cuántos registros se conservaron----------------

# Leer el archivos y contar filas

datos_ini_csv <- read_csv(here("R_Icterus", "I_data", "10_I_dominicensis_R.csv"))
datos_inicial <- nrow(datos_ini_csv)

datos_txt <- read.table(here("R_Icterus", "I_data", "eliminados__dendo_I10.txt"), header = FALSE)
datos_elim <- nrow(datos_txt)

dendo_datos_final <- datos_inicial - datos_elim

print(paste0("Dendograma/ número inicial: ", datos_inicial, ", eliminados: ", datos_elim, ", Número final: ", dendo_datos_final))



#-------------SpThin-------------------------------------


png(file = "C:/Users/Yas/OneDrive/Maestría/proyecto/QGis/layers/Icterus_proyect/R_Icterus/I_data/graphics/I_10spthin.png", width = 666, height = 740)   

head("I_thin")
plot(I_thin$decimalLon, I_thin$decimalLat,
     xlab = "Longitud", 
     ylab = "Latitud",
     sub = "Icterus dominicensis",
     cex.sub = 0.5,
     font.sub = 3)

thin( loc.data = I_thin,
      lat.col =  "decimalLat", long.col = "decimalLon", 
      spec.col = "species", 
      thin.par = 10, reps = 100, 
      locs.thinned.list.return = TRUE, 
      write.files = TRUE, 
      write.log.file = TRUE,
      out.dir = here("R_Icterus", "I_data", "output_I10_thin" ))





#-----------ADELGAZAMIENTO print cuántos registros se conservaron----------------

# Leer el archivos y contar filas

datos_inicial <- dendo_datos_final

datos_fin_csv <- read_csv(here("R_Icterus", "I_data", "output_I10_thin","thinned_data_thin1.csv"))
spthin_datos_final <- nrow(datos_fin_csv)

datos_elim <- datos_inicial - spthin_datos_final 

print(paste0("spThin/ Número inicial: ", datos_inicial, ", eliminados: ", datos_elim, ", Número final: ", spthin_datos_final))



#------------------------write id de observaciones conservadas después del adelazamiento---------------
#lectura de registros iniciales con id
thinned_data_thin1 <- read_csv(here("R_Icterus", "I_data","output_I10_thin", "thinned_data_thin1.csv"))
#lectura de resultados de adelgazamiento
inicial <- read_csv(here("R_Icterus", "I_data", "10_I_dominicensis_R.csv"))

# join usando las coordenadas y la especie como clave
id_spthin <- thinned_data_thin1 %>% 
  left_join(inicial,
            by = c("species" = "species",
                   "decimalLon" = "decimalLon",
                   "decimalLat" = "decimalLat" )) # "thinned_data_thin1" = "inicial"

#Verificar NAs para buscar errores de no coincidencia
which(is.na(id_spthin$fid))


#arreglar formato
colnames(id_spthin)[2] <- "decimalLat"
colnames(id_spthin)[3] <- "decimalLon"
id_spthin <- id_spthin %>% relocate(fid)

#guardar un csv
write_csv(id_spthin, "R_Icterus/I_data/output_I10_thin/id_spthin.csv")




#------------------------puntos adelgazados-------------------------

sthin <- read_csv(here("R_Icterus", "I_data", "output_I10_thin", "thinned_data_thin1.csv"))
points(sthin$decimalLon, sthin$decimalLat, col = "orange", pch = 20)
dev.off()

