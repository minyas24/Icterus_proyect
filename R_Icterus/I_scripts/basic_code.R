#install.packages("pacman")
library(pacman)
p_load("tidyverse",
       "dplyr",
       "here",
       "readr",
       "rgbif", 
       "sf", 
       "rworldxtra", 
       "usethis", 
       "CoordinateCleaner", 
       "geodata", 
       "ggspatial",
       "terra", 
       "tidyterra", 
       "paletteer", 
       "cluster",
       "spThin",
       "colorspace",
       "ggcorrplot", 
       "ggridges", 
       "plotly", 
       "ggplot2", 
       "factoextra", 
       "ggpubr", 
       "patchwork",
       "magick",
       "png",
       "grid",
       "cowplot",
       "caret")

library(tidyverse)
library(dplyr)
library(here)
library(readr)
library(rgbif) #descargar datos de ocurrencias
library(sf) #manipulación  de datos vectoriales
library(rworldxtra) #datos vectoriales de los paises del mundo
library(usethis) #credenciales
library(CoordinateCleaner) #limpieza de coordenadas
library(geodata) #datos geoespaciales complemenatarios
library(ggspatial)#auxiliar para visualizar datos espaciales
library(terra) #datos raster
library(tidyterra) #maniipulación de raster
library(paletteer) #colores
library(cluster)
library(spThin)
library(colorspace)
library(ggcorrplot) #diagrama de correlaciones
library(ggridges) #gráfico de ridges
library(plotly) #gráficos avanzados
library(ggplot2) #graficos
library(factoextra)  #estimación de clústers
library(ggpubr)  #visualización cluster
library(patchwork) #organizar gráficos
library(magick)
library(png)
library(grid)
library(cowplot)
library(caret) #correlacion de variables

#----------Generación de credenciales------------

#usethis::edit_r_environ()

#gbif_user="usuario_gbif"
#gbif_pwd="contraseña_gbif"
#gbif_email="email_gbif"

#------------Consulta ----------------

consulta_A <- name_suggest("Icterus")$data
consulta_1 <- name_backbone("Icterus auratus") 
consulta_2 <- name_backbone("Icterus dominicensis")

# Buscar la clave taxonómica del género "Bursera"
keyid <- name_backbone("Icterus auratus")$usageKey
keyid <- name_backbone("Icterus auratus")$usageKey


#keyid
#------------Descarga----------------

# asignar credenciales
usethis::edit_r_environ()
user <- Sys.getenv("gbif_user")
pwd <- Sys.getenv("gbif_pwd")
email <-Sys.getenv("gbif_email") 


# Solicitar la descarga de todas las ocurrencias georeferenciadas, sin problemas de georeferenciacion con estatus presente

descarga_gbif  <- occ_download(
  pred("taxonKey", keyid),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred("occurrenceStatus","PRESENT"),
  user = user,
  pwd = pwd,
  email = email
)

#ver el estado de la solicitud
print(descarga_gbif)
occ_download_wait('0002799-251025141854904')

#DOI1: 10.15468/dl.hzpadv
#DOI2: 10.15468/dl.hzpadv

#importar, 
df_gbif <- descarga_gbif%>%
  occ_download_get() %>% #descarga el zip
  occ_download_import() #descomprime


#para errores en zip. Forzar nueva descarga eliminando archivos temporales 
df_gbif <- descarga_gbif %>%
  occ_download_get(overwrite = TRUE) %>%
  occ_download_import(path = tempdir())
#df_gbif
#----------Limpieza coordenadas------------

#value = "flagged"

#limpia coordenadas duplicadas y ejecuta 7 test más para limpiar errores comunes en las coordenadas - lo guarda en un csv
df_cleaning <- df_gbif %>% 
  cc_equ(test = "identical", #default es "absolute"
         value = "clean")%>% 
  clean_coordinates(tests = c("capitals", "centroids", "gbif", "institutions", "outliers", "seas",
                              "zeros"),
                    value = "clean",
                    verbose = TRUE)%>% #informe de proceso
  write_csv("R_Icterus/I_data/I_10_cleaning.csv")

#------exploración y limpieza por país, base de datos e institución-----
####df_cleaning <- read_csv(here("R_Icterus/I_data/I_10_cleaning.csv"))      


unique(df_cleaning$countryCode)
unique(df_cleaning$basisOfRecord)
unique(df_cleaning$institutionCode)


#gráfico por institución
df_cleaning_grf <- df_cleaning %>% 
  ggplot(aes(x= institutionCode, fill= institutionCode))+
  geom_bar()+
  coord_flip()+
  theme(legend.position = "none")
df_cleaning_grf

#instituciones, quita NA e iNaturalist y puntos duplicados
df_clean <- df_cleaning %>%
  filter(!is.na(institutionCode), institutionCode != "iNaturalist") %>%
  distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE) %>% #elimina puntos duplicados, keep conserva todas las columnas
  select(species, decimalLatitude, decimalLongitude, month, year, gbifID) %>% #selecciona solo las columnas a usar, en el orden dado 
  write_csv("R_Icterus/I_data/I_02_clean.csv") #guarda en un csv


#df_clean
#----------Mapa inicial------------

#converte el dataframe a un archivo sf (vectorial), se definen  las columnas con las coordenadas y el CRS (Coordinate Reference System) 4326 que corresponde a WGS 84 - WGS84 - Sistema Geodésico Mundial 1984
shp_clean <- df_clean %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs= 4326)

#conservamos columnas de coordenadas separadas para el aclaramiento, quitamos colimnas duplicadas y renombramos
shp_clean <- full_join(shp_clean, df_clean, by = "gbifID")%>%
  select(-species.y, -month.y, -year.y)%>%
  rename(species = species.x,
         month = month.x,
         year = year.x)

#capa altura 
alt <- worldclim_global(var="elev", res=0.5, path=tempdir())

#cargo archivos vectoriales de la nube y asigna a un objeto
data(countriesHigh)
Mundo <- st_as_sf(countriesHigh) 

#Mapa 
map_ini <- ggplot()+
  geom_spatraster(data= alt)+
  geom_sf(data= shp_clean, aes(col = species), col="red4")+
  coord_sf(xlim = c(-120, -77), ylim = c(32, 10))+
  scale_fill_paletteer_c("grDevices::terrain.colors",
                         limits = c(0, 5000),
                         na.value = "transparent")+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering)
map_ini


map_ini_close <- ggplot()+
  geom_spatraster(data= alt)+
  geom_sf(data= shp_clean, aes(col = species), col="red4")+
  coord_sf(xlim = c(-97, -85), ylim = c(25, 15))+
  scale_fill_paletteer_c("grDevices::terrain.colors",
                         limits = c(0, 5000),
                         na.value = "transparent")

#cargar imagen y convertir en objeto ggplot 
path <- "R_Icterus/I_data/I_auratus.png"
img <- readPNG(path, native = TRUE)
cow_final <- ggdraw() +
  draw_image(image = img, x = 0.14, y =  0.24, scale = 0.25)

#mapa combinado
mapini_02 <-  map_ini/(map_ini_close | img)  + 
  plot_annotation(
    title = "Distribución de Icterus auratus",
    subtitle = "Registros de ocurrencia sobre modelo de elevación",
    caption = "Fuente: GBIF")+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering)
mapini_02

ggsave("R_icterus/I_salidas/icter_map_ini02.png", 
       plot = mapini_02,
       dpi = 300,
       width = 12,   
       height = 9,
       units = "in")
#shp_clean
#alt
#----------Extracción de variables bioclimáticas------------

#descargar datos ambientales de worldclim (variables bioclimáticas)
env <- worldclim_global(var = "bio", res = 0.5, path = "datos_wc")

#guardar rds para futuras cargas automáticas
saveRDS(env, "datos_wc/climate/wc2.1_30s/bio_enviromental.rds")
env2 <- readRDS("datos_wc/climate/wc2.1_30s/bio_enviromental.rds")

#renombrar las variables para mayor claridad
#vector vacio para loop
#i con valores del 1 al 19, llegando al 19 se detiene. En cada vuelta se pega bio_ y dos dígitos consecutivos
v_names <- vector()
for(i in 1:19){
  v_names[i] <- paste0("bio_", sprintf("%02d", i)) #sprintf(formato de 2 dígitos)
}

#asigna nombre con el vector generado
names(env) <- v_names
names(env)

#declara coordenadas a usar en la extraxxiòn
coord_elev <- shp_clean

#capa altura
puntos_sf <- st_as_sf(coord_elev, coords = c("lon", "lat"), crs = 4326)


# Extraer elevación 
elevaciones <- terra::extract(alt, puntos_sf)
elevaciones

#agrega la columna elev al shp puntos_sf
puntos_sf$elev <- elevaciones$wc2.1_30s_elev
print(puntos_sf)


#declara coordenadas a usar en la extraxxiòn
coord_bio <- shp_clean

#extraer datos en los puntos de ocurrencia de cada especie
env_icter <- terra::extract(env, coord_bio) %>% as_tibble() #extract, capa ambiental y el lugar al que queremos enviar la información
print(env_icter)

#combina objetos data.frames por columnas, une datos horizontalmente
df_icter <- cbind.data.frame(puntos_sf, env_icter)%>%
  filter(!is.na(elev))%>% #quita NA
  write_csv("R_Icterus/I_data/I_auratus_02_env.csv")

print(df_icter)

#df_icter
#-------------SpThin-------------------------------------

#
png(file = here("R_Icterus/I_salidas/I_02spthin.png"),
    width = 1000,
    height = 1000)   

plot(df_clean$decimalLongitude, df_clean$decimalLatitude,
     xlab = "Longitud", 
     ylab = "Latitud",
     main = "Aclaramiento de ocurrencias",
     sub = "Icterus auratus",
     cex.sub = 0.5,
     font.sub = 3)

thin( loc.data = df_icter,
      lat.col =  "decimalLatitude", long.col = "decimalLongitude", 
      spec.col = "species", 
      thin.par = 10, reps = 100, 
      locs.thinned.list.return = TRUE, 
      write.files = TRUE, 
      write.log.file = TRUE,
      out.dir = here("R_Icterus", "I_data", "output_I02_thin" ))




#------------------------visualizar y guardar puntos adelgazados-------------------------

#capa con datos aclarados
sthin <- read_csv(here("R_Icterus", "I_data", "output_I2_thin", "thinned_data_thin1.csv"))
points(sthin$decimalLongitude, sthin$decimalLatitude, col = "red", pch = 20)
#
dev.off()

#lectura de registros iniciales con id
inicial <- df_icter
#lectura de resultados de adelgazamiento
thinned_data_thin1 <- read_csv(here("R_Icterus", "I_data","output_I2_thin", "thinned_data_thin1.csv"))


# join usando las coordenadas y la especie como clave
icter_spthin <- thinned_data_thin1 %>% 
  left_join(inicial,
            by = c("species" = "species",
                   "decimalLongitude" = "decimalLongitude",
                   "decimalLatitude" = "decimalLatitude" ))%>%
  select(-ID)# "thinned_data_thin1" = "inicial"

#Verificar NAs para buscar errores de no coincidencia
which(is.na(icter_spthin$gbifID))


#guardar un csv
write_csv(icter_spthin, "R_Icterus/I_data/output_I2_thin/I_02_spthin.csv")



#-----------ADELGAZAMIENTO print cuántos registros se conservaron----------------

# Leer el archivos y contar filas

datos_inicial <- nrow(inicial)

datos_fin <-nrow(thinned_data_thin1)

datos_elim <- datos_inicial - datos_fin 

print(paste0("spThin/ Número inicial: ", datos_inicial, ", eliminados: ", datos_elim, ", Número final: ", datos_fin))



#icter_spthin
#------------corrplot----
#subselecciona del data, quita ID y species...
matriz_cor <- cor(subset(icter_spthin, select = -c(species, decimalLatitude, decimalLongitude, month, year, gbifID, geometry))) 

#correlograma
corr <- ggcorrplot(matriz_cor, #matriz de correlación
                   hc.order = FALSE, #si se ordena por un m?todo (HCLUST) o por default
                   type = "lower", #si se muestra toda o una secci?n
                   lab = TRUE, #si se agregan los d?gitos
                   lab_size = 3, #tama?o de los d?gitos
                   colors= c("#80FFFF", "#FFF7FF", "#FF80FF"),
                   ggtheme = theme_minimal())+
  labs(title = "Matriz de correlación para I. auratus")
corr

ggsave("R_Icterus/I_salidas/corr.png", 
       plot = corr,
       dpi = 300,
       width = 12,   
       height = 9,
       units = "in")

#matriz_cor
#-----eliminar variables correlacionadas---

# Encontrar las columnas a eliminar con un umbral de 0.7 (el predeterminado es 0.9)
# names = TRUE devuelve los nombres de las columnas en lugar de los índices
columnas_a_eliminar <- findCorrelation(matriz_cor, cutoff = 0.7, names = TRUE)
columnas_a_eliminar

icter_corr <- icter_spthin %>%
  select(-columnas_a_eliminar)%>%
  write_csv("R_Icterus/I_data/Iauratus_corr.csv")

#icter_corr
#----------Generación de kmeans------------

# Hacer el escalado
df_means <- icter_corr %>%
  select(-month, -year, -gbifID, -geometry, -decimalLongitude, -decimalLatitude) %>%
  select(where(is.numeric)) %>%
  scale() %>%
  as.data.frame()

#varianza dentro de los clusters en función del número de clusters (k). A medida que aumenta el valor de k, la varianza dentro de los clusters tiende a disminuir. La curva o “codo” indica que más allá de k = 4, agregar más clusters tiene poco valor adicional en términos de reducir la varianza dentro de los clusters.
#visualiza los resultados del agrupamiento 
agrupamiento <- fviz_nbclust(df_means, kmeans, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2)+
  labs(title = "Clústers óptimos para I. auratus",
       x = "Número de clusters k",
       y = "Varianza dentro de los clústers")

agrupamiento

ggsave("R_Icterus/I_salidas/agrupamiento.png", 
       plot = agrupamiento,
       dpi = 300,
       width = 12,   
       height = 9,
       units = "in")

#búsqueda de clústers
set.seed(123)
icter_means <- kmeans(df_means, 6, nstart = 25)



table(icter_means$cluster)

kmeans_grf <- fviz_cluster(icter_means, data = df_means,
                           palette = c("#0000FF", "#C870FF", "#74C9BC", "#7F8A8F", "#BB173A", "#C4D8F3"),
                           ggtheme = theme_minimal())
kmeans_grf

ggsave("R_Icterus/I_salidas/kmeans.png", 
       plot = kmeans_grf,
       dpi = 300,
       width = 12,   
       height = 9,
       units = "in")

# Crear data frame con todos los clusters tomando los datos iniciales(icter_spthin) y creando una la columna cluster posterior al kmeans 
icter_clusters <- icter_corr %>%
  mutate(cluster = icter_means$cluster)

# Filtrar cluster 1 y 5
datos_filtrados <- icter_clusters %>%
  filter(cluster != 1 & cluster != 5)%>%
  write_csv("R_Icterus/I_data/I_auratus_kmeans.csv")


#-----------kmeans print cuántos registros se conservaron----------------

# Leer  y contar filas

datos_inicial <- nrow(icter_corr)

datos_fin <-nrow(datos_filtrados)

datos_elim <- datos_inicial - datos_fin 

print(paste0("Kmeans/ Número inicial: ", datos_inicial, ", eliminados: ", datos_elim, ", Número final: ", datos_fin))


