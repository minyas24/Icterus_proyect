#-------------Librerias---------
#install.packages("pacman")
library(pacman)
p_load("tidyverse",
       "dplyr",
       "here",
       "readr",
       "rgbif",              #descargar datos de ocurrencias
       "sf",                 #manipulación  de datos vectoriales
       "rworldxtra",         #datos vectoriales de los paises del mundo
       "usethis",            #credenciales  
       "CoordinateCleaner",  #limpieza de coordenadas
       "geodata",            #datos geoespaciales complemenatarios
       "ggspatial",          #auxiliar para visualizar datos espaciales
       "terra",              #datos raster  
       "tidyterra",          #maniipulación de raster
       "paletteer", 
       "cluster",
       "spThin",
       "colorspace",
       "ggcorrplot",         #diagrama de correlaciones 
       "ggplot2",            #graficos  
       "factoextra",         #estimación de clústers
       "ggpubr",             #visualización cluster
       "patchwork",
       "magick",
       "png",
       "grid",
       "cowplot",            #combinar gráficos de ggplot2
       "caret",              #correlacion de variables
       "RColorBrewer")          

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
keyid <- name_backbone("Icterus dominicensis")$usageKey


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
occ_download_wait('0002799-251025141854904')
print(descarga_gbif)


#DOI1: 10.15468/dl.hzpadv
#DOI2: 10.15468/dl.hzpadv

#importar y descomprimir
#df_gbif2 <- descarga_gbif%>%
#  occ_download_get() %>%
#  occ_download_import()

#para errores en zip, forzar nueva descarga
#df_gbif <- descarga_gbif %>%
#  occ_download_get(overwrite = TRUE) %>% #sobreescribe
#  occ_download_import(path = tempdir())

#para acceder a descargas previas
df_gbif <- occ_download_get('0002799-251025141854904') %>%
  occ_download_import()

#df_gbif
#----------Limpieza coordenadas------------

#value = "flagged"

#limpia coordenadas duplicadas y ejecuta 7 test más para limpiar errores comunes en las coordenadas - lo guarda en un csv
df_cleaning <- df_gbif %>% 
  cc_equ(test = "identical", #"absolute"
         value = "clean")%>% 
  clean_coordinates(tests = c("capitals",
                              "centroids", 
                              "institutions", 
                              "seas",
                              "zeros"),
                    value = "clean",
                    verbose = TRUE)%>% #informe de proceso
  write_csv("R_Icterus/I_data/I_10_clean.csv")

#------exploración y limpieza por base de datos e institución-----
####df_cleaning <- read_csv(here("R_Icterus/I_data/I_10_clean.csv"))      

unique(df_cleaning$countryCode)
unique(df_cleaning$basisOfRecord)
unique(df_cleaning$institutionCode)


#gráfico por institución
df_cleaning_grf <- df_cleaning %>% 
  ggplot(aes(x = institutionCode, 
             fill = institutionCode))+
  geom_bar()+
  coord_flip()+
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(title = "Registros por Institucion",
       x = "Institucion",
       y = "Numero de Registros")
df_cleaning_grf

#instituciones, quita NA y puntos duplicados
df_clean <- df_cleaning %>%
  filter(!is.na(institutionCode)) %>%
  distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE) %>% #elimina puntos duplicados, keep conserva todas las columnas
  select(gbifID, species, decimalLatitude, decimalLongitude, month, year) %>% #selecciona en el orden dado 
  write_csv("R_Icterus/I_data/I_10_clean.csv")

#df_clean
#----------Mapa inicial------------

#converte el dataframe a un archivo sf (vectorial), se definen  las columnas con las coordenadas y el CRS (Coordinate Reference System) 4326 que corresponde a WGS 84 - WGS84 - Sistema Geodésico Mundial 1984
shp_clean <- df_clean %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs= 4326)

#conservamos columnas de coordenadas separadas para el aclaramiento, quitamos columnas duplicadas y renombramos
shp_clean <- full_join(shp_clean, df_clean, by = "gbifID")%>%
  select(!any_of(c("species.y", "month.y", "year.y")))%>%
  rename(species = species.x,
         month = month.x,
         year = year.x)

#capa altura 
alt <- worldclim_global(var="elev", res=0.5, path=tempdir())

#cargo archivos vectoriales de la nube y asigna a un objeto
data(countriesHigh)
Mundo <- st_as_sf(countriesHigh) 

#Mapa 
#mapa México
map_ini <- ggplot() +
  geom_spatraster(data = alt) +
  geom_sf(data = shp_clean, aes(col = species), col = "#F5191C") +
  scale_fill_paletteer_c("grDevices::terrain.colors",
                         limits = c(0, 5000),
                         na.value = "transparent",
                         guide = "none")+
  coord_sf(xlim = c(-120, -77), ylim = c(32, 10)) +
  annotation_scale(
    location = "bl",
    width_hint = 0.4,
    unit_category = "metric",
    bar_cols = c("#B5B5B5", "#EBEBEB"),
    text_col = "#B5B5B5",
    line_col = "#F8F5F2")+
  theme_minimal()+
  theme(axis.text = element_text(size = 5))

map_ini

#acercamiento
map_ini_close <- ggplot()+
  geom_spatraster(data= alt)+
  geom_sf(data= shp_clean, aes(col = species), col="#F5191C")+
  scale_fill_paletteer_c("grDevices::terrain.colors",
                         limits = c(0, 5000),
                         na.value = "transparent",
                         name = "Elevación (m)", 
                         breaks = seq(0, 5000, by = 1000))+
  coord_sf(xlim = c(-97, -85), ylim = c(25, 15))+
  annotation_north_arrow(
    location = "tr",  
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("#F8F5F2", "#EBEBEB"),
      line_col = "#B5B5B5"),
    height = unit(1.2, "cm"),
    width = unit(1.2, "cm")) +
  annotation_scale(
    location = "bl",
    width_hint = 0.4,
    unit_category = "metric",
    bar_cols = c("#B5B5B5", "#EBEBEB"),
    text_col = "#B5B5B5",
    line_col = "#F8F5F2")+
  theme_minimal()
map_ini_close

#cargar imagen y convertir en objeto ggplot 
path <- "R_Icterus/I_data/I_dominicensis.jpeg"
img <- readJPEG(path, native = TRUE)
cow_final <- ggdraw() + 
  draw_image(image = img,
             x = 0.1, y = 0.1,
             scale = 1)
cow_final

#mapa combinado
map_dist <- map_ini_close / ( cow_final | map_ini )  + 
  plot_annotation(
    title = "Distribucion de Icterus auratus",
    subtitle = "Registros de ocurrencia sobre modelo de elevacion",
    caption = "Fuente: GBIF")
map_dist

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
#env <- worldclim_global(var = "bio", res = 0.5, path = "datos_wc")

#guardar rds para futuras cargas automáticas
#saveRDS(env, "datos_wc/climate/wc2.1_30s/bio_enviromental.rds")
#env <- readRDS("datos_wc/climate/wc2.1_30s/bio_enviromental.rds")

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

#declara coordenadas a usar en la extracciòn
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
env_icter <- terra::extract(env, coord_bio) #extract, capa ambiental y el lugar al que queremos enviar la información
print(env_icter)

#combina objetos data.frames por columnas, une datos horizontalmente
df_icter <- cbind.data.frame(puntos_sf, env_icter)%>%
  filter(!is.na(elev))%>% #quita NA
  select(-ID)%>%
  write_csv("R_Icterus/I_data/I_10_clean.csv")

print(df_icter)

#df_icter
#-------------SpThin-------------------------------------
##*************
df_icter <- read.csv("R_Icterus/I_data/I_10_clean.csv")


#visualización de ocurrencias
ocurrencias <- ggplot(df_icter, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_point() +
  labs(x = "Longitud",
       y = "Latitud", 
       title = "Ocurrencias") +
  theme_minimal()
ocurrencias

#adelgazamiento
set.seed(123)
thin( loc.data = df_icter,
      lat.col =  "decimalLatitude", long.col = "decimalLongitude", 
      spec.col = "species", 
      thin.par = 10, reps = 100, 
      locs.thinned.list.return = FALSE, 
      write.files = TRUE, 
      write.log.file = FALSE,
      out.dir = "R_Icterus/I_data",
      out.base = "I_10")

#visualizar y guardar puntos adelgazados
map_thin <- read_csv("R_Icterus/I_data/I_10_thin1.csv") %>%
  ggplot(aes(decimalLongitude, decimalLatitude)) +
  geom_point(color = "red") +
  labs(x = "Longitud",
       y = "Latitud", 
       title = "Aclaramiento") +
  theme_minimal()
map_thin

map_spthin <- ocurrencias + map_thin +
  plot_annotation(title = "Distribucion de Icterus dominicensis")

map_spthin

ggsave("R_icterus/I_salidas/mapthin_10.png",
       plot = map_spthin,
       dpi = 300,
       width = 15,
       height = 7,
       units = "in")




#recuperación de id

#lectura de registros iniciales con id
inicial <- df_icter
#lectura de resultados de adelgazamiento
thin1 <-  read_csv("R_Icterus/I_data/I_10_thin1.csv")

# join usando las coordenadas y la especie como clave, "thin1" = "inicial"
icter_spthin <- thin1 %>% 
  left_join(inicial,
            by = c("species" = "species",
                   "decimalLongitude" = "decimalLongitude",
                   "decimalLatitude" = "decimalLatitude" ))%>%
  write_csv("R_Icterus/I_data/I_10_clean.csv")

which(is.na(icter_spthin$gbifID))  #Verificar NAs para buscar errores de no coincidencia
  

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
corr <- ggcorrplot(matriz_cor, #matriz de correlacion
                   hc.order = FALSE, 
                   type = "lower", 
                   lab = TRUE, 
                   lab_size = 2.5, 
                   colors= c("#80FFFF", "#FFF7FF", "#FF80FF"),
                   ggtheme = theme_minimal())+
  labs(title = "Matriz de correlacion para Icterus dominicensis")
corr

ggsave("R_Icterus/I_salidas/corr_10.png", 
       plot = corr,
       dpi = 300,
       width = 12,   
       height = 9,
       units = "in")



#matriz_cor

#-----eliminar variables correlacionadas------

# Encontrar las columnas a eliminar con un umbral de 0.7 (el predeterminado es 0.9)
# names = TRUE devuelve los nombres de las columnas en lugar de los índices
var_elim <- findCorrelation(matriz_cor, cutoff = 0.7, names = TRUE)
var_elim

icter_corr <- icter_spthin %>%
  select(-var_elim)%>%
  write_csv("R_Icterus/I_data/I_10_corr.csv")

#icter_corr
#----------Generación de kmeans------------


# Hacer el escalado
df_kmeans <- icter_corr %>%
  select(-month, -year, -gbifID, -geometry, -decimalLongitude, -decimalLatitude) %>%
  select(where(is.numeric)) %>%
  scale() %>%
  as.data.frame()

#varianza dentro de los clusters en función del número de clusters (k). A medida que aumenta el valor de k, la varianza dentro de los clusters tiende a disminuir. La curva o “codo” indica que más allá de k = 4, agregar más clusters tiene poco valor adicional en términos de reducir la varianza dentro de los clusters.
#visualiza los resultados del agrupamiento 
agrupamiento <- fviz_nbclust(df_kmeans, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(title = "Clusters optimos para I. dominicensis",
       x = "Numero de clusters k",
       y = "Varianza dentro de los clusters")

agrupamiento

ggsave("R_Icterus/I_salidas/I_10_grup.png", 
       plot = agrupamiento,
       dpi = 300,
       width = 12,   
       height = 9,
       units = "in")

#búsqueda de clústers
set.seed(123)
icter_kmeans <- kmeans(df_kmeans, 5, nstart = 25)



table(icter_kmeans$cluster)

kmeans_grf <- fviz_cluster(icter_kmeans, data = df_kmeans,
                           palette = c("#0000FF", "#C870FF", "#74C9BC", "#7F8A8F", "#BB173A", "#C4D8F3", "#D33682","#8AD6E4"),
                           ggtheme = theme_minimal())+
  labs(title = "Analisis de Clusters",
       subtitle = "Icterus dominicensis")
kmeans_grf

ggsave("R_Icterus/I_salidas/I_10_kmeans.png", 
       plot = kmeans_grf,
       dpi = 300,
       width = 12,   
       height = 9,
       units = "in")

# Crear data frame con todos los clusters tomando los datos iniciales(icter_corr) y creando una la columna cluster posterior al kmeans y Filtrar cluster 1 
icter_clusters <- icter_corr %>%
  mutate(cluster = icter_kmeans$cluster)%>%
  filter(cluster != 1 ) %>%
  write_csv("R_Icterus/I_data/I_10_kmeans.csv")
icter_clusters



#-----------kmeans print cuántos registros se conservaron----------------

# Leer  y contar filas

datos_ini <- nrow(icter_clusters)

datos_fin <-nrow(datos_filtrados)

datos_elim <- datos_ini - datos_fin 

print(paste0("Kmeans/ Número inicial: ", datos_ini, ", eliminados: ", datos_elim, ", Número final: ", datos_fin))


