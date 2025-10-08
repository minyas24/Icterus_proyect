library(tidyverse)
library("pacman")
p_load("vroom", "dplyr", "dendextend")
library(readr)
library(cluster)
library(here)
library(spThin)
library(readr)
library("colorspace")
#----------Generación de dendograma------------
I2_auratus <- read_csv(here("R_Icterus", "I_data", "2_I_auratus_R.csv"))
limpio <- I2_auratus %>% select(-fid, -gbifID, -species, -month, -year)
escalado <- scale( x = limpio) %>% as_tibble()
distancia <- dist( x = escalado, method = "euclidean")
clustering <- hclust( d= distancia, method = "average")
dendo <- as.dendrogram( clustering) 

#inicia el proceso de exportacion del plot
png(file = "C:/Users/Yas/OneDrive/Maestría/proyecto/QGis/layers/Icterus_proyect/R_Icterus/I_data/graphics/I_2dendo.png", width = 10000, height = 3500)   

dendo %>% set( what = "labels_cex", value = 0.3) %>% set( "branches_k_color", k = 1, value = c( "orange2")) %>% plot()
title(sub = "Icterus auratus", cex.sub = 0.8, col.sub = "gray0",  font.sub = 3) 


#------------Similitud-------
#altura máxima del dendo para calcular
dendo
#asignar un objeto con la altura máxima para la visualización y el cálculo de abline
altura_max <- as.numeric(c(9.74722))

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
plot(color_branches(dendo, k=78),leaflab="none") #genera un dendograma con color con el numero K de clusters indicado





#crear SÓLO el número de clústers encontrado en abline
clusters_df <- data.frame(ID = names(clusters), cluster = clusters)
cluster1 <- filter(clusters_df, cluster == 1)$ID
cluster2 <- filter(clusters_df, cluster == 2)$ID
cluster3 <- filter(clusters_df, cluster == 3)$ID
cluster4 <- filter(clusters_df, cluster == 4)$ID
cluster5 <- filter(clusters_df, cluster == 5)$ID

table(clusters)  #imprime el numero de obserbaciones que integran cada cluster

#preparación de datos para el archivo txt con filas a descartar (eliminados__dendo_I1)
cat(as.character(cluster1[1:124]), quote=FALSE,sep="\n") 
cat(as.character(cluster4[1:163]), quote=FALSE,sep="\n")
cat(as.character(cluster5[1:13]), quote=FALSE,sep="\n")


#--OPCIONAL-----------------codigo para crear un dendo con la nueva base de datos-----------------------
sub.trees <- cut(dendo, h = 15)
sub.trees$lower
clusterLow <- sub.trees$lower[[2]]
clusterLow %>% set( what = "labels_cex", value = 0.6) %>% set( "branches_k_color", k = 1, value = c( "orange2")) %>% plot()
#-*******--------------------------------------




#-------------creacion de nueva base de datos con filas eliminadas en el filtro dendo-------------------------------------


I_spthin <- read_csv(here("R_Icterus", "I_data", "2_I_auratus_R.csv"))
numeros_eliminar <- scan(here("R_Icterus", "I_data", "eliminados__dendo_I2.txt"))

I_thin <- I_spthin[-numeros_eliminar, ]
I_thin


#-----------DENDOGRAMA print cuántos registros se conservaron----------------

# Leer el archivos y contar filas

datos_ini_csv <- read_csv(here("R_Icterus", "I_data", "2_I_auratus_R.csv"))
datos_inicial <- nrow(datos_ini_csv)

datos_txt <- read.table(here("R_Icterus", "I_data", "eliminados__dendo_I2.txt"), header = FALSE)
datos_elim <- nrow(datos_txt)

dendo_datos_final <- datos_inicial - datos_elim

print(paste0("Dendograma/ número inicial: ", datos_inicial, ", eliminados: ", datos_elim, ", Número final: ", dendo_datos_final))


#-------------SpThin-------------------------------------


png(file = "C:/Users/Yas/OneDrive/Maestría/proyecto/QGis/layers/Icterus_proyect/R_Icterus/I_data/graphics/I_2spthin.png", width = 666, height = 740)   

head("I_thin")
plot(I_thin$decimalLon, I_thin$decimalLat,
     xlab = "Longitud", 
     ylab = "Latitud",
     sub = "Icterus auratus",
     cex.sub = 0.5,
     font.sub = 3)

thin( loc.data = I_thin,
      lat.col =  "decimalLat", long.col = "decimalLon", 
      spec.col = "species", 
      thin.par = 10, reps = 100, 
      locs.thinned.list.return = TRUE, 
      write.files = TRUE, 
      write.log.file = TRUE,
      out.dir = here("R_Icterus", "I_data", "output_I2_thin" ))



#-----------ADELGAZAMIENTO print cuántos registros se conservaron----------------

# Leer el archivos y contar filas

datos_inicial <- dendo_datos_final

datos_fin_csv <- read_csv(here("R_Icterus", "I_data", "output_I2_thin","thinned_data_thin1.csv"))
spthin_datos_final <- nrow(datos_fin_csv)

datos_elim <- datos_inicial - spthin_datos_final 

print(paste0("spThin/ Número inicial: ", datos_inicial, ", eliminados: ", datos_elim, ", Número final: ", spthin_datos_final))



#------------------------write id de observaciones conservadas después del adelazamiento---------------
#lectura de registros iniciales con id
thinned_data_thin1 <- read_csv(here("R_Icterus", "I_data","output_I2_thin", "thinned_data_thin1.csv"))
#lectura de resultados de adelgazamiento
inicial <- read_csv(here("R_Icterus", "I_data", "2_I_auratus_R.csv"))

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
write_csv(id_spthin, "R_Icterus/I_data/output_I2_thin/id_spthin.csv")




#------------------------puntos adelgazados-------------------------

sthin <- read_csv(here("R_Icterus", "I_data", "output_I2_thin", "thinned_data_thin1.csv"))
points(sthin$decimalLon, sthin$decimalLat, col = "orange", pch = 20)
dev.off()

