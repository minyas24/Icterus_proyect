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
I1_abeillei <- read_csv(here("R_Icterus", "I_data", "1_I_abeillei_R.csv"))
limpio <- I1_abeillei %>% select(-fid, -gbifID, -species, -month, -year)
escalado <- scale( x = limpio) %>% as_tibble()
distancia <- dist( x = escalado, method = "euclidean")
clustering <- hclust( d= distancia, method = "average")
dendo <- as.dendrogram( clustering) 

#inicia el proceso de exportacion del plot
png(file = "C:/Users/Yas/OneDrive/Maestría/proyecto/QGis/layers/Icterus_proyect/R_Icterus/I_data/graphics/I_1dendo.png", width = 5000, height = 2000)   

dendo %>% set( what = "labels_cex", value = 0.3) %>% set( "branches_k_color", k = 1, value = c( "orange2")) %>% plot()





#------------Similitud-------
#altura máxima del dendo para calcular
dendo
#asignar un objeto con la altura máxima para la visualización y el cálculo de abline
altura_max <- as.numeric(c(28.12245))

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
clusters <-cutree(dendo, k= 21, order_clusters_as_data = FALSE) 

#Opcional -----dendo <-rect.dendrogram(dendo, k = 21)


#opcional
plot(color_branches(dendo, k=21),leaflab="none") #genera un dendograma con color con el numero K de clusters indicado





#crear SÓLO el número de clústers encontrado en abline
clusters_df <- data.frame(ID = names(clusters), cluster = clusters)
cluster1 <- filter(clusters_df, cluster == 1)$ID
cluster2 <- filter(clusters_df, cluster == 2)$ID
cluster3 <- filter(clusters_df, cluster == 3)$ID
cluster4 <- filter(clusters_df, cluster == 4)$ID
cluster5 <- filter(clusters_df, cluster == 5)$ID
cluster6 <- filter(clusters_df, cluster == 6)$ID
cluster7 <- filter(clusters_df, cluster == 7)$ID
cluster8 <- filter(clusters_df, cluster == 8)$ID
cluster9 <- filter(clusters_df, cluster == 9)$ID
cluster10 <- filter(clusters_df, cluster == 10)$ID
cluster11 <- filter(clusters_df, cluster == 11)$ID
cluster12 <- filter(clusters_df, cluster == 12)$ID
cluster13 <- filter(clusters_df, cluster == 13)$ID
cluster14 <- filter(clusters_df, cluster == 14)$ID
cluster15 <- filter(clusters_df, cluster == 15)$ID
cluster16 <- filter(clusters_df, cluster == 16)$ID
cluster17 <- filter(clusters_df, cluster == 17)$ID
cluster18 <- filter(clusters_df, cluster == 18)$ID
cluster19 <- filter(clusters_df, cluster == 19)$ID
cluster20 <- filter(clusters_df, cluster == 20)$ID
cluster21 <- filter(clusters_df, cluster == 21)$ID
cluster22 <- filter(clusters_df, cluster == 22)$ID



table(clusters)  #imprime el numero de obserbaciones que integran cada cluster

#preparación de datos para el archivo txt con filas a descartar (eliminados__dendo_I1)
cat(as.character(cluster1[1]), quote=FALSE,sep="\n") 
cat(as.character(cluster2[1]), quote=FALSE,sep="\n")
cat(as.character(cluster3[1:40]), quote=FALSE,sep="\n")
cat(as.character(cluster4[1:2]), quote=FALSE,sep="\n")
cat(as.character(cluster5[1:3]), quote=FALSE,sep="\n")
cat(as.character(cluster6[1:3]), quote=FALSE,sep="\n")
cat(as.character(cluster7[1]), quote=FALSE,sep="\n")
cat(as.character(cluster8[1:3]), quote=FALSE,sep="\n")
cat(as.character(cluster9[1:7]), quote=FALSE,sep="\n")
cat(as.character(cluster10[1]), quote=FALSE,sep="\n")
cat(as.character(cluster11[1:2]), quote=FALSE,sep="\n") 
cat(as.character(cluster12[1:3]), quote=FALSE,sep="\n")
cat(as.character(cluster13[1:9]), quote=FALSE,sep="\n")
cat(as.character(cluster14[1]), quote=FALSE,sep="\n")
cat(as.character(cluster18[1:3]), quote=FALSE,sep="\n")
cat(as.character(cluster19[1:1]), quote=FALSE,sep="\n")
cat(as.character(cluster20[1:1]), quote=FALSE,sep="\n")
cat(as.character(cluster21[1:3]), quote=FALSE,sep="\n")

#--OPCIONAL-----------------codigo para crear un dendo con la nueva base de datos-----------------------
sub.trees <- cut(dendo, h = 15)
sub.trees$lower
clusterLow <- sub.trees$lower[[2]]
clusterLow %>% set( what = "labels_cex", value = 0.6) %>% set( "branches_k_color", k = 1, value = c( "orange2")) %>% plot()
#-*******--------------------------------------




#-------------creacion de nueva base de datos con filas eliminadas en el filtro dendo-------------------------------------


I_spthin <- read_csv(here("R_Icterus", "I_data", "1_I_abeillei_R.csv"))
numeros_eliminar <- scan(here("R_Icterus", "I_data", "eliminados__dendo_I1.txt"))

I_thin <- I_spthin[-numeros_eliminar, ]
I_thin


#-----------DENDOGRAMA print cuántos registros se conservaron----------------

# Leer el archivos y contar filas

datos_ini_csv <- read_csv(here("R_Icterus", "I_data", "1_I_abeillei_R.csv"))
datos_inicial <- nrow(datos_ini_csv)

datos_txt <- read.table(here("R_Icterus", "I_data", "eliminados__dendo_I1.txt"), header = FALSE)
datos_elim <- nrow(datos_txt)

dendo_datos_final <- datos_inicial - datos_elim

print(paste0("Dendograma/ número inicial: ", datos_inicial, ", eliminados: ", datos_elim, ", Número final: ", dendo_datos_final))



#-------------SpThin-------------------------------------


png(file = "C:/Users/Yas/OneDrive/Maestría/proyecto/QGis/layers/Icterus_proyect/R_Icterus/I_data/graphics/I_1spthin.png", width = 666, height = 740)   

head("I_thin")
plot(I_thin$decimalLon, I_thin$decimalLat,
     xlab = "Longitud", 
     ylab = "Latitud",
     sub = "Icterus abeillei",
     cex.sub = 0.5,
     font.sub = 3)

thin( loc.data = I_thin,
      lat.col =  "decimalLat", long.col = "decimalLon", 
      spec.col = "species", 
      thin.par = 10, reps = 100, 
      locs.thinned.list.return = TRUE, 
      write.files = TRUE, 
      write.log.file = TRUE,
      out.dir = here("R_Icterus", "I_data", "output_I1_thin" ))





#-----------ADELGAZAMIENTO print cuántos registros se conservaron----------------

# Leer el archivos y contar filas

datos_inicial <- dendo_datos_final

datos_fin_csv <- read_csv(here("R_Icterus", "I_data", "output_I1_thin","thinned_data_thin1.csv"))
spthin_datos_final <- nrow(datos_fin_csv)

datos_elim <- datos_inicial - spthin_datos_final 

print(paste0("spThin/ Número inicial: ", datos_inicial, ", eliminados: ", datos_elim, ", Número final: ", spthin_datos_final))



#------------------------write id de observaciones conservadas después del adelazamiento---------------
#lectura de registros iniciales con id
thinned_data_thin1 <- read_csv(here("R_Icterus", "I_data","output_I1_thin", "thinned_data_thin1.csv"))
#lectura de resultados de adelgazamiento
inicial <- read_csv(here("R_Icterus", "I_data", "1_I_abeillei_R.csv"))

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
write_csv(id_spthin, "R_Icterus/I_data/output_I1_thin/id_spthin.csv")




#------------------------puntos adelgazados-------------------------

sthin <- read_csv(here("R_Icterus", "I_data", "output_I1_thin", "thinned_data_thin1.csv"))
points(sthin$decimalLon, sthin$decimalLat, col = "orange", pch = 20)
dev.off()

