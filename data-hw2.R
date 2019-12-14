eu = read.csv("HW2/europe.csv")

library(ggplot2)
library(grid)
library(rworldmap)
library(ggmap)
library(mapproj)
library(maps)
library(maptools)
library(dplyr)
library(eurostat)
library(sf)
library(tmap)
library(ggplot2)
library(devtools)
library(FactoMineR)
library(factoextra)
library(plotly)

# ADAPT EUROPE DATABASE
eu[nrow(eu)+10,] = NA
eu$Country = as.character(eu$Country)
eu[29,"Country"] = "Albania"
eu[30,"Country"] = "Bosnia and Herzegovina"
eu[31,"Country"] = "Belarus"
eu[32,"Country"] = "France"
eu[33,"Country"] = "Romania"
eu[34,"Country"] = "Montenegro"
eu[35,"Country"] = "Moldova"
eu[36,"Country"] = "Macedonia"
eu[37,"Country"] = "Kosovo"
eu[38,"Country"] = "Republic of Serbia"
eu$Country = as.factor(eu$Country)

# REPRESENTING DATA THROW MAP VISUALIZATION
suppressPackageStartupMessages(library(sf))
world <- st_as_sf(rnaturalearth::countries110)
europe <- dplyr::filter(world, region_un=="Europe" & name!='Russia')

names(europe)[names(europe) == 'sovereignt'] <- 'Country'
europe = merge(europe, eu, by="Country")

# A bounding box for continental Europe.
europe.bbox <- st_polygon(list(
  matrix(c(-25,29,45,29,45,75,-25,75,-25,29),byrow = T,ncol = 2)))

europe.clipped = suppressWarnings(st_intersection(europe, st_sfc(europe.bbox, crs=st_crs(europe))))


# REPRESENTING SOME VARIABLES THROUGH MAP VISUALIZATION 
map_simple_function = function(v_input) {
  ggplot(europe.clipped, aes(fill=v_input)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") 
}
head(europe.clipped)

ggplot(europe.clipped, aes(fill=Military)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") 

ggplot(europe.clipped, aes(fill=Life.expect)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") 

ggplot(europe.clipped, aes(fill=GDP)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") 

#
#
# PCA
#
#

library(ggplot2)
library(GGally)

# Decide whether to use matrix S or R to extract the principal components
## El an??lisis de componentes principales se va a realizar con la matriz de correlaciones R, ya que las variables
## est??n expresadas en diferentes unidades y magnitudes.
library(FactoMineR)
library(factoextra)
# quali.sup --> indicamos la columna correspondiente con la variable categ??rica suplementaria
# ncp --> indicamos el n??mero de dimensiones que queremos guardar en el resultado 
eu.pca = read.csv("HW2/europe.csv")
eu.pca = PCA(eu.pca, quali.sup=1, ncp=8, scale.unit = T, graph = T)
eu.pca
par(mfrow = c(1,1))
summary(eu.pca$eig)
plot(eu.pca, cex = 0.7, shadow=T)
fviz_pca_var(eu.pca, axes = c(1,1))
fviz_pca_ind(eu.pca)

fviz_eig(eu.pca, main = "Percentage of variance explained by each dimension")
fviz_contrib(eu.pca, choice = "var", axes = 2, top = 10)




eu.pca = read.csv("HW2/europe.csv")
eu.pca = PCA(eu.pca, quali.sup=1, ncp=8, scale.unit = T, graph = F)
variance_pca = data.frame(eu.pca$eig)
variance_pca$components = rownames(variance_pca)
variance_pca$not_explained = 100-variance_pca$cumulative.percentage.of.variance
is.num = sapply(variance_pca, is.numeric)
variance_pca[is.num] = lapply(variance_pca[is.num], round, 2)
variance_pca$number_comp = c("First component", "First two components", "First three components", "First four components", "First five components", "First six components", "First seven components")


plot_ly(labels= variance_pca$components , values = variance_pca$percentage.of.variance)%>%
        add_pie(hole = 0.75) %>%
        layout(title = "",  showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p <- plot_ly(variance_pca, x = ~cumulative.percentage.of.variance[1], y = ~number_comp[1], type = 'bar', orientation = 'h',
             height= 250, width = 1000) %>%
  add_trace(x = ~not_explained[1]) %>%
  layout(barmode = 'stack',
         xaxis = list(title = ""),
         yaxis = list(title =""))
p
#
#
# KMEANS
#
#
# Cargamos el mismo archivo
eu.rows= read.csv("Desktop/developer/R/big data/HW2/europe.csv")
rownames(eu.rows) = eu.rows$Country
View(eu.rows)
eu.rows$Country = NULL
# Escalo el daraset para que las distancias entre los pa??ses en este caso cuente por igual
eu.scaled = as.data.frame(scale(eu.rows))
View(eu.scaled)
km = kmeans(eu.scaled,3)
km$cluster
c2 = data.frame(km$cluster)
colnames(c2) = c("cluster")
c2$Country = rownames(c2)
c2[nrow(c2)+10,] = NA
c2[29,"Country"] = "Albania"
c2[30,"Country"] = "Bosnia and Herzegovina"
c2[31,"Country"] = "Belarus"
c2[32,"Country"] = "France"
c2[33,"Country"] = "Romania"
c2[34,"Country"] = "Montenegro"
c2[35,"Country"] = "Moldova"
c2[36,"Country"] = "Macedonia"
c2[37,"Country"] = "Kosovo"
c2[38,"Country"] = "Republic of Serbia"
c2$Country = as.factor(c2$Country)
c2$cluster = as.factor(c2$cluster)
europe.clipped_Kmeans_map = merge(europe.clipped, c2, by = "Country")
ggplot(europe.clipped_Kmeans_map, aes(fill=cluster)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14")

# Para cada uno de los clusters podemos calcular la media para cada variable
aggregate(eu.scaled, by = list(cluster = km$cluster), mean)

# Cargamos el mismo archivo
eu.rows= read.csv("Desktop/developer/R/big data/HW2/europe.csv")
rownames(eu.rows) = eu.rows$Country
eu.rows$Country = NULL
eu.scaled = as.data.frame(scale(eu.rows))
View(eu.scaled)
#km = kmeans(eu.scaled,3)
km = kmeans(eu.scaled[,1:1],3)
fviz_cluster(km, data = eu.scaled)

# KMEANS WITH PCA
eu.a = read.csv("Desktop/developer/R/big data/HW2/europe.csv")
rownames(eu.a) = eu.a$Country
eu.a$Country = NULL
eu.pca_map = PCA(eu.a,ncp=9, scale.unit = T, graph = F)
eu.pca_map.a = data.frame(eu.pca_map$ind$coord)
set.seed(1)
km.a = kmeans(eu.pca_map.a[,1:4],3)
fviz_cluster(km.a, eu.pca_map.a)

#
#
#
# HIERARCHICAL CLUSTERING
#
#
hierarchical.cluster.ward.D2 = hclust(dist(eu.scaled, method = "euclidean"),
                                      method = "ward.D2")
plot(hierarchical.cluster.ward.D2, hang = -0.01, cex = 0.7)
rect.hclust(hierarchical.cluster.ward.D2, k=3, border = "red")
fviz_cluster(list(data = eu.scaled, cluster = cutree(hierarchical.cluster.ward.D2, 3)))

eu.pca = PCA(eu.scaled, ncp = 5)
eu.pca$ind$coord
summary(eu.pca)
HCPC(eu.pca, nb.clus = 2, iter.max = 10)

#
#
# MAP CLUSTER GRAPH
#
#
c1 = data.frame(cutree(hierarchical.cluster.ward.D2, 3))
c1[nrow(c1)+10,] = NA
colnames(c1) = c("cluster")
c1$Country = rownames(c1)
c1[29,"Country"] = "Albania"
c1[30,"Country"] = "Bosnia and Herzegovina"
c1[31,"Country"] = "Belarus"
c1[32,"Country"] = "France"
c1[33,"Country"] = "Romania"
c1[34,"Country"] = "Montenegro"
c1[35,"Country"] = "Moldova"
c1[36,"Country"] = "Macedonia"
c1[37,"Country"] = "Kosovo"
c1[38,"Country"] = "Republic of Serbia"
c1$Country = as.factor(c1$Country)
c1$cluster = as.factor(c1$cluster)
europe.clipped_HC_map = merge(europe.clipped, c1, by = "Country")
ggplot(europe.clipped_HC_map, aes(fill=cluster)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14")



tm_shape(europe.clipped) +
  tm_polygons("Military", id="Military", pallete="Greens")
  tmap_mode("view")
  tmap_last()

map_plot = tm_shape(europe.clipped) +tm_polygons("Military", id="Military", pallete="Greens") + tmap_mode("view")
map_plot

#
#
# CLUSTERING VALIDATION
#
#
library(factoextra)
library(FactoMineR)
library(cluster)
library(fpc)
library(NbClust)
eu.validation= read.csv("Desktop/developer/R/big data/HW2/europe.csv")
rownames(eu.validation) = eu.validation$Country
eu.validation$Country = NULL
# Escalo el daraset para que las distancias entre los pa??ses en este caso cuente por igual
eu.validation = as.data.frame(scale(eu.validation))
View(eu.validation)
set.seed(111)
# KMEANS
km.validation = eclust(eu.validation, FUNcluster = "kmeans", k = 3, graph = F, seed = 1)
fviz_silhouette(km.validation)
# HC
hc.validation = eclust(eu.validation, FUNcluster = "hclust", k = 3, hc_metric = "euclidean", hc_method = "ward.D2")
fviz_dend(hc.validation)
fviz_silhouette(hc.validation)
# KMEANS WITH PCA
eu = read.csv("Desktop/developer/R/big data/HW2/europe.csv")
eu.pca_map = PCA(eu, quali.sup=1, ncp=9, scale.unit = T, graph = F)
eig = data.frame(eu.pca_map$eig)
is.num = sapply(eig, is.numeric)
eig[is.num] = lapply(eig[is.num], round, 2)
View(eig)

eu = read.csv("Desktop/developer/R/big data/HW2/europe.csv")
eu.pca = PCA(eu, quali.sup=1, ncp=8, scale.unit = T, graph = F)
variance_pca = data.frame(eu.pca$eig)
is.num = sapply(variance_pca, is.numeric)
variance_pca[is.num] = lapply(variance_pca[is.num], round, 2)

eu.pca_map = data.frame(eu.pca_map$ind$coord)
km.validation = eclust(eu.pca_map[,1:1,drop=F], FUNcluster = "kmeans", k = 3, seed = 1)

fviz_cluster(km.validation, eu.pca_map )

# HC WITH PCA
eu = read.csv("Desktop/developer/R/big data/HW2/europe.csv")
eu.HC_pca = PCA(eu, quali.sup=1, ncp=9, scale.unit = T, graph = F)
eu.HC_pca = data.frame(eu.HC_pca$ind$coord)
hc.PCA_validation = eclust(eu.HC_pca, FUNcluster = "hclust", k =3, hc_metric = "euclidean", hc_method = "ward.D2", seed = 1)
fviz_dend(hc.PCA_validation, main = "")
