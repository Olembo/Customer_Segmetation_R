customer_data = read.csv("C:/Users/Richmond Réel OLEMBO/Desktop/Customer_Segmetation_R/Mall_Customers.csv")
str(customer_data)

names(customer_data)

#Afficher les 5 premieres lignes du dataset
head(customer_data)

# Voir les statistiques de la colonne Age
summary(customer_data$Age)

# Applique la fonction sd pour trouver l'ecart type de la colonne Age
sd(customer_data$Age)

# Voir les statistiques de la colonne Annual.Income..k..
summary(customer_data$Annual.Income..k..)

# Applique la fonction sd pour trouver l'ecart type de la colonne Annual.Income..k..
sd(customer_data$Annual.Income..k..)

# Applique la fonction sd pour trouver l'ecart type de la colonne spending.Score..1.100
sd(customer_data$Spending.Score..1.100.)

################### Visualisation Du genre du Client###########################
a = table(customer_data$Genre)
#Utiliser Barplot
barplot(a, main = "comparaison du Genre du Client", 
        ylab = "Compter",
        xlab = "Genre",
        col = rainbow(2),
        legend = rownames(a))

pct = round(a/sum(a)*100)
lbs = paste(c("Female", "Male"), " ", pct, "%", sep = " ")
library(plotrix)
pie3D(a, labels=lbs,
      main= "Diagramme circulaire (Pie chart) representant le ratio de Femmes et d'hommes")

summary(customer_data$Age)

# Histogramme des Ages
hist(customer_data$Age,
     col = "blue",
     main = "Histogramme pour montrer le nombre de classes d'Age",
     xlab = "Classe d'Age",
     ylab = "La frequence",
     labels = TRUE)

boxplot(customer_data$Age,
        col = "#ff0066",
        main = "Boxplot pour l'analyse descriptive des Ages")

# Analyse du revenu annuel de clients

summary(customer_data$Annual.Income..k..)

hist(customer_data$Annual.Income..k..,
     col = "#660033",
     main = "Histogramme des revenus annuels",
     xlab = "Classe des revenus annuel",
     ylab = "La frequence",
     labels = TRUE)
plot(density(customer_data$Annual.Income..k..),
     col= "yellow",
     main = "Diagramme de densite pour le revenu annuel",
     xlab = "classe de revenu",
     ylab = "Densite")
polygon(density(customer_data$Annual.Income..k..),
        col = "#ccff66")

#Analyse du score de depenses des clients
summary(customer_data$Spending.Score..1.100.)

boxplot(customer_data$Spending.Score..1.100.,
        horizontal = TRUE,
        col = "#990000",
        main = "Boxplot pour l'analyse descriptive du score de depenses")
hist(customer_data$Spending.Score..1.100.,
     main = "Histogramme du score de depenses",
     xlab = "Classe du score de depenses",
     ylab = "Frequence",
     col = "#6600cc",
     labels = TRUE)

# Methode de code pour le Cluster
library(purrr)
set.seed(123)

#Fonction pour calculer la somme total intra-cluster du carre
iss <- function(k){
  kmeans(customer_data[,3:5], k, iter.max = 100, nstart = 100, algorithm = "Lloyd")$tot.withinss}

k.values <- 1:10

iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     col = "blue", type = "b", pch = 19, frame = FALSE,
     xlab = "Nombre de clusters K",
     ylab = "somme totale intra-cluster du carre")

library(cluster)
library(gridExtra)
library(grid)

k2 <- kmeans(customer_data[,3:5], 2, iter.max=100, nstart=50, algorithm="Lloyd")
s2 <- plot(silhouette(k2$cluster, dist(customer_data[,3:5], "euclidean")))

k3 <- kmeans(customer_data[,3:5], 3, iter.max=100, nstart=50, algorithm="Lloyd")
s3 <- plot(silhouette(k3$cluster, dist(customer_data[,3:5], "euclidean")))

k4 <- kmeans(customer_data[,3:5], 4, iter.max=100, nstart=50, algorithm="Lloyd")
s4 <- plot(silhouette(k4$cluster, dist(customer_data[,3:5], "euclidean")))

k5 <- kmeans(customer_data[,3:5], 5, iter.max=100, nstart=50, algorithm="Lloyd")
s5 <- plot(silhouette(k5$cluster, dist(customer_data[,3:5], "euclidean")))

k6 <- kmeans(customer_data[,3:5], 6, iter.max=100, nstart=50, algorithm="Lloyd")
s6 <- plot(silhouette(k6$cluster, dist(customer_data[,3:5], "euclidean")))

k7 <- kmeans(customer_data[,3:5], 7, iter.max=100, nstart=50, algorithm="Lloyd")
s7 <- plot(silhouette(k7$cluster, dist(customer_data[,3:5], "euclidean")))

k8 <- kmeans(customer_data[,3:5], 8, iter.max=100, nstart=50, algorithm="Lloyd")
s8 <- plot(silhouette(k8$cluster, dist(customer_data[,3:5], "euclidean")))

k9 <- kmeans(customer_data[,3:5], 9, iter.max=100, nstart=50, algorithm="Lloyd")
s9 <- plot(silhouette(k9$cluster, dist(customer_data[,3:5], "euclidean")))

k10 <- kmeans(customer_data[,3:5], 10, iter.max=100, nstart=50, algorithm="Lloyd")
s10<- plot(silhouette(k10$cluster, dist(customer_data[,3:5], "euclidean")))

library(NbClust)
library(factoextra)

fviz_nbclust(customer_data[,3:5], kmeans,method= "silhouette")

# calculer la statistique de l'écart
set.seed(123)
gap_stat <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

## calculer la statistique de l'écart
k6 <- kmeans(customer_data[,3:5], 6, iter.max = 100, nstart = 50, algorithm = "Lloyd")
k6

# Visualisation des resultats du clutering a l'aide des deux premiers composant principaux
#Analyse des composant princupal
pcclust = prcomp(customer_data[,3:5], scale=FALSE)
summary(pcclust)

pcclust$rotation[,1:2]

######### Visualiser les CLUSER##############
set.seed(1)
ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) +
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5", "6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6")) +
  ggtitle("Segment of Mall Customers", subtitle = "Using K-means Clustering")


ggplot(customer_data, aes(x =Spending.Score..1.100., y = Age)) +
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5", "6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6")) +
  ggtitle("Segment of Mall Customers", subtitle = "Using K-means Clustering")

kCols= function(vec){cols=rainbow (length (unique(vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster <- k6$cluster; dignm <- as.character(digCluster); # K-means cluster

plot(pcclust$x[,1:2], col= kCols(digCluster), pch = 19, xlab = "K-means", ylab = "classes")
legend("bottomleft", unique(dignm), fill = unique(kCols(digCluster)))
