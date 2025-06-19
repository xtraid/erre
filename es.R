
install.packages(c("cluster", "factoextra", "dplyr", "ggplot2", 
                   "corrplot", "VIM", "tidyr", "ggally", "dendextend",
                   "pheatmap", "mclust", "factoextra"))

# Caricamento della libreria
library(factoextra)
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(corrplot)
library(VIM)
library(tidyr)
library(ggally)
library(cluster)
library(dendextend)
library(pheatmap)
library(mclust)
library(GGally)

#analisi esplorativa

data=read.csv("UNLifeExpectancy.csv", header= T)
View(data)
str(data)
dim(data)

#gestione missing values

missing_analysis= data %>% 
  summarise_at(vars(-COUNTRY, -REGION), ~sum(is.na(.))) %>%
  gather(key = "Variable", value= "Missing_Count") %>%
  mutate(Missing_Percent=round(Missing_Count / nrow(data)*100,1))
print(missing_analysis)

# Escludiamo variabili con >40% missing
clustering_vars <- c("LIFEEXP", "ILLITERATE", "POP", "FERTILITY", 
                     "PRIVATEHEALTH", "PUBLICEDUCATION", "HEALTHEXPEND", 
                     "BIRTHATTEND", "PHYSICIAN", "GDP")

# Dataset pulito
clean_data <- data %>%
  select(COUNTRY, all_of(clustering_vars)) %>%
  na.omit()

View(clean_data)
str(clean_data)
dim(clean_data)

#standardizzazione

data_for_cluster= clean_data %>% select(-COUNTRY)
scaled_data=scale(data_for_cluster)

#verifichiamo sia tutto normalizzato

colMeans(scaled_data)
apply(scaled_data, 2, sd)
# happy

ggpairs(scaled_data)

#cerchiamo quanti centri

k_values <- 2:8
wss_values <- numeric(length(k_values))

for(i in seq_along(k_values)) {
  kmeans_result <- kmeans(scaled_data, centers = k_values[i], nstart = 25)
  wss_values[i] <- kmeans_result$tot.withinss
}

# Plot del gomito
plot(k_values, wss_values, type = "b", 
     xlab = "Numero Cluster", ylab = "WSS")
 
#cerco dove smette di cadere drasticamente 3 - 5 

#analisi silhouette
silhouette_scores= numeric(length(k_values))

for(i in seq_along(k_values)) {
  kmeans_result <- kmeans(scaled_data, centers = k_values[i], nstart = 25)
  sil_score=silhouette(kmeans_result$cluster, dist(scaled_data))
  silhouette_scores[i]=mean(sil_score[,3])
  
}

plot(k_values, silhouette_scores, type = "b",
     xlab = "Numero Cluster", ylab = "Silhouette Score")

#mah fanno un po schifo tutti ma 3 è il migliore

#facciamo il clustering

k=3
set.seed(7)
k_means=kmeans(scaled_data, centers=k,nstart=25)
final_result= clean_data %>%
  mutate(Cluster = as.factor(k_means$cluster))


#analisi
# Statistiche per cluster
cluster_summary <- final_result %>%
  group_by(Cluster) %>%
  summarise(across(all_of(clustering_vars), mean))

# Paesi per cluster  
final_result %>%
  group_by(Cluster) %>%
  summarise(Countries = paste(COUNTRY[1:5], collapse = ", "))

print(cluster_summary)


# PCA per visualizzazione 2D
pca_result <- prcomp(scaled_data)
fviz_cluster(k_means, data = scaled_data, 
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
             geom = "point", ellipse.type = "convex")

# Silhouette plot
fviz_silhouette(silhouette(k_means$cluster, dist(scaled_data)))


#analisi cluster con agnes

methods <- c("average", "single", "complete", "ward")
agnes_results <- list()  # Lista vuota per memorizzare i risultati

agnes_single=agnes(scaled_data, method= "single")
agnes_single$ac
#0.876
# Metti le etichette alla stessa altezza: hang = -1
pltree(agnes_single, cex=0.8, hang = -1, main = "agnes (single)", xlab="", sub ="")

plot(agnes_single, which=1, col=c(8,1), main="")



agnes.comp<-agnes(scaled_data,method="complete")
plot(agnes.comp, which=1, col=c(8,1), main="")

agnes.comp$ac
pltree(agnes.comp, cex = 0.8, hang = -1, main = "agnes (completo)", xlab="", sub ="")
#0.91

agnes.ave<-agnes(scaled_data,method="average")
agnes.ave$ac

#0.899
pltree(agnes.ave, cex = 0.8, hang = -1, main = "agnes (average)", xlab="", sub ="")


agnes.Ward<-agnes(scaled_data,method="ward")
agnes.Ward$ac
#0.95
pltree(agnes.Ward, cex = 0.8, hang = -1, main = "agnes (Ward)", xlab="", sub ="")

hc<-cutree(agnes.ave, 6)
cnames<-row.names(as.matrix(scaled_data))
cnames[hc==2]


pltree(agnes.ave, hang=-1, cex = 0.7, main="")
abline(h=1000)
rect.hclust(agnes.ave, k = 4, border = 3:7)



