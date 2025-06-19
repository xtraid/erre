# Clustering Analysis - UN Life Expectancy Data
# Homework: Analisi cluster su aspettativa di vita mondiale

# Carico le librerie
packages <- c("cluster", "factoextra", "dplyr", "ggplot2", "corrplot", 
              "VIM", "tidyr", "GGally", "dendextend", "pheatmap", "mclust")

# Installo se necessario (decommentare se serve)
# install.packages(packages)

library(cluster)
library(factoextra)
library(dplyr)
library(ggplot2)
library(corrplot)
library(VIM)
library(tidyr)
library(GGally)
library(dendextend)
library(pheatmap)
library(mclust)

# ===== 1. CARICAMENTO DATI =====

data <- read.csv("UNLifeExpectancy.csv", header = TRUE)

cat("Dataset caricato:", dim(data)[1], "paesi,", dim(data)[2], "variabili\n")
str(data)
head(data)

# ===== 2. GESTIONE MISSING VALUES =====

# Controllo missing per variabile
missing_analysis <- data %>% 
  summarise_at(vars(-COUNTRY, -REGION), ~sum(is.na(.))) %>%
  gather(key = "Variable", value = "Missing_Count") %>%
  mutate(Missing_Percent = round(Missing_Count / nrow(data) * 100, 1)) %>%
  arrange(desc(Missing_Percent))

cat("\n--- Missing values ---\n")
print(missing_analysis)

# Seleziono variabili con <40% missing per clustering
clustering_vars <- c("LIFEEXP", "ILLITERATE", "POP", "FERTILITY", 
                     "PRIVATEHEALTH", "PUBLICEDUCATION", "HEALTHEXPEND", 
                     "BIRTHATTEND", "PHYSICIAN", "GDP")

# Dataset pulito (rimuovo righe con NA)
clean_data <- data %>%
  select(COUNTRY, all_of(clustering_vars)) %>%
  na.omit()

cat("Dataset pulito:", nrow(clean_data), "paesi\n")
cat("Paesi rimossi:", nrow(data) - nrow(clean_data), "\n")

# ===== 3. STANDARDIZZAZIONE =====

# Estraggo solo variabili numeriche e standardizzo
data_for_cluster <- clean_data %>% select(-COUNTRY)
scaled_data <- scale(data_for_cluster)

cat("\n--- Controllo standardizzazione ---\n")
cat("Medie (devono essere ~0):\n")
print(round(colMeans(scaled_data), 3))
cat("Deviazioni standard (devono essere 1):\n")
print(round(apply(scaled_data, 2, sd), 3))

# ===== 4. ANALISI CORRELAZIONI =====

# Matrice correlazioni
correlation_matrix <- cor(scaled_data)
corrplot(correlation_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black",
         title = "Correlazioni tra variabili")

# Pairs plot
ggpairs(as.data.frame(scaled_data), 
        title = "Distribuzione e correlazioni variabili standardizzate")

# ===== 5. NUMERO OTTIMALE DI CLUSTER =====

k_values <- 2:8

# cerco i centri (WSS)
cat("\n--- Numero di centri ---\n")
wss_values <- numeric(length(k_values))

for(i in seq_along(k_values)) {
  set.seed(123)
  kmeans_result <- kmeans(scaled_data, centers = k_values[i], nstart = 25)
  wss_values[i] <- kmeans_result$tot.withinss
}

plot(k_values, wss_values, type = "b", pch = 19, col = "blue",
     xlab = "Numero Cluster", ylab = "WSS",
     main = "Metodo del Gomito")
grid()

# Metodo Silhouette
cat("--- Analisi Silhouette ---\n")
silhouette_scores <- numeric(length(k_values))

for(i in seq_along(k_values)) {
  set.seed(123)
  kmeans_result <- kmeans(scaled_data, centers = k_values[i], nstart = 25)
  sil_score <- silhouette(kmeans_result$cluster, dist(scaled_data))
  silhouette_scores[i] <- mean(sil_score[, 3])
}

plot(k_values, silhouette_scores, type = "b", pch = 19, col = "red",
     xlab = "Numero Cluster", ylab = "Silhouette Score",
     main = "Analisi Silhouette")
grid()

optimal_k <- k_values[which.max(silhouette_scores)]
cat("Numero ottimale cluster:", optimal_k, "\n")

# ===== 6. K-MEANS CLUSTERING =====

# Eseguo k-means con k=3
k <- 3
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = k, nstart = 25)

# Aggiungo cluster al dataset
final_result <- clean_data %>%
  mutate(Cluster = as.factor(kmeans_result$cluster))

cat("\n--- Risultati K-means ---\n")
cat("Paesi per cluster:\n")
print(table(final_result$Cluster))

# ===== 7. ANALISI CLUSTER =====

# Statistiche per cluster
cluster_summary <- final_result %>%
  group_by(Cluster) %>%
  summarise(across(all_of(clustering_vars), mean, .names = "{.col}_mean"),
            Count = n()) %>%
  arrange(Cluster)

cat("\n--- Medie per cluster ---\n")
print(cluster_summary)

# Esempi paesi per cluster
countries_by_cluster <- final_result %>%
  group_by(Cluster) %>%
  summarise(
    Count = n(),
    Examples = paste(head(COUNTRY, 5), collapse = ", "),
    .groups = 'drop'
  )

cat("\n--- Esempi paesi ---\n")
print(countries_by_cluster)

# ===== 8. VISUALIZZAZIONI =====

# Silhouette plot
sil_plot <- silhouette(kmeans_result$cluster, dist(scaled_data))
fviz_silhouette(sil_plot, main = "Silhouette Plot K-means")

# Heatmap centroidi
centers_df <- as.data.frame(kmeans_result$centers)
centers_df$Cluster <- paste("Cluster", 1:k)

centers_long <- centers_df %>%
  pivot_longer(cols = -Cluster, names_to = "Variable", values_to = "Value")

ggplot(centers_long, aes(x = Variable, y = Cluster, fill = Value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, name = "Z-score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Centroidi K-means", x = "Variabili", y = "Cluster")

# ===== 9. CLUSTERING GERARCHICO =====

cat("\n--- Clustering gerarchico ---\n")

# Test diversi metodi linkage
methods <- c("average", "single", "complete", "ward")
agnes_results <- list()

for(method in methods) {
  agnes_result <- agnes(scaled_data, method = method)
  agnes_results[[method]] <- agnes_result
  cat("Coefficiente agglomerativo", method, ":", round(agnes_result$ac, 3), "\n")
}

# Migliore metodo (AC più alto)
best_method <- "ward"
best_agnes <- agnes_results[[best_method]]
cat("Migliore metodo:", best_method, "AC =", round(best_agnes$ac, 3), "\n")

# Dendrogrammi
par(mfrow = c(2, 2))
for(method in methods) {
  pltree(agnes_results[[method]], cex = 0.6, hang = -1, 
         main = paste("AGNES -", method),
         sub = paste("AC =", round(agnes_results[[method]]$ac, 3)))
}

# Dendrogramma dettagliato migliore
par(mfrow = c(1, 1))
pltree(best_agnes, cex = 0.8, hang = -1, 
       main = paste("Dendrogramma AGNES -", best_method),
       sub = paste("AC =", round(best_agnes$ac, 3)))

# ===== 10. CONFRONTO METODI =====

# Taglio dendrogramma
hc_clusters <- cutree(best_agnes, k = 3)

# Confronto k-means vs gerarchico
comparison <- table(K_means = kmeans_result$cluster, 
                    Hierarchical = hc_clusters)
cat("\n--- Confronto metodi ---\n")
print(comparison)

# Similitudine tra metodi
ari <- adjustedRandIndex(kmeans_result$cluster, hc_clusters)
cat("Adjusted Rand Index:", round(ari, 3), "\n")

# ===== 11. SUMMARY FINALE =====

cat("\n--- RIASSUNTO ANALISI ---\n")
cat("Paesi originali:", nrow(data), "\n")
cat("Paesi analizzati:", nrow(clean_data), "\n")
cat("Variabili usate:", length(clustering_vars), "\n")
cat("Cluster finali:", k, "\n")
cat("Silhouette score:", 
    round(mean(silhouette(kmeans_result$cluster, dist(scaled_data))[, 3]), 3), "\n")
cat("Migliore AC gerarchico:", round(best_agnes$ac, 3), "\n")
cat("Similitudine metodi:", round(ari, 3), "\n")

cat("\nAnalisi clustering completata!\n")