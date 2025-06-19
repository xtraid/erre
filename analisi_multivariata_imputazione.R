# Gestione valori mancanti - Dataset Earnings
# Homework: Analisi e imputazione missing values

# Carico le librerie
library(tidyverse)
library(ggcorrplot)
library(ggplot2)

# Carico i dati
data <- read.csv("earnings.csv")

# ===== 1. PREPROCESSING =====
# I valori 99 sono codificati come missing, li converto in NA

cat("Controllo valori 99:\n")
cat("Mother education 99s:", sum(data$mother_education == 99, na.rm = TRUE), "\n")
cat("Father education 99s:", sum(data$father_education == 99, na.rm = TRUE), "\n")

# Converto 99 in NA
data$mother_education[data$mother_education == 99] <- NA
data$father_education[data$father_education == 99] <- NA

cat("Dopo conversione:\n")
cat("Mother education NA:", sum(is.na(data$mother_education)), "\n")
cat("Father education NA:", sum(is.na(data$father_education)), "\n")

# ===== 2. ESPLORAZIONE DATASET =====

cat("\n--- Dataset summary ---\n")
print(summary(data))

# Converto ethnicity in factor per analisi future
data$ethnicity_factor <- as.factor(data$ethnicity)

# Analisi missing per tutte le variabili
cat("\n--- Missing values per variabile ---\n")
missing_analysis <- data %>% 
  summarise_all(~sum(is.na(.))) %>%
  gather(key = "Variable", value = "Missing_Count") %>%
  mutate(Missing_Percent = round(Missing_Count / nrow(data) * 100, 1)) %>%
  arrange(desc(Missing_Count))

print(missing_analysis)

# ===== 3. PATTERN DEI MISSING =====
# Verifico se chi ha missing guadagna diversamente

# Creo indicatori di missing
data$mother_na <- is.na(data$mother_education)
data$father_na <- is.na(data$father_education)

# Test t per vedere se earnings differiscono
t_test_mother <- t.test(earn ~ mother_na, data = data)
t_test_father <- t.test(earn ~ father_na, data = data)

cat("\n--- Test differenze earnings ---\n")
cat("Mother missing p-value:", round(t_test_mother$p.value, 3), "\n")
cat("Father missing p-value:", round(t_test_father$p.value, 3), "\n")

# ===== 4. MODELLI PER IMPUTAZIONE =====
# Uso regressioni multivariata per predire i missing

cat("\n--- Correlazioni attuali ---\n")
cor_mother <- cor(data$mother_education, data$earn, use = "complete.obs")
cor_father <- cor(data$father_education, data$earn, use = "complete.obs")
cat("Correlazione madre-earnings:", round(cor_mother, 3), "\n")
cat("Correlazione padre-earnings:", round(cor_father, 3), "\n")

# Modelli di predizione
model_mother <- lm(mother_education ~ age + male + height + education, data = data)
model_father <- lm(father_education ~ age + male + height + education, data = data)

cat("\n--- R-squared modelli ---\n")
cat("Modello madre R²:", round(summary(model_mother)$r.squared, 3), "\n")
cat("Modello padre R²:", round(summary(model_father)$r.squared, 3), "\n")

# ===== 5. IMPUTAZIONE =====

# Salvo distribuzioni originali per confronto
original_mother <- data$mother_education[!is.na(data$mother_education)]
original_father <- data$father_education[!is.na(data$father_education)]

# Copio il dataset
data_imputed <- data

# Imputo mother_education
missing_mother_idx <- which(is.na(data_imputed$mother_education))
if(length(missing_mother_idx) > 0) {
  predicted_mother <- predict(model_mother, newdata = data_imputed[missing_mother_idx, ])
  data_imputed$mother_education[missing_mother_idx] <- round(predicted_mother, 0)
}

# Imputo father_education  
missing_father_idx <- which(is.na(data_imputed$father_education))
if(length(missing_father_idx) > 0) {
  predicted_father <- predict(model_father, newdata = data_imputed[missing_father_idx, ])
  data_imputed$father_education[missing_father_idx] <- round(predicted_father, 0)
}

cat("\n--- Valori imputati ---\n")
cat("Mother education:", length(missing_mother_idx), "valori\n")
cat("Father education:", length(missing_father_idx), "valori\n")

# ===== 6. VALIDAZIONE =====

cat("\n--- Confronto distribuzioni ---\n")
cat("MOTHER EDUCATION:\n")
cat("Media originale:", round(mean(original_mother), 1), "\n")
cat("Media imputata:", round(mean(data_imputed$mother_education), 1), "\n")

cat("\nFATHER EDUCATION:\n")
cat("Media originale:", round(mean(original_father), 1), "\n")
cat("Media imputata:", round(mean(data_imputed$father_education), 1), "\n")

# Grafici di confronto
par(mfrow = c(2, 2))
hist(original_mother, main = "Mother Ed - Originale", col = "lightblue")
hist(data_imputed$mother_education, main = "Mother Ed - Imputato", col = "lightcoral")
hist(original_father, main = "Father Ed - Originale", col = "lightblue")
hist(data_imputed$father_education, main = "Father Ed - Imputato", col = "lightcoral")
par(mfrow = c(1, 1))

# ===== 7. PULIZIA FINALE =====

# Rimuovo variabili ausiliarie
data_imputed <- data_imputed %>% select(-mother_na, -father_na, -ethnicity_factor)

# Controllo missing finali
cat("\n--- Controllo finale missing ---\n")
missing_final <- data_imputed %>% 
  summarise(across(where(is.numeric), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count")
print(missing_final)

# Se ci sono ancora missing, li rimuovo
if(any(missing_final$Missing_Count > 0)) {
  cat("Rimuovo osservazioni con missing rimanenti\n")
  data_imputed <- na.omit(data_imputed)
}

# ===== 8. ANALISI CORRELAZIONI =====

# Esploro relazioni con earnings
plot(data_imputed$mother_education, log(data_imputed$earn), 
     main = "Mother Education vs Log(Earnings)")
plot(data_imputed$father_education, log(data_imputed$earn), 
     main = "Father Education vs Log(Earnings)")

# Matrice correlazioni
datanum <- data_imputed %>% select_if(is.numeric)
corrdata <- round(cor(datanum, use = "complete.obs"), 2)
ggcorrplot(corrdata, type="lower", title = "Correlation Matrix")

# ===== 9. REGRESSIONE MULTIVARIATA =====

# Modello completo
full_model <- lm(earn ~ height + weight + male + education + 
                   mother_education + father_education + tense + angry + age, 
                 data = data_imputed)

cat("\n--- Significatività variabili ---\n")
p_values <- summary(full_model)$coefficients[,4]
non_significant <- p_values[p_values > 0.05]
cat("Variabili non significative (p > 0.05):\n")
print(names(non_significant))

# Modello ridotto (solo variabili significative)
final_model <- lm(earn ~ height + male + education + angry + age, 
                  data = data_imputed)

cat("\n--- Summary modello finale ---\n")
print(summary(final_model))

# ===== 10. DIAGNOSTICA MODELLO =====

# Grafici diagnostici
par(mfrow = c(2, 2))
plot(final_model, main = "Diagnostica Modello")
par(mfrow = c(1, 1))

# Calcolo predizioni
data_imputed$predict_earn <- predict(final_model)
data_imputed$residuals <- residuals(final_model)

# ===== 11. PERFORMANCE =====

actual <- data_imputed$earn
predicted <- data_imputed$predict_earn

# Metriche
r_squared <- cor(actual, predicted)^2
rmse <- sqrt(mean((actual - predicted)^2))
mae <- mean(abs(actual - predicted))
mape <- mean(abs((actual - predicted) / actual) * 100)

cat("\n--- Performance modello ---\n")
cat("R²:", round(r_squared, 3), "\n")
cat("RMSE: $", round(rmse, 0), "\n")
cat("MAE: $", round(mae, 0), "\n")
cat("MAPE:", round(mape, 1), "%\n")

# Confronto modelli
cat("\n--- Confronto modelli ---\n")
cat("R² completo:", round(summary(full_model)$r.squared, 3), "\n")
cat("R² finale:", round(summary(final_model)$r.squared, 3), "\n")
cat("AIC completo:", round(AIC(full_model), 0), "\n")
cat("AIC finale:", round(AIC(final_model), 0), "\n")

cat("\nAnalisi completata!\n")

