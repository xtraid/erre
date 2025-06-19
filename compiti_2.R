# =============================================================================
# DATA ANALYTICS - HOMEWORK 2: ANALISI DATASET FEV
# =============================================================================

# -----------------------------------------------------------------------------
# SETUP E CARICAMENTO DATI
# -----------------------------------------------------------------------------
# Caricamento del dataset
data <- read.csv("fev.csv")

# Visualizzazione struttura dati
str(data)
summary(data)

# Installazione pacchetti necessari (se non già installati)
# install.packages(c("ggplot2", "gridExtra"))
library(ggplot2)
library(gridExtra)

# =============================================================================
# 1. ANALISI UNIVARIATA DI CIASCUNA VARIABILE
# =============================================================================

# -----------------------------------------------------------------------------
# VARIABILI QUANTITATIVE
# -----------------------------------------------------------------------------

# AGE (Età)
par(mfrow = c(2, 2))
hist(data$AGE, main = "Distribuzione Età", xlab = "Età (anni)", 
     col = "lightblue", probability = TRUE)
lines(density(data$AGE))

boxplot(data$AGE, main = "Boxplot Età", ylab = "Età (anni)" )

# FEV (Capacità Polmonare)
hist(data$FEV, main = "Distribuzione FEV", xlab = "FEV (litri)", 
     col = "lightcoral", probability = TRUE)
lines(density(data$FEV), col = "red", lwd = 2)

boxplot(data$FEV, main = "Boxplot FEV", ylab = "FEV (litri)", col = "lightgoldenrod")

par(mfrow = c(1, 2))
# HEIGHT (Altezza)
hist(data$HEIGHT, main = "Distribuzione Altezza", xlab = "Altezza (pollici)", 
     col = "lightsteelblue", probability = TRUE)
lines(density(data$HEIGHT), col = "red", lwd = 2)

boxplot(data$HEIGHT, main = "Boxplot Altezza", ylab = "Altezza (pollici)", col = "lightsalmon")

# -----------------------------------------------------------------------------
# VARIABILI CATEGORICHE
# -----------------------------------------------------------------------------

par(mfrow = c(2, 2))

# SEX (Genere)
barplot(table(data$SEX), main = "Distribuzione Genere", 
        xlab = "Genere", ylab = "Frequenza", col = c("pink", "lightblue"))

pie(table(data$SEX), main = "Distribuzione Genere", col = c("pink", "lightblue"))

# SMOKE (Fumatore)
barplot(table(data$SMOKE), main = "Distribuzione Fumatori", 
        xlab = "Fumatore (0=No, 1=Sì)", ylab = "Frequenza", 
        col = c("lightgreen", "orange"))
pie(table(data$SMOKE), main = "Distribuzione Fumatori", 
    labels = c("Non Fumatori", "Fumatori"), col = c("lightgreen", "orange"))

# Statistiche descrittive
cat("\n=== STATISTICHE DESCRITTIVE ===\n")
cat("Età - Media:", mean(data$AGE), "Mediana:", median(data$AGE), "\n")
cat("FEV - Media:", mean(data$FEV), "Mediana:", median(data$FEV), "\n")
cat("Altezza - Media:", mean(data$HEIGHT), "Mediana:", median(data$HEIGHT), "\n")
cat("Genere - Percentuali:\n")
print(prop.table(table(data$SEX)) * 100)
cat("Fumatori - Percentuali:\n")
print(prop.table(table(data$SMOKE)) * 100)

# =============================================================================
# 2. DIFFERENZE FEV TRA MASCHI E FEMMINE
# =============================================================================

par(mfrow = c(1, 1))

# Istogrammi sovrapposti
hist(data$FEV[data$SEX == "Female"], 
     xlim = range(data$FEV), ylim = c(0, 80),
     col = rgb(1, 0, 0, 0.5), main = "Distribuzione FEV per Genere",
     xlab = "FEV (litri)", ylab = "Frequenza")

hist(data$FEV[data$SEX == "Male"], 
     col = rgb(0, 0, 1, 0.5), add = TRUE)

legend("topright", legend = c("Femmine", "Maschi"), 
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))

# Boxplot per confronto
par(mfrow = c(1, 2))
boxplot(FEV ~ SEX, data = data, main = "FEV per Genere", 
        xlab = "Genere", ylab = "FEV (litri)", col = c("pink", "lightblue"))

# Test t per differenze significative
t_test_result <- t.test(FEV ~ SEX, data = data)
cat("\n=== TEST T PER DIFFERENZE FEV TRA GENERI ===\n")
print(t_test_result)

# =============================================================================
# 3. REGRESSIONE LINEARE: FEV ~ HEIGHT
# =============================================================================

# Modello di regressione
model_height <- lm(FEV ~ HEIGHT, data = data)

# Sommario del modello
cat("\n=== REGRESSIONE FEV ~ HEIGHT ===\n")
summary(model_height)

# Grafico di dispersione con retta di regressione
par(mfrow = c(1, 1))
plot(data$HEIGHT, data$FEV, main = "Regressione FEV ~ Altezza", 
     xlab = "Altezza (pollici)", ylab = "FEV (litri)", pch = 16, col = "blue")
abline(model_height, col = "red", lwd = 2)

# Aggiunta equazione della retta
coef_height <- coef(model_height)
equation <- paste("FEV =", round(coef_height[1], 3), "+", 
                  round(coef_height[2], 3), "* HEIGHT")
legend("topleft", legend = equation, bty = "n", cex = 1.2)

# R-squared e altre metriche
r_squared <- summary(model_height)$r.squared
cat("R-squared:", r_squared, "\n")
cat("R-squared aggiustato:", summary(model_height)$adj.r.squared, "\n")

# -----------------------------------------------------------------------------
# TRASFORMAZIONE IN CENTIMETRI
# -----------------------------------------------------------------------------

# Conversione pollici in centimetri (1 pollice = 2.54 cm)
data$HEIGHT_CM <- data$HEIGHT * 2.54

# Nuovo modello con altezza in cm
model_height_cm <- lm(FEV ~ HEIGHT_CM, data = data)

cat("\n=== REGRESSIONE FEV ~ HEIGHT (in cm) ===\n")
summary(model_height_cm)

# Confronto grafici
par(mfrow = c(1, 2))
plot(data$HEIGHT, data$FEV, main = "FEV ~ Altezza (pollici)", 
     xlab = "Altezza (pollici)", ylab = "FEV (litri)", pch = 16, col = "blue")
abline(model_height, col = "red", lwd = 2)

plot(data$HEIGHT_CM, data$FEV, main = "FEV ~ Altezza (cm)", 
     xlab = "Altezza (cm)", ylab = "FEV (litri)", pch = 16, col = "green")
abline(model_height_cm, col = "red", lwd = 2)

# =============================================================================
# 4. ANALISI DEI RESIDUI
# =============================================================================

# Calcolo residui
residui <- residuals(model_height)
valori_previsti <- fitted(model_height)

# Grafico residui vs valori previsti
par(mfrow = c(1, 1))
plot(valori_previsti, residui, main = "Residui vs Valori Previsti", 
     xlab = "Valori Previsti", ylab = "Residui", pch = 16, col = "blue")

# Linee orizzontali a ±sqrt(varianza dei residui)
varianza_residui <- var(residui)
sqrt_var <- sqrt(varianza_residui)

abline(h = 0, col = "black", lwd = 1)
abline(h = sqrt_var, col = "red", lwd = 2, lty = 2)
abline(h = -sqrt_var, col = "red", lwd = 2, lty = 2)

legend("topright", legend = c("Residui = 0", paste("±√Var =", round(sqrt_var, 3))), 
       col = c("black", "red"), lty = c(1, 2), lwd = c(1, 2))

cat("\n=== ANALISI RESIDUI ===\n")
cat("Varianza residui:", varianza_residui, "\n")
cat("Deviazione standard residui:", sqrt_var, "\n")

# =============================================================================
# 5. REGRESSIONE LINEARE A TRATTI (HEIGHT = 63 pollici)
# =============================================================================

# Creazione variabili per regressione a tratti
cutoff <- 63
data$group <- ifelse(data$HEIGHT <= cutoff, "low", "high")

# Modelli separati per i due gruppi
model_low <- lm(FEV ~ HEIGHT, data = data[data$group == "low", ])
model_high <- lm(FEV ~ HEIGHT, data = data[data$group == "high", ])

cat("\n=== REGRESSIONE A TRATTI (cutoff = 63 pollici) ===\n")
cat("Modello per HEIGHT ≤ 63:\n")
summary(model_low)
cat("\nModello per HEIGHT > 63:\n")
summary(model_high)

# Grafico della regressione a tratti
par(mfrow = c(1, 1))
plot(data$HEIGHT, data$FEV, main = "Regressione Lineare a Tratti", 
     xlab = "Altezza (pollici)", ylab = "FEV (litri)", pch = 16, 
     col = ifelse(data$HEIGHT <= cutoff, "blue", "red"))

# Aggiunta delle rette di regressione
height_low <- seq(min(data$HEIGHT[data$group == "low"]), cutoff, length.out = 100)
height_high <- seq(cutoff, max(data$HEIGHT[data$group == "high"]), length.out = 100)

lines(height_low, predict(model_low, newdata = data.frame(HEIGHT = height_low)), 
      col = "blue", lwd = 2)
lines(height_high, predict(model_high, newdata = data.frame(HEIGHT = height_high)), 
      col = "red", lwd = 2)

# Linea verticale al cutoff
abline(v = cutoff, col = "black", lty = 2, lwd = 1)

legend("topleft", legend = c("HEIGHT ≤ 63", "HEIGHT > 63", "Cutoff"), 
       col = c("blue", "red", "black"), lty = c(1, 1, 2), lwd = c(2, 2, 1))

# =============================================================================
# 6. VALUTAZIONE QUALITÀ REGRESSIONI (MSE)
# =============================================================================

# MSE per regressione semplice
mse_simple <- mean((data$FEV - predict(model_height))^2)

# MSE per regressione a tratti
pred_piecewise <- ifelse(data$HEIGHT <= cutoff, 
                         predict(model_low, newdata = data), 
                         predict(model_high, newdata = data))
mse_piecewise <- mean((data$FEV - pred_piecewise)^2)

cat("\n=== CONFRONTO QUALITÀ MODELLI (MSE) ===\n")
cat("MSE Regressione Semplice:", mse_simple, "\n")
cat("MSE Regressione a Tratti:", mse_piecewise, "\n")
cat("Miglioramento:", mse_simple - mse_piecewise, "\n")
cat("Riduzione percentuale errore:", ((mse_simple - mse_piecewise) / mse_simple) * 100, "%\n")

# =============================================================================
# 7. REGRESSIONE A TRATTI CON CONTINUITÀ
# =============================================================================

# Per imporre continuità, il valore delle due rette deve essere uguale a x = 63
# Modello: FEV = a1 + b1*HEIGHT per HEIGHT ≤ 63
#          FEV = a2 + b2*HEIGHT per HEIGHT > 63
# Con vincolo: a1 + b1*63 = a2 + b2*63

# Creazione variabili per il modello continuo
data$HEIGHT_adj <- ifelse(data$HEIGHT > cutoff, data$HEIGHT - cutoff, 0)
data$indicator <- ifelse(data$HEIGHT > cutoff, 1, 0)

# Modello con continuità forzata
model_continuous <- lm(FEV ~ HEIGHT + HEIGHT_adj, data = data)

cat("\n=== REGRESSIONE A TRATTI CON CONTINUITÀ ===\n")
summary(model_continuous)

# Predizioni per il modello continuo
pred_continuous <- predict(model_continuous)
mse_continuous <- mean((data$FEV - pred_continuous)^2)

# Grafico della regressione continua
par(mfrow = c(1, 1))
plot(data$HEIGHT, data$FEV, main = "Regressione a Tratti Continua", 
     xlab = "Altezza (pollici)", ylab = "FEV (litri)", pch = 16, 
     col = ifelse(data$HEIGHT <= cutoff, "blue", "red"))

# Predizioni per il grafico
height_seq <- seq(min(data$HEIGHT), max(data$HEIGHT), length.out = 200)
newdata_seq <- data.frame(
  HEIGHT = height_seq,
  HEIGHT_adj = ifelse(height_seq > cutoff, height_seq - cutoff, 0),
  indicator = ifelse(height_seq > cutoff, 1, 0)
)
pred_seq <- predict(model_continuous, newdata = newdata_seq)

lines(height_seq, pred_seq, col = "purple", lwd = 2)
abline(v = cutoff, col = "black", lty = 2, lwd = 1)

legend("topleft", legend = c("HEIGHT ≤ 63", "HEIGHT > 63", "Regressione Continua", "Cutoff"), 
       col = c("blue", "red", "purple", "black"), 
       lty = c(NA, NA, 1, 2), pch = c(16, 16, NA, NA), lwd = c(NA, NA, 2, 1))

# =============================================================================
# CONFRONTO FINALE TUTTI I MODELLI
# =============================================================================

cat("\n=== CONFRONTO FINALE MODELLI ===\n")
cat("MSE Regressione Semplice:", mse_simple, "\n")
cat("MSE Regressione a Tratti:", mse_piecewise, "\n")
cat("MSE Regressione Continua:", mse_continuous, "\n")

# R-squared per tutti i modelli
r2_simple <- summary(model_height)$r.squared
r2_continuous <- summary(model_continuous)$r.squared

# Calcolo R-squared per modello a tratti
ss_total <- sum((data$FEV - mean(data$FEV))^2)
ss_res_piecewise <- sum((data$FEV - pred_piecewise)^2)
r2_piecewise <- 1 - (ss_res_piecewise / ss_total)

cat("\nR-squared:\n")
cat("Regressione Semplice:", r2_simple, "\n")
cat("Regressione a Tratti:", r2_piecewise, "\n")
cat("Regressione Continua:", r2_continuous, "\n")

# Ripristino layout grafico
par(mfrow = c(1, 1))
c
