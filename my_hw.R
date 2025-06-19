#===========================================================================
#DATA ANALITYCS - HOMEWORK 2 
#===========================================================================

#---------------------------------------------------------------------------
#SETUP E CARICAMENTO DATI
#---------------------------------------------------------------------------

#Caricamento dataset
data<-read.csv("fev.csv")

#Visualizzazione del dataset
str(data)
summary(data)

#installazione pacchetti necessari
library(ggplot2)


#===========================================================================
#ANALISI UNIVARIATA
#===========================================================================

#---------------------------------------------------------------------------
#VARIABILI QUANTITATIVE
#---------------------------------------------------------------------------

#AGE
par(mfrow=c(2,2))
hist(data$AGE, main= "Distribuzione Età", xlab= "Età (anni)", probability=TRUE)
lines(density(data$AGE))

boxplot(data$FEV, main="Boxplot FEV", xlab="capacità polmonare (litri)")

#FEV
hist(data$FEV, main= "Distribuzione FEV", xlab= "capacità polmonare (litri)", probability=TRUE)
lines(density(data$FEV))

boxplot(data$FEV, main(Boxplot$FEV, xlab="capacità polmonare (litri)"))

#HEIGHT
par(mfrow = c(1, 2))
hist(data$HEIGHT, main= "Distribuzione altezza", xlab= "altezza (pollici)", probability=TRUE)
lines(density(data$HEIGHT))

boxplot(data$HEIGHT, main="boxplot altezza", xlab="altezza (pollici)")

#---------------------------------------------------------------------------
#VARIABILI QUALITATIVE
#---------------------------------------------------------------------------

par(mfrow = c(2, 2))

#SEX
barplot(table(data$SEX), main= "Distribuzione Genere", xlab= "Genere", ylab="frequenze", col=c("pink","lightblue") )

pie(table(data$SEX), main= "Distribuzione Genere", col=c("pink","lightblue"))

#SMOKE
barplot(table(data$SMOKE), main= "Distribuzione Fumatori", xlab= "Fumatore (0=no, 1=sì))", ylab="frequenze", col=c("green","grey") )

pie(table(data$SMOKE), main= "Distribuzione Fumatori", col=c("green","grey"))

#Statistiche descrittive
cat("\n======= STATISTICHE =======\n")
cat("Età - Media:", mean(data$AGE), "Età - Mediana:", median(data$AGE), "\n")
cat("FEV - Media:", mean(data$FEV), "FEV - Mediana:", median(data$FEV), "\n")
cat("Altezza - Media:", mean(data$HEIGHT), "Altezza - Mediana:", median(data$HEIGHT), "\n")
cat("Genere - percentuale:\n")
print(prop.table(table(data$SEX))*100)
cat("Fumatori - percentuale:\n")
print(prop.table(table(data$SMOKE))*100)

#===========================================================================
#DIFFERENZE FEV MASCHI / FEMMINE
#===========================================================================

par(mfrow= c(1,1))
#istogrammi sovrapposti
hist(data$FEV[data$SEX=="Female"], xlim=range(data$FEV), ylim=c(0,80), col= "pink", main="Distribuzione FEV per genere", xlab="FEV (litri)", ylab="frequenze")

hist(data$FEV[data$SEX=="Male"], col = "lightblue", add= TRUE)

legend("topright", legend=c("Femmine", "maschi"), fill= c("pink","lightblue"))

#boxplot per confronto
par(mfrow= c(1,2))
boxplot(FEV ~ SEX, data = data, main="FEV per Genere", xlab="Genere", ylab="FEV (litri)", col= c("pink", "lightblue"))

#t test per vedere se le differenze sono significative
t_test_result =t.test(FEV ~ SEX, data= data)
cat("\n======= T TEST =======\n")
print(t_test_result)


#===========================================================================
#REGRESSIONE LINEARE FEV 126 HEIGHT
#===========================================================================

#modello di regressione 
model_height= lm(FEV ~ HEIGHT, data= data)

cat("\n======= REGGRESSIONE FEV DIPENDENTE HEIGHT =======\n")
summary(model_height)

#plot

par(mfrow=c(1,1))
plot(data$HEIGHT, data$FEV, main = "regressione", xlab= "altezza (pollici)", ylab="FEV (litri)", pch = 16, col= "blue")
abline(model_height, col = "red")

#equazione
coefficienti= coef(model_height)
equazione=paste("FEV =", round(coefficienti[1],3),"+", round(coefficienti[2],3), "* HEIGHT")legend("topleft",legend=equazione)

#test r ^2
r_squared= summary(model_height)$r.squared
cat("R^2:", r_squared, "\n")
cat("R^2 aggiustato:", summary(model_height)$adj.r.squared, "\n")

#===========================================================================
#TRASFORMAZIONE LINEARE CENTIMETRI
#===========================================================================

#conversione 1 pollice= 2.54 cm
data$HEIGHT_cm= data$HEIGHT*2.54

#new model
model_cm=lm(FEV ~ HEIGHT, data= data)
cat("\n======= REGRESSIONE IN cm =======\n")
summary(model_cm)

#confronto tra grafici 
par(mfrow=c(1,2))
plot(data$HEIGHT, data$FEV, main = "FEV ~ ALTEZZA (pollici)", xlab= "altezza (pollici)", ylab="FEV (litri)", pch = 16, col= "blue")
abline(model_height, col = "red")
plot(data$HEIGHT_cm, data$FEV, main = "FEV ~ ALTEZZA (centimetri)", xlab= "altezza (centimetri)", ylab="FEV (litri)", pch = 16, col= "black")
abline(model_cm, col = "red")
#===========================================================================
#ANALISI DEI RESIDUI
#===========================================================================

residui= residuals(model_height)
valori_previsti= fitted(model_height)

#plot valori previsti e residuiù
par(mfrow=c(1,1))
plot(valori_previsti, residui, main="valori previsti e residui", xlab="valori previsti", ylab="residui", pch=16, col= "blue")

#line scarto quadratico medio
varianza_residui= var(residui)
sigma=sqrt(varianza_residui)

abline(h=0, col = "black", lwd= 1)
abline(h=sigma, col= "red",lwd= 2, lty= 2) 
abline(h=-sigma, col= "red",lwd= 2, lty= 2) 

legend("topleft", legend= c("residui= 0", paste("σ =", round(sigma,3))), col= c("black","red"), lty= c(1,2), lwd=c(1,2))
cat("\n======= ANALISI RESIDUI =======\n")
cat("varianza residui:", varianza_residui,"\n")
cat("deviazione dtandard residui:", sigma,"\n")


# =============================================================================
#REGRESSIONE LINEARE A TRATTI (height= 63 POLLICI)
# =============================================================================

#creazione variabili
cutoff=63
data$grup=ifelse(data$HEIGHT<= cutoff, "low","high")
model_low=lm(FEV ~ HEIGHT, data= data[data$grup=="low"])
model_high=lm(FEV ~ HEIGHT, data= data[data$grup=="high"])

cat("\n======= MODELLO DI REGRESSIO A TRATTI =======\n")
cat("modello per HEIGHT<= 63:\n")
summary(model_low)
cat("\nmodello per HEIGHT> 63\n")
summary(model_high)

#plot a tratti
par(mfrow= c(1,1))
plot(data$HEIGHT, data$FEV, main= "Regressione a tratti", xlab="altezza(pollici)", ylab= "FEV (litri)", pch=16, col= ifelse(data$HEIGHT<= cutoff, "blue","red"))

#aggiunta rette di regressione 
height_low= seq(min(data$HEIGHT[data$grup== "low"]), cutoff,lengh.out=100)
heigh_high= seq(cutoff, max(data$HEIGHT[data$grup== "high"]),lengh.out=100)

lines(height_low, predict(model_low, newdata=data.frame(HEIGHT= height_low)), col= "blue", lwd= 2)
lines(height_high, predict(model_high, newdata=data.frame(HEIGHT= height_high)), col= "red", lwd= 2)

#linea di cutoff
abline(v= cutoff, col= "black", lty= 2, lwd= 1)

legend("topleft", legend= c("HEIGHT<=63", "height>63", "Cutoff"),col= c("blue", "red", "black"), lty=c(1,1,2), lwd=c(2,2,1))


# =============================================================================
#VALUTAZIONE QUALITA' REGESSIONI
# =============================================================================

#MSE per regressione semplice

mse_semplice= mean((data$FEV - predict(model_height))^2)

#MSE per regressione a tratti
pred_pezzi= ifelse(data$HEIGHT<= cutoff , predict(model_low, newdata= data), predict(model_high, newdata= data))
mse_pezzi= mean((data$FEV-pred_pezzi)^2)

cat("\n======= CONFRONTO QUALITà MODELLI =======\n")
cat("mse regressione semplice:", mse_semplice,"\n")
cat("mse regressione a tratti:", mse_pezzi,"\n")
cat("miglioramento:", mse_semplice- mse_pezzi,"\n")
cat("riduzione percentuale errore:", ((mse_semplice-mse_pezzi)/mse_semplice)*100,"%\n")

# =============================================================================
#REGRESSIONE A TRATTI CONTINUA
# =============================================================================

# Si impone che le 2 rette siano alla stessa FEV per HEIGHT = CUTOFF= 63

#creazione variabili per il modello continuo
data$HEIGHT_adj= ifelse(data$HEIGHT> cutoff, data$HEIGHT - cutoff, 0)
data$indicator= ifelse(data$HEIGHT>cutoff, 1,0)

#modello con continuità forzata
model_continuo= lm(FEV ~ HEIGHT + HEIGHT_adj, data= data)

cat("\n======= REGRESSIONE A TRATTI CON CONTINUITA' =======\n")
summary(model_continuo)

#predizioni modello continuo
pred_continuo= predict(model_continuo)
mse_continuo= mean((data$FEV- predict(model_continuo))^2)

#plot

par(mfrow=c(1,1))
plot(data$HEIGHT, data$FEV,main= "regressione a tratti continua", xlab="altezza (pollici)", ylab="FEV (litri)", pch= 16, col= ifelse(data$HEIGHT<= cutoff, "blue","red"))

#predizione per il grafico
height_seq= seq(min(data$HEIGHT), max(data$HEIGHT), lengh.out = 200)
newdata_seq= data.frame(HEIGHT= height_seq, HEIGHT_adj= ifelse(height_seq>cutoff, height_seq -cutoff,0),indicator= ifelse(height_seq>cutoff,1,0))
pred_seq=predict(model_continuo, newdata= newdata_seq)

lines(height_seq, pred_seq, col="purple", lwd= 2)
abline(v=cutoff, col="black", lty=2, lwd=1)

legend("topleft", legend= c("HEIGHT<=63","HEIGHT>63", "regressione continua", "Cutoff"), col= c("blue", "red","purple", "black"), lty=c(NA,NA,1,2),pch=c(16,16,NA,NA), lwd=c(NA,NA,2,1))

# =============================================================================
#CONFRONTI FINALI
# =============================================================================

cat("\n======= CONFRONTI =======\n")
cat("mse regressione semplice:", mse_semplice,"\n")
cat("mse regressione a tratti:", mse_pezzi,"\n")
cat("mse regressione continua:", mse_continuo,"\n")

#r^2 per i modelli 
 r2_semplice= summary(model_height)$r.squared
 r2_continuo= summary(model_continuo)$r.squared

#r^2 per il modello a tratti

ss_total= sum((data$FEV - mean(data$FEV))^2)
ss_pezzi= sum((data$FEV - pred_pezzi)^2)
r2_pezzi=1-(ss_pezzi/ss_total)

cat("\nR^2:\n")
cat("regressione semplice:",r2_semplice, "\n")
cat("regressione pezzi:",r2_pezzi, "\n")
cat("regressione continua:",r2_continuo, "\n")

#ripristino il layout grafico 
par(mfrow=c(1,1))

























