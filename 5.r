## Importazione dati -----

# impostare la directory di lavoro
getwd()
setwd("") # o Sessione -> Imposta directory di lavoro o scheda File -> Altro

## read.table(<nome.file>, <argomenti>)

## read.table("../data/mydata.dat", header=T)

## read.csv(<percorso file + nome file>, <argomenti>)

Cibo <- read.csv("data/food_coded1.csv")

Cibo <- read.csv("data/food_coded1.csv", header=TRUE, sep = ";")

# Disponibile all'indirizzo: https://www.kaggle.com/borapajo/food-choices

# E' possibile leggere i dati anche direttamente dal file xlsx
# install.packages("openxlsx") #oppure dalla scheda Pacchetti -> Installa
# library(openxlsx) #carica il pacchetto per utilizzare le sue funzioni
# a <- read.xlsx("data/food_coded1.xlsx")

sigaretta <-read.table("importazione dataset/cigarette.txt",
                        intestazione = F)
cipolle <- read.table("importazione dataset/cipole.dat",
                      intestazione = T, stringhe come fattori = T)
macchine <-read.csv("importazione dataset/macchine/macchine.data",
                     intestazione = F, na.strings = "?")
nazioni <-read.csv("importazione dataset/nazioni.csv")
mulino a vento <- read.table("importazione dataset/windmill.txt",
                       dec = ",", intestazione = T)
str(mulino a vento)

## Selezione del data frame -----

dim(cibo)
str(Cibo)
testa (cibo)
lunghezza (cibo)
# per caricare dati con altre estensioni è possibile utilizzare le funzioni
# nel pacchetto: straniero

Cibo$GPA
Cibo[, "Media dei voti"]
Cibo[, 1]

str(Cibo[[1]])

Food$Gender[2:7] #selezione degli elementi in una singola variabile
str(Cibo$Genere)

is.numeric(Food$Gender) #accedendo ad una singola colonna è possibile trattarla come vettore
is.factor(Cibo$Genere)

Food$Gender <- factor(Food$Gender) #convertiamo la variabile in un fattore con 2 livelli
Cibo$Genere
livelli(Cibo$Sesso) <- c("Femmina","Maschio") #rinominiamo i livelli
str(Cibo$Genere)

#quante donne ci sono nel dataset?

sum(Cibo$Genere=="Femmina")
lunghezza(Cibo$Genere[Cibo$Genere=="Femmina"])
tabella(Cibo$Genere)[1]

#Selezioniamo il peso dei maschi
Cibo$peso[Cibo$genere=="Maschio"]

Cibo[Cibo$Gender=="Maschio", "peso"]
Cibo[Cibo$Gender=="Maschio", c("Media", "peso")]

#selezioniamo le colonne sport e lavoro solo per le donne
#le condizioni sono specifiche nel campo dedicato alle righe
Cibo[Cibo$Gender=="Femmina", c("sport", "impiego")]

#selezioniamo il genere dei soggetti con sport==1 e vitamine==1
Cibo$Genere[Cibo$sport==1&Cibo$vitamine==1]

#selezioniamo i valori fruit day e veggie day per gli studenti maschi
#che hanno dichiarato fare attività sportiva (sports==1)

Cibo2 <- Cibo[Cibo$Gender=="Maschio"&Cibo$sports==1, c("giorno_frutta","giorno_verdure")]
Cibo.sottoinsieme <- sottoinsieme(Cibo, sottoinsieme = (Genere=="Maschio"& sport==1),
       seleziona = c(giorno_frutta, giorno_verdura))

#eliminiamo la variabile peso selezionando chi ha impiego ==1 o ==2
Cibo[Cibo$occupazione==1|Cibo$occupazione==2, -48]
Cibo[Cibo$occupazione<3, -48]

sottoinsieme (cibo, sottoinsieme = (occupazione==1|occupazione==2),
       seleziona = -c(peso))

#creiamo un nuovo data frame con studenti maschi con reddito >3,
#salvando nel nuovo oggetto solo le variabili da reddito a nutrizionali_check
newdata <- subset(Cibo, Genere=="Maschio" e reddito > 3,
                  seleziona=c(reddito:controllo_nutrizionale))
str(nuovi dati)


### Esercizi ----
# I seguenti esercizi richiedono il caricamento dei dati AutoBi su
# sinistri per lesioni personali automobilistiche nei dati assicurativi del pacchetto R.
# Ottienilo eseguendo i seguenti comandi

install.packages("insuranceData")
libreria("datiassicurativi")
dati (AutoBi)

#1. Utilizzare una funzione adatta per analizzare la struttura dei dati.
# Controlla i nomi, la dimensione ed elenca le prime 8 osservazioni.


#2. Utilizzare la funzione summary() per ottenere statistiche riassuntive delle
# variabili numeriche in un frame di dati.

#3. Cosa restituiscono le seguenti righe? In cosa differiscono i due comandi per
# il sottoinsieme?
AutoBi$CLMAGE[AutoBi$CLMAGE<20]
subset(AutoBi, subset=CLMAGE<20, select=CLMAGE)

#4. Seleziona le variabili MARITAL e CLMINSUR ed i soggetti rappresentati da un
#avvocato, escludi i valori mancanti utilizzando la funzione na.omit

#5. Seleziona le donne di età inferiore ai 40 anni, sposate o divorziate, e con
# perdite superiori a cinquemila dollari.

#6. Aggiungi una nuova colonna con la variabile LOSS su scala logaritmica

## Valori mancanti - NA ------

which(is.na(Food$calories_day)) #numero di righe di valori mancanti

is.na(Cibo$calorie_giorno)
media(Cibo$calorie_giorno)
mean(Cibo$calorie_giorno, na.rm = TRUE)
#eliminiamo le calorie che mi mancano su_day
d<im(Cibo[!is.na(Cibo$calorie_giorno),])

na.omit(Cibo) #teniamo solo i casi completi
casi completi (cibo)

## File di esportazione -----

write.table(Cibo,file="Cibo.dat")
write.table(Cibo,file="Cibo.csv", sep=",", row.names = FALSE)
#write.csv(...)
salva(Cibo,file="Cibo.RData")
salva(file="Cibo.RData")
#salva.immagine()

#load("Cibo.RData")


### Esercizi ----
# Il set di dati `feeling` riguarda l'atteggiamento degli elettori americani
# nei confronti di diverse categorie di individui. Ogni variabile esprime un
#punteggio da 0 a 100, indicando un atteggiamento sfavorevole, indifferente
# o favorevole nei confronti dei gruppi di persone oggetto della domanda

load("dati/sensazione.Rdata")

#1. Definisci una variabile categoriale dalla variabile `ft_immig_2016`
# con le seguenti classi: fortemente sfavorevole (0-25),
# sfavorevole/indifferente (26-50), leggermente favorevole (51-75), favorevole (76-100).
# Aggiungi la nuova variabile ai dati.
# Utilizza la funzione 'cut()'

#2. Rimuovi i valori mancanti ed esporta il dataframe in file .csv.