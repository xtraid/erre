# Matrici ed array ----

x <- matrice(c(2,3,5,7,11,13), nrow=3)
X
x <- matrice(c(2,3,5,7,11,13), ncol=3)
X

mx <- matrice(x, ncol=3, byrow=TRUE)
mx

nrow(mx)

ncol(mx)
dim(mx)
dim(mx)[1]

rownames(mx) <- c("A", "B")
nomicolo(mx) <- c("a", "b", "c")

x[1,]
x[,2]

x[,-2]
x[,c(1,3)]
x[,1:2]


## selezione degli elementi: analoga ai vettori ma su due dimensioni
## [riga/righe, colonna/e]
mx[2,1]     
mx[2,2]     

# selezione di un'intera riga o colonna
mx[2, ]             
mx["B", ]

mx[ ,3]         
mx[ ,"c"]    

mx[, c("a", "b")] # possiamo utilizzare anche i nomi di riga o colonna

x <- matrice(1:16, ncol=4)
X
y <- x[c(1,4),2:4]
e

##array: le matrici sono array con due dimensioni,
## ma possono avere anche più di due dimensioni
x <- 1:20
ax <- array(x, dim=c(5,2,2))
ascia
dim(ax)

ax[3,2,1] #la selezione è analoga alle matrici
ascia[ ,2, ]
ascia[-1, ,1]


str(2:4)
y <- matrice(2:4, nrow = 1) # vettore riga
e
str(y)

y <- matrice(2:4, ncol = 1) # vettore colonna
e
str(y)


## una matrice è un array
a <- matrice(1:6, ncol = 3, nrow = 2)

è.matrice(a); è.array(a)

## è un vettore, non più una matrice. dim(b) #NULL
b <- a[,2]
str(b)
è.vettore(b)
è.matrice(b)
dim(b)

# notare cosa si ottiene convertendo un vettore in matrice
come.matrice(b)

## Operazioni di algebra lineare ----

y <- c(1,2, 3, 0,4, 10)
2*anni

crossprod(c(1,2,3), c(0,12,13)) # prodotto scalare tra due vettori
m <- c(1,2,3) %*% c(0,12,13)
M
è.matrice(m)

## prodotto riga per colonna
a <- matrice(c(1,2,3,4), ncol = 2, byrow = T)
b <- matrice(c(1,-1,0,1), ncol = 2, byrow = T)
un; b
un*b
un%*%b
crossprod(a,b) #t(a) %*% b
tcrossprod(a,b) #a %*% t(b)
crossprod(t(a),b)  
tcrossprod(a,t(b))

X <- matrice(runif(100000),50)
sistema.tempo(crossprod(X))
sistema.tempo(t(X)%*%X)

a <- matrice(c(1,1,-1,1),nrow=2,ncol=2)
UN
risolvere(a) ##inversa
risolvere(b)

b <- c(2,4)
risolvere(a,b) #soluzione del sistema lineare Ax=b => x=A^-1 b

diag(a) #estrai la diagonale
diag(b) #matrice diagonale con b elementi sulla diagonale
diag(3) #matrice identità 3x3

# chol()
# det()
# auto()
# T()


# Lista -----
# collezione di oggetti anche di tipo diverso
x <- vector("lista", lunghezza = 3) # oppure list()
x <- lista()
X

x1 <- 1:3
x2 <- c("A", "B", "C", "D", "E")
x3 <- matrice(1:12, nrow=3)

mylist <- list(x1, x2, x3) #creiamo la lista popolandola con i 3 oggetti
str(mialista)

##selezione degli elementi della lista
la mia lista[[1]]
la mia lista[[2]][3]
la mia lista[[3]]
mia lista[[3]][1,1]

l1 <- mialista[[1]]
l1
l1[2] # o in alternativa:
la mia lista[[1]][2]

# selezioniamo le prime due righe e tre colonne della matrice in posizione 3 nella lista
# ...


x[[1]] <- x1

x[[4]] <- "questo è il quarto elemento della lista x" #creiamo un nuovo elemento

mylist2 <- list(comp1 = x1, comp2 = x2, comp3 = x3)
mylist2$comp1
lamialista2$comp2[3]


nuova lista <- c(mialista,mialista2)
is.list(nuovalista)
str(nuova lista)

nomi(mialista) <- c("A", "B", "C")
nomi(mialista2)
nomi(nuovalista) <- c("A", "B", "C")

nuova lista[[1]]
nuova lista$A
nuova lista[["A"]]

## valori speciali: NULL, TRUE (T), FALSE (F), NaN (non è un numero), NA (non disponibile), Inf
0/0 #Non è un numero
un <- -1/0
un #-Inf
aa #Non è un numero
come.numerico("a")

# Fattori ----
# vettori per variabili categoriali. Ogni gruppo corrisponde ad un livello
paese <- c("Italia","Germania","Francia","Germania","Germania","Germania",
             "Francia", "Italia", "Italia", "Francia")
str(paese)
countryf <- factor(country) # fattore vs as.factor
str(paese)
è.fattore(paesef)
come fattore (paese)
livelli(paese)

cbind(paese, paesef)

# scegli la prima classe
?rilivellare
a <- relevel(countryf, "Italia")
UN
#...o scegliere l'ordine delle classi
fattore(paese, livelli = c("Italia", "Germania", "Francia"))


paesef2 <- paesef
livelli(paese)
livelli(paesef2) <- c("Italia", "Germania", "Francia")
cbind(paesef, paesef2)
cbind.data.frame(paesef, paesef2)
#ordered() # fattore ordinato

età <- c(47,44,44,40,38,36,42,34,34,44)
tapply(età, paese, media)
cbind(età, paese)
genere <- c(1,1,2,1,1,2,1,2,2,2)
generef <- fattore(genere)
generef
livelli (genere)
livelli(generef) <- c("F","M")
str(generef)

######## Esercizi #########
#1. Definisci un vettore x con gli elementi 5, 12, 13, 12. Converti questo vettore
# in factor e ispeziona la sua struttura. Come vengono definiti i livelli?

#2. Crea un fattore dalla sequenza di stringhe "1", "1", "0", "1",
# "1", "0". Cosa restituiscono length() e mode()?

x <- c("1", "1", "0", "1", "1", "0")
X
lunghezza(x)
modalità(x)
str(x)

#3. Converti la variabile factor del punto precedente in un factor
# con livelli "m" e "f". Cosa produce il comando table()?

livelli(x) <- c("m", "f")
x2 <- fattore(x, livelli = c("1","0"), etichette = c("f", "m"))
x2

#4. Eseguire le seguenti righe
v1 <- fattore(lettere[1:5])
livelli(v1) <- rev(livelli(v1))
v2 <- fattore(lettere[1:5], livelli = rev(lettere[1:5]))
#Cosa succede a v1 quando modifica i suoi livelli? In cosa differisce v2
# da v1?



# Frame di dati ----
# è una lista ma può essere considerata come una matrice con colonne possibilmente di diverso tipo
# le componenti devono essere vettori (numerici, caratteri o logici), fattori, matrici numeriche, liste o altri dataframe
# vettori e matrici devono avere la stessa dimensione
# di solito memorizziamo le variabili nelle colonne e le unità nelle righe
under40 <- età < 40
dat <- data.frame(Paese=countryf, Età=età, Sesso=genderf,
                  Under40=under40)
dat$Paese
data[,1]
str(dato)
è.data.frame(dat)
head(dat) #stampa le prime 6 righe

## Visualizza(dat)

# Sottoinsieme

dati[3,2]
data[1:3, 2:4]
dati[3, ]

x <- dato[, 2]
str(x)

dat[ , c("Età", "Sesso")]
dati[ , c(2,3)]
data[ , 2:3]

str(dat[ , c("Età", "Sesso")])
dat["Età"]
str(dat["Età"])
str(dat[,"Età"])

dat$Sex #selezione di una sola colonna

dat$Under40 <- NULL
#dat <-dat[, -2]
testa(dat)
testa(dat)

cbind.data.frame(dat, sotto40)
X <- cbind.data.frame(dat, under40)
cbind.data.frame(dat, Under40= under40*1)

#creare una nuova variabile logica uguale a TRUE se Paese == Italia
dat$CountryTF <- dat$Country == "IT"

## convertire i caratteri in fattore automaticamente
df <- data.frame(x = 1:5,
                 y = c("A", "B", "C", "D", "E"))
df <- data.frame(x = 1:5,
                 y = c("A", "B", "C", "D", "E"),
                 stringheComeFattori = T)

Età #Le singole variabili non sono direttamente accessibili

allega(dat)
Età
dat$Age <- Età + 1
Età <- Età + 1 #non eseguito
data$Età
Age #il nuovo valore della variabile Age non è visibile finché il data frame non viene scollegato
stacca(dat)
Età
data$Età

######## Esercizi #########
#1. Esegui il codice seguente
x <- runif(8)
y <- lettere[1:8]
z <- campione(c(rep(T,5),rep(F,3)))
#Definisci un dataframe chiamato newdf con colonne z, y, x.


#2. Crea un dataframe con 5 righe, utilizzando un elenco di caratteri
# che rappresenta i nomi e un vettore di numeri che rappresentano le età.
