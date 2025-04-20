#Es 1
# Esercizio a: Mostra a schermo la sequenza di numeri naturali da 20 a 50
v=20:50  # Crea un vettore di numeri interi da 20 a 50
v  # Visualizza il vettore

# Esercizio b: Mostra a schermo la sequenza di numeri razionali da 20 a 50 intervallati con distanza 0.01
vr=seq(from=20, to=50, by=0.01)  # Crea un vettore di numeri da 20 a 50 con incrementi di 0.01
vr  # Visualizza la sequenza

# Esercizio c: Salva in un vettore chiamato sum la somma dei numeri trovati al punto b ed in mean la loro media
sum=sum(vr)  # Calcola la somma di tutti i valori nel vettore vr
sum  # Visualizza la somma

mean=mean(vr)  # Calcola la media di tutti i valori nel vettore vr
mean  # Visualizza la media

#Es 2
# Funzione ricorsiva per calcolare l'n-esimo numero della sequenza di Fibonacci
fibo = function(n, el_1 = 1, el_2 = 1) {
  if(n == 0)
    return(el_2)
  else {
    app = el_1 + el_2
    el_1 = el_2
    el_2 = app
    return(fibo(n-1, el_1, el_2))
  }
}

# Funzione che calcola i primi n numeri di Fibonacci e restituisce anche la loro somma e media
fun = function(n) {
  somma = 0
  for (i in 1:n) {
    elemento = fibo(i)
    print(elemento)  # Stampa ogni numero di Fibonacci
    somma = somma + elemento
  }
  print(paste("Somma:", somma))  # Stampa la somma dei primi n numeri
  media = somma/n
  print(paste("Media:", media))  # Stampa la media dei primi n numeri
  return(media)
}

# Crea un vettore di 10 numeri casuali compresi tra 1 e 10 (con possibili ripetizioni)
vett = sample(1:10, 10, replace=TRUE)

# Crea una lista con i nomi di 3 cantanti
cantanti = list("Eminem", "Caparezza", "Elisa")

# Crea una matrice 3x2 con i numeri primi 2, 3, 5, 7, 11, 13
mx = matrix(c(2, 3, 5, 7, 11, 13), nrow=3)

# Crea una lista principale assegnando un nome a ciascun elemento
my__list = list("Numeri"= vett, "Matrice"= mx, "Cantanti"= cantanti)

# Visualizza la lista creata
my__list

