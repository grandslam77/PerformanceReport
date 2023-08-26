ilosc_kwadratow <- 100000
paleta_kolorow <- c("#8E19FE", "#5400A5", "#310060")

# Wygenerowanie losowych danych
dane <- data.frame(
  x = runif(ilosc_kwadratow, 0, 50),
  y = runif(ilosc_kwadratow, 0, 50),
  kolor = sample(paleta_kolorow, ilosc_kwadratow, replace = TRUE)
)
require(ggplot2)
# Tworzenie wykresu
wykres <- ggplot(dane, aes(x = x, y = y, fill = kolor)) +
  geom_tile(width = 1, height = 1) +
  scale_fill_identity() +
  theme_void()

# Wyświetlenie wykresu
print(wykres)

library(ggplot2)

# Parametry
ilosc_kolumn <- 20
ilosc_rzedow <- 1
paleta_kolorow <- c("red", "blue", "green", "orange", "purple")

# Wygenerowanie losowych danych
dane <- expand.grid(
  x = 1:ilosc_kolumn,
  y = 1:ilosc_rzedow
)
dane$kolor <- sample(paleta_kolorow, ilosc_kolumn * ilosc_rzedow, replace = TRUE)

# Tworzenie wykresu
wykres <- ggplot(dane, aes(x = x, y = y, fill = kolor)) +
  geom_tile(width = 1, height = 1, color = "white") +
  scale_fill_identity() +
  theme_void() +
  coord_fixed(ratio = 1, xlim = c(0.5, ilosc_kolumn + 0.5), ylim = c(0.5, ilosc_rzedow + 0.5))

# Wyświetlenie wykresu
print(wykres)

library(ggplot2)

# Parametry
ilosc_kolumn <- 100
ilosc_rzedow <- 1
paleta_kolorow <- c("red", "blue", "green", "orange", "purple")

# Wygenerowanie losowych danych
dane <- expand.grid(
  x = 1:ilosc_kolumn,
  y = 1:ilosc_rzedow
)
dane$kolor <- sample(paleta_kolorow, ilosc_kolumn * ilosc_rzedow, replace = TRUE)

# Tworzenie wykresu
wykres <- ggplot(dane, aes(x = x, y = y, fill = kolor)) +
  geom_tile(width = 1, height = 1, color = NA) +  # NA usuwa linie
  scale_fill_identity() +
  theme_void() +
  coord_fixed(ratio = 1, xlim = c(0.5, ilosc_kolumn + 0.5), ylim = c(0.5, ilosc_rzedow + 0.5))

# Wyświetlenie wykresu
print(wykres)



library(ggplot2)

# Parametry
ilosc_kolumn <- 25
ilosc_rzedow <- 100
paleta_kolorow <- c("#C68CFF", "#8E19FE", "#5400A5", "#310060")
paleta_kolorow <- c( "#8E19FE", "#5400A5", "#310060")
# Wygenerowanie losowych danych
dane <- expand.grid(
  x = 1:ilosc_kolumn,
  y = 1:ilosc_rzedow
)

# Inicjalizacja zmiennej przechowującej poprzedni kolor
poprzedni_kolor <- sample(paleta_kolorow, 1)

# Funkcja do generowania kolorów
generuj_kolor <- function() {
  repeat {
    nowy_kolor <- sample(paleta_kolorow, 1)
    if (nowy_kolor != poprzedni_kolor) {
      return(nowy_kolor)
    }
  }
}

dane$kolor <- sapply(1:(ilosc_kolumn * ilosc_rzedow), function(i) {
  kolor <- generuj_kolor()
  poprzedni_kolor <<- kolor
  return(kolor)
})

# Tworzenie wykresu
wykres <- ggplot(dane, aes(x = x, y = y, fill = kolor)) +
  geom_tile(width = 1, height = 1, color = NA) +
  scale_fill_identity() +
  theme_void() +
  coord_fixed(ratio = 1, xlim = c(0.5, ilosc_kolumn + 0.5), ylim = c(0.5, ilosc_rzedow + 0.5))

# Wyświetlenie wykresu
print(wykres)



