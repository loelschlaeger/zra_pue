
# Aufgabe 1 ---------------------------------------------------------------

### b)

data <- read.csv("geburten_deutschland.csv")
births <- ts(data$births, start = 1950, frequency = 12)
births_decomposed <- decompose(births, type = "additive")
plot(births_decomposed)

## übrigens, die Restkomponente beinhaltet anscheinend noch Information:
## (beachte hier auch die etwas irreführende Skala auf der x-Achse)
acf(births_decomposed$random, na.action = na.pass)

### c)

## wir brauchen unsere Funktion für die gleitende Glättung vom Aufgabenblatt 4
gleitende_glaettung <- function(data, p = 1, f = 1, g = rep(1/(1 + p + f), 1 + p + f)) {
  T <- length(data)
  smoothed <- rep(NA, T)
  for (t in (p + 1):(T - f)) {
    smoothed[t] <- as.numeric(data[(t - p):(t + f)] %*% g)
  }
  return(smoothed)
}

## bestimme zunächst die Trendkomponente mit symmetrischen moving averages
g <- c(0.5, rep(1, 11), 0.5) / 12
trend <- gleitende_glaettung(data = births, p = 6, f = 6, g = g)

## bestimme die fixe Saisonkomponente, indem die Trendkomponente abgezogen und
## der durchschnittliche Wert in den Saisons berechnet wird
season <-  births - trend
index <- seq.int(1, length(births), by = 12) - 1
figure <- numeric(12)
for (i in 1:12) {
  figure[i] <- mean(season[index + i], na.rm = TRUE)
}
seasonal <- rep(figure, floor(length(births) / 12) + 1)[1:length(births)]

## die Restkomponente ist dann einfach noch das, was von der Zeitreihe übrig 
## bleibt, nachdem Trend und Saisonkomponente abgezogen wurden
random <- births - seasonal - trend

## plotte abschließend die Zerlegung
plot(
  cbind(observed = births, trend = trend, seasonal = seasonal, random = random), 
  main = paste("My decomposition")
)


# Aufgabe 2 ---------------------------------------------------------------

### c)

set.seed(1)
T <- 100
a <- -0.5
b <- 0.1
epsilon <- rnorm(T)

y1 <- y2 <- y3 <- y4 <- numeric(T)
for (t in 1:T) {
  y1[t] <- a * epsilon[t] + b
  y2[t] <- if (t > 2) epsilon[t] + b * epsilon[t - 2] else NA
  y3[t] <- t + b * epsilon[1]
  y4[t] <- if (t > 1) a * y4[t - 1] + epsilon[t] else epsilon[t]
}

par(mfrow = c(2, 2))
plot(1:T, y1, type = "l")
plot(1:T, y2, type = "l")
plot(1:T, y3, type = "l")
plot(1:T, y4, type = "l")


# Aufgabe 3 ---------------------------------------------------------------

### b)

par(mfrow = c(2, 2))
acf(y1)
acf(y2, na.action = na.pass)
acf(y3)
acf(y4)

### c)

par(mfrow = c(2, 2))
pacf(y1)
pacf(y2, na.action = na.pass)
pacf(y3)
pacf(y4)

