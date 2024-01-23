
# Aufgabe 1 ---------------------------------------------------------------

set.seed(1)
T <- 304
y <- numeric(T)
for (t in 4:T) {
  y[t] <- 0.1 * t + 2 * (t %% 12) + 10 + 0.5 * y[t-1] - 0.7 * y[t-2] + 0.6 * y[t-3]
}
x <- y[-(1:100)]
plot(x, type = "l", xlab = "t")
write.csv(x, file = "secret.csv")

### a)

x <- read.csv("secret.csv", row.names = 1)[, 1]
plot(x[1:50], type = "l")
abline(v = 12 * 1:4 - 5, col = "red") # sieht nach Frequenz = 12 aus
x <- ts(data = x, frequency = 12)
x_dec <- decompose(x)
plot(x_dec)
rest <- as.numeric(x_dec$random)
rest <- rest[!is.na(rest)]

### b)

acf(rest)
pacf(rest)

### c)

AR <- MA <- numeric(4)

for (p in 1:4) AR[p] <- arima(rest, order = c(p, 0, 0))$aic
for (q in 1:4) MA[q] <- arima(rest, order = c(0, 0, q))$aic

data.frame(
  "aic" = c(AR, MA), 
  row.names = c(paste("AR", 1:4), paste("MA", 1:4))
)

# Aufgabe 2 ---------------------------------------------------------------

### a)

geburten <- read.csv("geburten_deutschland.csv")
geburten <- ts(geburten$births, start = 1950, frequency = 12)

geburten_train <- window(geburten, start = 2000, end = 2020 + 1/12)
geburten_test <- window(geburten, start = 2020 + 2/12)

plot(geburten_train, type = "l", xlim = c(2000, 2024))
lines(
  seq(2020 + 2/12, 2023 + 1/12, by = 1/12), geburten_test, 
  type = "l", col = "red"
)

t <- 1:length(geburten_train)
model <- lm(geburten_train ~ t + I(t^2) + I(t^3) + as.factor(t %% 12))
summary(model)

### b)

plot(model$residuals, type = "l")
acf(model$residuals)
pacf(model$residuals) # Vermutung: AR(2)
zyklus <- arima(model$residuals, order = c(2, 0, 0), include.mean = FALSE)

### c)

Box.test(zyklus$residuals, lag = 5, type = "Box-Pierce")
Box.test(zyklus$residuals, lag = 5, type = "Ljung-Box")

# Aufgabe 3 ---------------------------------------------------------------

### a)

geburten <- read.csv("geburten_deutschland.csv")
geburten <- ts(geburten$births, start = 1950, frequency = 12)

geburten_train <- window(geburten, start = 2000, end = 2020 + 1/12)
geburten_test <- window(geburten, start = 2020 + 2/12)

plot(geburten_train, type = "l", xlim = c(2000, 2024))

### b)

zyklus_prognose <- predict(zyklus, n.ahead = 36)$pred

alpha_1 <- zyklus$coef[1]
alpha_2 <- zyklus$coef[2]

### c)

t <- length(geburten_train) + 1:length(geburten_test)
trend_saison_prognose <- predict(model, newdata = data.frame("t" = t))
prognose <- trend_saison_prognose + zyklus_prognose

plot(
  geburten_train, type = "l", xlim = c(2000, 2024), ylim = c(5e4, 9e4),
  ylab = "Geburten"
)
lines(
  seq(2020 + 2/12, 2023 + 1/12, by = 1/12), geburten_test, 
  type = "l", col = "red"
)
lines(
  seq(2020 + 2/12, 2023 + 1/12, by = 1/12), prognose, 
  type = "l", col = "blue"
)
legend("topright", c("true", "predicted"), col = c("red", "blue"), lwd = 2)
