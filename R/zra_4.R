
data <- read.csv("geburten_deutschland.csv")
births <- ts(data$births, start = 1950, frequency = 12)
T <- length(births)


# Aufgabe 1 ---------------------------------------------------------------

### c)

plot(births)

gleitende_glaettung <- function(data, p = 1, f = 1, g = rep(1/(1 + p + f), 1 + p + f)) {
  T <- length(data)
  smoothed <- rep(NA, T)
  for (t in (p + 1):(T - f)) {
    smoothed[t] <- as.numeric(data[(t - p):(t + f)] %*% g)
  }
  return(smoothed)
}

# backward smoothing
lines(ts(gleitende_glaettung(births, p = 50, f = 0), start = 1950, frequency = 12), col = "red")

# forward smoothing
lines(ts(gleitende_glaettung(births, p = 0, f = 50), start = 1950, frequency = 12), col = "green")

# moving averages
lines(ts(gleitende_glaettung(births, p = 30, f = 30), start = 1950, frequency = 12), col = "blue")

legend(
  "topright", legend = c("backward smoothing", "forward smoothing", "moving averages"),
  col = c("red", "green", "blue"), lwd = 1
)


# Aufgabe 2 ---------------------------------------------------------------

### a)

a <- rep(NA, T)
a[1] <- 1 # births[1]
alpha <- 0.1 # 0.9
for (t in 2:T) {
  a[t] <- alpha * births[t] + (1 - alpha) * a[t - 1]
}

plot(births, xlim = c(1950, 1970))
lines(ts(a, start = 1950, frequency = 12), col = "green")

### b)

N <- rep(NA, T)
b <- rep(NA, T)
N[1] <- births[1] # 1
b[1] <- -500 # 1e6
alpha <- 0.3 # 0.9
beta <- 0.3 # 0.9
for (t in 2:T) {
  N[t] <- alpha * births[t] + (1 - alpha) * (N[t - 1] + b[t - 1])
  b[t] <- (1 - beta) * b[t - 1] + beta * (N[t] - N[t - 1])
}

plot(births, xlim = c(1950, 1970))
lines(ts(N, start = 1950, frequency = 12), col = "blue")

### c)

einfach <- HoltWinters(births, alpha = NULL, beta = FALSE, gamma = FALSE, start.periods = 10)
einfach$coefficients[["a"]]
predict(einfach, n.ahead = 4)

zweifach <- HoltWinters(births, alpha = NULL, beta = NULL, gamma = FALSE, start.periods = 10)
zweifach$coefficients[["a"]] + 4 * zweifach$coefficients[["b"]]
predict(zweifach, n.ahead = 4)


# Aufgabe 3 ---------------------------------------------------------------

births_subset <- window(births, start = 2010)

### a)

plot(births_subset)
abline(v = 2010:2023 + 1/12, col = "red") # Februar
abline(v = 2010:2022 + 6/12, col = "green") # Juli
legend(
  "topright", legend = c("weniger Geburten im Februar", "mehr im Juli"),
  col = c("red", "green"), lwd = 1
)

subset(data, year == 2022 & month == 2)$births
sum(subset(data, year == 2022 & month %in% 4:6)$births)

### b)

model_1 <- lm(
  formula = births ~ 1 + 
    I(12 * (year - 2010) + month) + 
    I((12 * (year - 2010) + month)^2) + 
    as.factor(month),
  data = subset(data, year >= 2010)
)
summary(model_1)

### c)

model_2 <- lm(
  formula = births ~ 0 + 
    I(12 * (year - 2010) + month) + 
    I((12 * (year - 2010) + month)^2) + 
    as.factor(month),
  data = subset(data, year >= 2010)
)
summary(model_2)

plot(births_subset)
lines(ts(predict(model_1), start = 2010, frequency = 12), col = "blue")
lines(ts(predict(model_2), start = 2010, frequency = 12), col = "red")
