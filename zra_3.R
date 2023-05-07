
# Aufgabe 1 ---------------------------------------------------------------

### c)

set.seed(1)
x_1 <- rep(1:5, times = 2)
x_2 <- rep(1:5, each = 2)
y <- round(3 + x_1 - 2 * x_2 + rnorm(10))
data <- data.frame(y, x_1, x_2)
model <- lm(y ~ x_1 + x_2, data)
summary(model)
predict(model, data.frame(x_1 = 6, x_2 = -1)) # oder model$coefficients %*% c(1, 6, -1)

# Aufgabe 2 ---------------------------------------------------------------

### a)

her_f <- read.table("hermannslauf_frauen.csv", header = TRUE, sep = ",")
head(her_f)
her_f_date <- her_f$Jahr
her_f_min <- her_f$h * 60 + her_f$m + her_f$s / 60
her_f_min_ts <- ts(her_f_min, start = 1972)
plot(her_f_min_ts, ylab = "min")

### b)

T <- length(her_f_min)
t <- 1:T
max_degree <- 9
degrees <- 1:max_degree
models <- list()
for (d in degrees) {
  models[[d]] <- lm(her_f_min ~ poly(t, d, raw = TRUE), na.action = na.exclude)
}
plot(her_f_min_ts, ylab = "min")
legend(
  "topright", legend = c("Zeitreihe", paste("degree", degrees)), 
  lty = 1, col = c("black", degrees + 1)
)
for (d in degrees) {
  predictions <- fitted(models[[d]])
  points(her_f_date, predictions, type = "l", col = d + 1)
  invisible(readline(prompt = "Press [enter] to continue"))
}

# F-Tests 
T <- length(na.omit(her_f_min))
p_values <- rep(NA, max_degree - 1)
k_ur <- max_degree
SSR_ur <- sum(models[[max_degree]]$residuals^2)
degree_seq <- 1:(max_degree - 1) # oder (max_degree - 1):1 (absteigend)
for (d in degree_seq) {
  SSR_r <- sum(models[[d]]$residuals^2)
  k_r <- d
  Fstat <- ((SSR_r - SSR_ur) / (k_ur - k_r)) / ((SSR_ur) / (T - k_ur - 1))
  p_values[d] <- 1 - pf(Fstat, k_ur - k_r, T - k_ur - 1)
}
data.frame("p.value" = round(p_values, 4))

# Modellselektionskriterien
crit <- matrix(NA, length(degrees), 4)
colnames(crit) <- c("R^2", "adj. R^2", "AIC", "BIC")
for (d in degrees) {
  crit[d, 1] <- summary(models[[d]])$r.squared
  crit[d, 2] <- summary(models[[d]])$adj.r.squared
  crit[d, 3] <- AIC(models[[d]])
  crit[d, 4] <- BIC(models[[d]])
}
round(crit, 4)

# Kreuzvalidierung
train_proportion <- 0.5
train_ind <- 1:round(length(her_f_min) * train_proportion)
train_data <- her_f_min[train_ind]
test_data <- na.omit(her_f_min[-train_ind])
max_degree <- 9
degrees <- 1:max_degree
models <- list()
for (d in degrees) {
  models[[d]] <- lm(train_data ~ poly(1:length(train_ind), d, raw = TRUE))
}
ssr <- rep(NA, max_degree)
for (d in degrees) {
  t_test <- 1:length(test_data) + length(train_data)
  new_data <- data.frame(1, poly(t_test, d, raw = TRUE))
  predictions <- as.matrix(new_data) %*% models[[d]]$coefficients
  ssr[d] <- sum((test_data - predictions)^2)
}
data.frame("ssr" = round(ssr, 4))
plot(her_f_min_ts, ylab = "min")
legend(
  "topright", legend = c("Zeitreihe", paste("degree", degrees)), 
  lty = 1, col = c("black", degrees + 1)
)

### c)

plot(her_f_min_ts, ylab = "min")
abline(v = 2005, col = "red")


# Aufgabe 3 ---------------------------------------------------------------

### c)

T <- 100
t <- 1:T
u <- rnorm(T) 
c <- ts(3 + 0.4*t + 0.3*t^2 + u)
plot(c)
plot(diff(c))
plot(diff(c, difference = 2))
