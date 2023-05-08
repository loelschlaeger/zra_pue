
# Aufgabe 1 ---------------------------------------------------------------

### c)

set.seed(1)
x_1 <- rep(1:5, times = 2)
x_2 <- rep(1:5, each = 2)
y <- round(3 + x_1 - 2 * x_2 + rnorm(10))
data <- data.frame(y, x_1, x_2)
model <- lm(y ~ x_1 + x_2, data)
summary(model)
model$coefficients %*% c(1, 6, -1)

# Aufgabe 2 ---------------------------------------------------------------

### a)

her_f <- read.table("hermannslauf_frauen.csv", header = TRUE, sep = ",")
head(her_f)
her_f_min <- her_f$h * 60 + her_f$m + her_f$s / 60
her_f_min_ts <- ts(her_f_min, start = 1972)
plot(her_f_min_ts, ylab = "min")

### b)

t <- 1:length(her_f_min_ts)
dates <- her_f$Jahr[!is.na(her_f_min)]
models <- list()
models[[1]] <- lm(her_f_min ~ 1)
max_degree <- 9
for (d in 1:max_degree) {
  models[[d + 1]] <- lm(her_f_min ~ poly(t, degree = d, raw = TRUE))
}

plot(her_f_min_ts, ylab = "min")
legend(
  "topright", lty = 1, col = c(1, 1:(max_degree + 1) + 1),
  legend = c("ZR", paste("k =", c(0, 1:max_degree)))
)
for (d in 1:(max_degree + 1)) {
  fitted_values <- models[[d]]$fitted.values
  points(dates, fitted_values, type = "l", col = d + 1)
}

## Modellselektion
criteria <- matrix(NA, nrow = length(models), ncol = 6)
colnames(criteria) <- c("F-Test", "R^2", "adj. R^2", "AIC", "BIC", "SSE")
rownames(criteria) <- paste("k =", c(0, 1:max_degree))

# F-Tests
T <- length(dates)
k <- max_degree
residuals_ur <- models[[max_degree + 1]][["residuals"]]
SSR_ur <- sum(residuals_ur^2)
for (d in 1:(max_degree + 1)) {
  k1 <- d - 1
  residuals_r <- models[[d]][["residuals"]]
  SSR_r <- sum(residuals_r^2)
  F <- ((SSR_r - SSR_ur) / (k - k1)) / (SSR_ur / (T - k - 1))
  p_value <- 1 - pf(q = F, df1 = k - k1, df2 = T - k - 1)
  criteria[d, "F-Test"] <- p_value
}
round(criteria, 4)

# Modellselektionskriterien
T <- length(dates)
for (d in 1:(max_degree + 1)) {
  model <- models[[d]]
  k <- d - 1
  SSR <- sum(model$residuals^2)
  SST <- var(her_f_min, na.rm = TRUE) * (T - 1)
  criteria[d, "R^2"] <- 1 - SSR / SST
  criteria[d, "adj. R^2"] <- 1 - (SSR / (T - k - 1)) / (SST / (T - 1))
  criteria[d, "AIC"] <- T * log(SSR / T) + 2 * (k + 1)
  criteria[d, "BIC"] <- T * log(SSR / T) + log(T) * (k + 1)
}
round(criteria, 4)

# Kreuzvalidierung
full_data <- her_f_min
T <- length(full_data)
t <- 1:T
train_proportion <- 0.6
train_ind <- 1:round(T * train_proportion)
train_data <- her_f_min[train_ind]
test_ind <- t[-train_ind]
test_data <- full_data[test_ind]
model_train <- lm(train_data ~ 1)
prediction <- as.matrix(rep(1, length(test_ind))) %*% model_train$coefficients
criteria[1, "SSE"] <- sum(na.omit(as.numeric(test_data - prediction))^2)
for (d in 1:max_degree) {
  model_train <- lm(train_data ~ poly(train_ind, degree = d, raw = TRUE))
  prediction <- cbind(1, poly(test_ind, degree = d, raw = TRUE)) %*% model_train$coefficients
  criteria[d + 1, "SSE"] <- sum(na.omit(as.numeric(test_data - prediction))^2)
}
round(criteria, 4)

### c)

plot(her_f_min_ts)
abline(v = 2005, col = "red")
t <- 1:length(her_f_min_ts)
dates <- her_f$Jahr[!is.na(her_f_min)]
T1 <- which(dates == 2005)
zt <- t >= T1
model <- lm(her_f_min ~ 1 + t + zt + I(t^2))
points(dates, model$fitted.values, type = "l", col = "blue")


# Aufgabe 3 ---------------------------------------------------------------

### c)

T <- 100
t <- 1:T
u <- rnorm(T) 
c <- ts(3 + 0.4 * t + 0.3 * t^2 + u)
plot(c)
plot(diff(c))
plot(diff(c, difference = 2))
