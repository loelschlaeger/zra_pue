
# Aufgabe 3 ---------------------------------------------------------------

### c)

T <- 1000
alpha_0 <- 1
alpha <- c(0.4, -0.3, 0.2, -0.1)
y <- numeric(T)

for (t in  5:T) {
  y[t] <- sum(alpha * y[(t-1):(t-4)]) + rnorm(1, mean = alpha_0)
}

y <- ts(y)
plot(y)

## Yule-Walker

ar.yw(y, aic = FALSE, order.max = 4, demean = TRUE)

## Regression

ar.ols(y, aic = FALSE, order.max = 4, demean = FALSE, intercept = TRUE)

## Alternative

arima(y, order = c(4, 0, 0))
