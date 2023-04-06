
# Aufgabe 1 ---------------------------------------------------------------

### a)

x <- c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y <- c(8.1, 6.9, 7.5, 8.8, 8.3, 9.9, 7.2, 4.2, 10.8, 4.8, 5.6)

### b)

x + y
x - y
x * y
x / y
x %*% y
t(x) %*% y
x %*% t(y)
t(x) %*% t(y)

### c)

plot(x, y, main = "Streudiagramm")

### d)

data <- data.frame(x = x, y = y)

### e)

model <- lm(formula = y ~ x, data = data)
abline(model)

### f)

summary(model)

