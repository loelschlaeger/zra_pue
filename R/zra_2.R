
# Aufgabe 1 ---------------------------------------------------------------

### a)

btc <- fHMM::download_data("BTC-USD", from = "2022-01-01", to = "2022-12-31")
eth <- fHMM::download_data("ETH-USD", from = "2022-01-01", to = "2022-12-31")
cov(btc$Close, eth$Close)
cor(btc$Close, eth$Close)

### d)

x <- 1:10
y <- x
cor(x, y)
y <- -x
cor(x, y)
y <- (x - mean(x))^2
cor(x, y)

# Aufgabe 2 ---------------------------------------------------------------

### a)

pbinom(13, size = 100, prob = 0.2)
pbinom(14, size = 100, prob = 0.2)

binom.test(13, n = 100, p = 0.2, alternative = "less")
binom.test(14, n = 100, p = 0.2, alternative = "less")

### b)

f.x <- function(x, p) dbinom(x, size = 100, prob = p)
plot(0:100, f.x(x = 0:100, p = 0.2), type = "o", xlab = "x", ylab = "f.x")
lines(0:13, f.x(x = 0:13, p = 0.2), type = "o", col = "red")
lines(0:100, f.x(x = 0:100, p = 0.1), type = "o", col = "blue")
abline(v = 14)
1 - pbinom(13, size = 100, prob = 0.1)

### c)

pbinom(15, size = 100, prob = 0.2)
1 - pbinom(15, size = 100, prob = 0.1)

n_range <- 1:1000
crit <- qbinom(0.05, size = n, prob = 0.2)
beta <- 1 - pbinom(crit - 1, size = n, prob = 0.1)
plot(n, beta, type = "l")
abline(h = 0.05)
min(which(beta < 0.05))

