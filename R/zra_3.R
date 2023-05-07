
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

T <- length(her_f_min)
t <- 1:T
fit1 <- lm(her_f_min ~ t)
fit2 <- lm(her_f_min ~ t + I(t^2))
fit3 <- lm(her_f_min ~ t + I(t^2) + I(t^3))
fit4 <- lm(her_f_min ~ t + I(t^2) + I(t^3) + I(t^4))
fit5 <- lm(her_f_min ~ t + I(t^2) + I(t^3) + I(t^4) + I(t^5))

par(mfrow=c(2,3), mar=c(2.1,4.1,3.1,2.1))
plot.ts(her_f_min_ts, lwd=2, col="black",
        ylab="Bestzeit (Min)",
        main="Bestzeiten beim Hermannnslauf 1972-2019")
legend("topright", legend=c("ZR", "K=1","K=2","K=3","K=4","K=5"),
       lwd=2, lty=1, bty="n")
points(t,fit1$fitted,type="l", col="orange", lwd=2)
points(z1,fit2$fitted,type="l", col="lightblue", lwd=2)
points(z1,fit3$fitted,type="l", col="darkgreen", lwd=2)
points(z1,fit4$fitted,type="l", col="purple", lwd=2)
points(z1,fit5$fitted,type="l", col="brown", lwd=2)
plot.ts(fm, lwd=2, col="black",
        ylab="Bestzeit (Min)", main="K=1")
points(z1,fit1$fitted,type="l", col="orange", lwd=2)
plot.ts(fm, lwd=2, col="black",
        ylab="Bestzeit (Min)", main="K=2")
points(z1,fit2$fitted,type="l", col="lightblue", lwd=2)
plot.ts(fm, lwd=2, col="black",
        ylab="Bestzeit (Min)", main="K=3")
points(z1,fit3$fitted,type="l", col="darkgreen", lwd=2)
plot.ts(fm, lwd=2, col="black",
        ylab="Bestzeit (Min)", main="K=4")
points(z1,fit4$fitted,type="l", col="purple", lwd=2)
plot.ts(fm, lwd=2, col="black",
        ylab="Bestzeit (Min)", main="K=5")
points(z1,fit5$fitted,type="l", col="brown", lwd=2)
par(mfrow=c(1,1))

fit8 <- lm(fm ~ z1 + z2 + z3 + z4 + z5 + I(z1^6) + I(z1^7) + I(z1^8))
plot.ts(fm, lwd=2, col="black",
        ylab="Bestzeit (Min)", main="Bestzeiten beim Hermannnslauf 1972-2019")
points(z1,fit8$fitted,type="l", col="brown", lwd=2)

# c) Testabfolgen
# F-Tests
N <- length(fm)
K_ur <- 5
SSR_ur <- sum(fit5[["residuals"]]^2)

# aufsteigend
# H0: K_r = 0 vs H1: K_r > 0 d.h. beta_j = 0, j > 0, vs. beta_j != 0, j in {1,2,3,4,5}
fit0 <- lm(fm ~ 1)
K_r <- 0
SSR_r <- sum(fit0[["residuals"]]^2)
Fstat <- ((SSR_r - SSR_ur)/(K_ur - K_r))/((SSR_ur)/(N - K_ur - 1))
(p <- 1 - pf(Fstat, K_ur - K_r, N - K_ur - 1))

# H0: K_r = 1 vs H1: K_r > 1 d.h. beta_j = 0, j > 1, vs. beta_j != 0, j in {2,3,4,5}
K_r <- 1
SSR_r <- sum(fit1[["residuals"]]^2)
Fstat <- ((SSR_r - SSR_ur)/(K_ur - K_r))/((SSR_ur)/(N - K_ur - 1))
(p <- 1 - pf(Fstat, K_ur - K_r, N - K_ur - 1))

# H0: K_r = 2 vs H1: K_r > 1 d.h. beta_j = 0, j > 2, vs. beta_j != 0, j in {3,4,5}
K_r <- 2
SSR_r <- sum(fit2[["residuals"]]^2)
Fstat <- ((SSR_r - SSR_ur)/(K_ur - K_r))/((SSR_ur)/(N - K_ur - 1))
(p <- 1 - pf(Fstat, K_ur - K_r, N - K_ur - 1))

# H0: K_r = 3 vs H1: K_r > 3 d.h. beta_j = 0, j > 3, vs. beta_j != 0, j in {4,5}
K_r <- 3
SSR_r <- sum(fit3[["residuals"]]^2)
Fstat <- ((SSR_r - SSR_ur)/(K_ur - K_r))/((SSR_ur)/(N - K_ur - 1))
(p <- 1 - pf(Fstat, K_ur - K_r, N - K_ur - 1))

# H0: K_r = 4 vs H1: K_r = 5 d.h. beta_5 = 0 vs. beta_5 != 0
K_r <- 4
SSR_r <- sum(fit4[["residuals"]]^2)
Fstat <- ((SSR_r - SSR_ur)/(K_ur - K_r))/((SSR_ur)/(N - K_ur - 1))
(p <- 1 - pf(Fstat, K_ur - K_r, N - K_ur - 1))
# Entscheidung fuer fit5

# absteigend
# H0: K_r = 4 vs H1: K_r = 5 d.h. beta_5 = 0 vs. beta_5 != 0
K_r <- 4
SSR_r <- sum(fit4[["residuals"]]^2)
Fstat <- ((SSR_r - SSR_ur)/(K_ur - K_r))/((SSR_ur)/(N - K_ur - 1))
(p <- 1 - pf(Fstat, K_ur - K_r, N - K_ur - 1))
# Entscheidung fuer fit5

# d) Modellselektion
AIC.calc <- function(SSR,K,N){
  aic <- log(SSR/N) + 2*(K + 1)/N
  return(aic)
}
BIC.calc <- function(SSR,K,N){
  bic <- log(SSR/N) + (K + 1) * log(N)/N
  return(bic)
}

AICs <- numeric(5)
BICs <- numeric(5)
R2adjs <- numeric(5)

fitList <- list(fit1, fit2, fit3, fit4, fit5)
N <- length(fm)

for (i in 1:5){
  fit <- fitList[[i]]
  SSR <- sum(fit[["residuals"]]^2)
  K <- length(fit[["coefficients"]]) - 1
  AICs[i] <- AIC.calc(SSR, K, N)
  BICs[i] <- BIC.calc(SSR, K, N)
  R2adjs[i] <- summary(fit)[["adj.r.squared"]]
}

tab <- cbind(AICs, BICs, R2adjs)
tab <- round(tab, 3)
rownames(tab) <- paste0("K=", 1:5)
colnames(tab) <- c("AIC", "BIC", "R2adj")
tab



# Aufgabe 3 ---------------------------------------------------------------

### c)

T <- 100
t <- 1:T
u <- rnorm(T) 
c <- ts(3 + 0.4*t + 0.3*t^2 + u)
plot(c)
plot(diff(c))
plot(diff(c, difference = 2))
