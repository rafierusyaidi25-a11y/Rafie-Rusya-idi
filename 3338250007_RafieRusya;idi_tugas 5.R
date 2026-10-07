
# SOAL 1
cat("--- SOAL 1 ---\n")
prob1_a <- pexp(5, rate = 1/5, lower.tail = FALSE)
prob1_b <- 1 - pexp(5, rate = 1/5)
cat("Peluang (lower.tail = FALSE):", prob1_a, "\n")
cat("Peluang (1 - pexp):", prob1_b, "\n")

x_dexp1 <- seq(1, 30, by = 1)
y_dexp1 <- dexp(x_dexp1, rate = 0.2)
plot(x_dexp1, y_dexp1, type = "l", col = "blue", lwd = 2,
     main = "PDF Distribusi Eksponensial (Lambda = 0.2)",
     xlab = "x", ylab = "f(x)")

# SOAL 2
set.seed(2025)
n2 <- 1000
a <- 0
b <- 20

var_teoritis <- (b - a)^2 / 12
cat("Varians waktu tunggu:", var_teoritis, "menit^2\n")


x2 <- runif(n2, min = a, max = b)

hist(x2, breaks = 30, probability = TRUE,
     main = "Histogram Sampel U(0,20) dengan PDF Teoritis",
     xlab = "Waktu Tunggu (menit)")
curve(dunif(x, min = a, max = b), from = a, to = b, add = TRUE, col = "red", lwd = 2)

# SOAL 3

prob3_a <- pexp(5, rate = 1/10, lower.tail = TRUE)
prob3_b <- 1 - pexp(5, rate = 1/10)
cat("Peluang P(X <= 5):", prob3_a, "\n")
cat("Peluang P(X > 5):", prob3_b, "\n")

x_dexp3 <- seq(1, 30, by = 1)
y_dexp3 <- dexp(x_dexp3, rate = 0.1)
plot(x_dexp3, y_dexp3, type = "l", col = "green", lwd = 2,
     main = "PDF Distribusi Eksponensial (Lambda = 0.1)",
     xlab = "x", ylab = "f(x)")


#SOAL 4
n4 <- 100
mu <- 250
sigma <- 5

x4 <- rnorm(n4, mean = mu, sd = sigma)
x_bar <- mean(x4)
cat("Mean Sampel (x_bar):", x_bar, "\n")

mle_sigma2 <- mean((x4 - x_bar)^2) 
cat("MLE Sigma^2:", mle_sigma2, "\n")

sd_sample <- sd(x4) 
cat("Standar Deviasi Sampel:", sd_sample, "\n")


hist(x4, breaks = 30, probability = TRUE,
     main = "Histogram sampel N(250, 5^2) dengan PDF teoritis",
     xlab = "Berat Bersih Kopi (gram)")
curve(dnorm(x, mean = mu, sd = sigma), from = mu - 4 * sigma, to = mu + 4 * sigma, add = TRUE, lwd = 2)
abline(v = x_bar, col = "blue", lwd = 2)       
abline(v = mu, col = "red", lwd = 2, lty = 2)  
legend("topright", legend = c("PDF teoritis", "mean sampel", "mean true"),
       lty = c(1, 1, 2), col = c("black", "blue", "red"), bty = "n")
