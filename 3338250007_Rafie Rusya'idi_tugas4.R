
# 1. DISTRIBUSI POISSON


lambda <- 3

P_X_ge_5 <- 1 - ppois(4, lambda)
cat("P(X >= 5) :\n", P_X_ge_5, "\n\n")

x_pois <- 0:15
pmf_pois <- dpois(x_pois, lambda)

plot(x_pois, pmf_pois,
     type = "h",
     lwd = 3,
     col = "blue",
     main = "Distribusi Poisson (λ = 3)",
     xlab = "k (Jumlah pelanggan)",
     ylab = "P(X = k)")


# 2. DISTRIBUSI HIPERGEOMETRIK


N <- 100       
K <- 20        
n_hyper <- 10  

k_hyper <- seq(from = max(0, n_hyper + K - N), to = min(n_hyper, K))

pmf_hyper <- dhyper(k_hyper, m = K, n = N - K, k = n_hyper)

df_hyper <- data.frame(k = k_hyper, Probabilitas = pmf_hyper)
cat("Tabel Probabilitas Hipergeometrik:\n")
print(df_hyper)
cat("\n")

plot(k_hyper, pmf_hyper,
     type = "h",
     lwd = 3,
     col = "red",
     main = paste0("Hipergeometrik (N=", N, ", K=", K, ", n=", n_hyper, ")"),
     xlab = "k (Banyak bola merah yang terambil)",
     ylab = "P(X = k)")

m_hyper <- 10000
samp_hyper <- rhyper(m_hyper, m = K, n = N - K, k = n_hyper)

cat("Rata-rata simulasi (Hipergeometrik) :", mean(samp_hyper), "\n")
cat("Rata-rata teoretis (Hipergeometrik) :", n_hyper * K / N, "\n\n")



# 3. DISTRIBUSI BINOMIAL


n_binom <- 15   
p_binom <- 0.4  
m_binom <- 1000
set.seed(2025)

samp_binom <- rbinom(m_binom, size = n_binom, prob = p_binom)

cat("10 Hasil Simulasi Binomial Pertama:\n")
print(head(samp_binom, 10))
cat("\n")

x_binom <- 0:n_binom
pmf_binom <- dbinom(x_binom, size = n_binom, prob = p_binom)

df_binom <- data.frame(x = x_binom, Probabilitas = pmf_binom)

hist(samp_binom,
     breaks = seq(-0.5, 15.5, by = 1),
     probability = TRUE,
     col = "lightgray",
     border = "white",
     main = "Simulasi Binomial vs PMF Teoretis",
     xlab = "Jumlah Sukses",
     ylab = "Probabilitas")

points(x_binom, pmf_binom, type = "h", lwd = 3, col = "darkgreen")
points(x_binom, pmf_binom, pch = 16, col = "darkgreen")