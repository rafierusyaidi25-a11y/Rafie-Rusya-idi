# Load dan Cek Data
data(airquality)
View(airquality)
str(airquality)

# 1. HISTOGRAM & DENSITY PLOT
# Histogram A
hist(airquality$Wind, 
     breaks = 1.5 + (0:4) * 5, 
     ylim = c(0, 35),
     col = "skyblue",
     xlab = "Wind (mph)", 
     main = "Histogram A: Breaks di 1.5, 6.5, ...")

# Histogram B
hist(airquality$Wind,
     breaks = c(0, 5, 10, 15, 20, 25),
     ylim = c(0, 35),
     labels = TRUE,
     col = "salmon",
     xlab = "Wind (mph)",
     main = "Histogram B: Breaks di 0, 5, 10, 15, 20, 25")

# Estimasi kepadatan
dens <- density(airquality$Wind, na.rm = TRUE)

# Histogram + Density Curve
hist(airquality$Wind, 
     breaks = 0 + (0:11) * 2, 
     probability = TRUE, 
     xlab = "Wind (mph)", 
     main = "Histogram + Density Curve")
lines(dens, col = "green", lwd = 2)

# Lattice Density Plot per Bulan
library(lattice)
densityplot(~ Wind | factor(Month), data = airquality,
            main = "Density Plot Wind Berdasarkan Bulan",
            xlab = "Wind",
            aspect = 1)


# 2. BOXPLOT
# Base R Boxplot
boxplot(airquality$Wind, horiz = TRUE, 
        main = "Boxplot Wind", 
        xlab = "Wind (mph)")

# Lattice Boxplot (Sudah Diperbaiki)
bwplot(~ Wind, data = airquality, 
       main = "Boxplot (lattice) Wind", 
       xlab = "Wind (mph)")



# 3. SCATTER PLOT
# Scatterplot dengan Rug Plot & Garis Acuan
xyrange <- range(c(airquality$Wind, airquality$Temp), na.rm = TRUE)

plot(Wind ~ Temp, data = airquality, 
     xlim = xyrange, ylim = xyrange, 
     pch = 16,
     main = "Scatterplot Wind terhadap Temp",
     xlab = "Temperature", 
     ylab = "Wind")
rug(airquality$Temp)
rug(airquality$Wind, side = 2)
abline(0, 1, col = "red", lwd = 2, lty = 2)

# Panel A: Skala Linear
plot(Wind ~ Temp, data = airquality, 
     xlab = "Temperature (°F)", 
     ylab = "Wind (mph)",
     main = "Scatterplot Wind terhadap Temp (Skala Linear)",
     pch = 16,
     col = "darkblue")

# Panel B: Skala Logaritmik
plot(log(Wind) ~ log(Temp), data = airquality,
     xlab = "log(Temperature)", 
     ylab = "log(Wind)",
     main = "Scatterplot Wind terhadap Temp (Skala Logaritmik)",
     pch = 16,
     col = "brown")