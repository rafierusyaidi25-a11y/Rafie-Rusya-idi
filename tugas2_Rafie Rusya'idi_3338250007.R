
library(dplyr)
#Masukkan data iris di R
iris

#Tampilkan data Sepal.Length saja
iris$Sepal.Length

#Sebutkan tipe data tiap kolom
sapply(iris, class)

#Membuat variabel baru(turunan),dengan nilai (besar/kecil) 
iris <- iris %>%
  mutate(turunan = ifelse(Sepal.Width > 3, "Besar", "Kecil")
  )
iris

#Ubah variabel turunan menjadi sepal
iris <- iris %>%
rename(sepal = turunan)
iris

#Ambil data dengan sepal bernilai besar dari species virginica
virginica_besar <- iris %>%
  filter(sepal == "Besar", Species == "virginica")

virginica_besar

#Cek jumlah species dalam data
table(iris$Species)

#Pecah data iris menjadi 3 data frame dengan tiap data frame khusus untuk species tertentu
df_setosa <- filter(iris, Species == "setosa")
df_setosa
df_versicolor <- filter(iris, Species == "versicolor")
df_versicolor
df_virginica <- filter(iris, Species == "virginica")
df_virginica

#Dari setiap data frame species, urutkan data berdasarkan Sepal.Width
urutkan_setosa <- arrange(df_setosa, Sepal.Width)
urutkan_setosa
urutkan_versicolor <- arrange(df_versicolor, Sepal.Width)
urutkan_versicolor
urutkan_virginica <- arrange(df_virginica, Sepal.Width)
urutkan_virginica