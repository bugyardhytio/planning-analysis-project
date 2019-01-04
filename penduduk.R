library(dplyr)

setwd("~/R/magangRCUS/penjaringan/penduduk")

data <- read.csv("penduduk_2013.csv")
penjaringan_2013 <- filter(data, nama.kelurahan == "PENJARINGAN")

data <- read.csv("penduduk_2014.csv")
penjaringan_2014 <- filter(data, nama_kelurahan == "PENJARINGAN")

data <- read.csv("penduduk_2015.csv")
penjaringan_2015 <- filter(data, nama_kelurahan == "PENJARINGAN")

data <- read.csv("penduduk_2016.csv")
penjaringan_2016 <- filter(data, nama_kelurahan == "PENJARINGAN")

data <- read.csv("penduduk_2017.csv")
penjaringan_2017 <- filter(data, kelurahan == "PENJARINGAN")

penjaringan <- data.frame()


# Pindah ke kelompok usia, subset

setwd("~/R/magangRCUS/penjaringan/penduduk/usia")

data <- read.csv("byusia_2013.csv")
penj2013 <- filter(data, nama_kelurahan == "PENJARINGAN")

data <- read.csv("byusia_2014.csv")
penj2014 <- filter(data, nama_kelurahan == "PENJARINGAN")

data <- read.csv("byusia_2015.csv")
penj2015 <- filter(data, nama_kelurahan == "PENJARINGAN")

data <- read.csv("byusia_2016.csv")
penj2016 <- filter(data, nama_kelurahan == "PENJARINGAN")

data <- read.csv("byusia_2017.csv")
penj2017 <- filter(data, nama_kelurahan == "PENJARINGAN")

penjaringan[1] <- penj2013[6]


# Pindah ke kelompok usia, subset

setwd("~/R/magangRCUS/penjaringan/penduduk/per.agama")

data <- read.csv("byagama_2013.csv")
penj2013 <- filter(data, kelurahan == "PENJARINGAN")

data <- read.csv("byagama_2014.csv")
penj2014 <- filter(data, kelurahan == "PENJARINGAN")

data <- read.csv("byagama_2015.csv")
penj2015 <- filter(data, kelurahan == "PENJARINGAN")

data <- read.csv("byagama_2016.csv")
penj2016 <- filter(data, nama_kelurahan == "PENJARINGAN")

data <- read.csv("byagama_2017.csv")
penj2017 <- filter(data, kelurahan == "PENJARINGAN")


# Pindah ke kelompok usia, subset

setwd("~/R/magangRCUS/penjaringan/penduduk/per.kerjaan")

data <- read.csv("bykerjaan_2013.csv")
nama.kolom <- names(data)
  
# Kalau datanya time series

tahun <- c("2013", "2014", "2015", "2016", "2017")

for (i in tahun) {
  file <- paste("bykerjaan", tahun, sep = "_")
  filename <- paste(file, "csv", sep = ".")
}

penjaringan <- data.frame()

for (i in 1:length(filename)) {
  data <- read.csv(filename[i])
  data <- filter(data, nama_kelurahan == "PENJARINGAN")
  colnames(data) <- nama.kolom
  penjaringan <- rbind(penjaringan, data)
}

# Cek 
unique(penjaringan[1])

penjaringan %>%
  group_by(tahun, jenis_pekerjaan)

write.csv(penjaringan, file = "bykerjaan.csv")

#---------------------------------------------------
names(data)
str(data)

