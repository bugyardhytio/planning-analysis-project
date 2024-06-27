library(dplyr)

setwd("~/R/magangRCUS/penjaringan/penduduk/per.warganegara-jk")

data <- read.csv("penduduk_2013.csv")
kecamatan <- filter(data, nama.kecamatan == "PENJARINGAN")
 
nama.kolom <- names(data)
nama.kolom[7] <- "jenis.kelamin"
  
# Kalau datanya time series

tahun <- c("2013", "2014", "2015", "2016", "2017")

for (i in tahun) {
  file <- paste("bykerjaan", tahun, sep = "_")
  filename <- paste(file, "csv", sep = ".")
}

penjaringan <- data.frame()

for (i in 1:length(filename)) {
  data <- read.csv(filename[5])
  colnames(data) <- nama.kolom
  data <- filter(data, nama_kelurahan == "PENJARINGAN")
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

jenis_pekerjaan <- data.frame()
for (i in tahun) {
  jeniskerja <- penjaringan %>%
    filter(tahun == i) %>%
    select(jenis_pekerjaan)
  colnames(jeniskerja) <- i
  print(jeniskerja)
}

colnames(jenis)

#---------------------------------------------------

# Comment disini
pekerjapertahun <- function(dataset, padatahun) {
  jumlahpekerja <- 0
  jumlahpekerja <- filter(dataset, tahun == padatahun)
  jumlahpekerja <- as.vector(jumlahpekerja$jumlah)
  
  namafile <- paste0("pekerja", padatahun, ".csv")
  
  write.csv(jumlahpekerja, namafile)
}


# ----------------------------------------------------

write.csv(penjaringan, file = "bykerjaan.csv")





#---------------------------------------------------
names(data)
str(data)

jenis_pekerjaan <- data.frame()
for (i in tahun) {
  jeniskerja <- penjaringan %>%
    filter(tahun == i) %>%
    select(jenis_pekerjaan)
  colnames(jeniskerja) <- i
  print(jeniskerja)
}

colnames(jenis)

#---------------------------------------------------

# Comment disini
pekerjapertahun <- function(dataset, padatahun) {
  jumlahpekerja <- 0
  jumlahpekerja <- filter(dataset, tahun == padatahun)
  jumlahpekerja <- as.vector(jumlahpekerja$jumlah)
  
  namafile <- paste0("pekerja", padatahun, ".csv")
  
  write.csv(jumlahpekerja, namafile)
}


setwd("~/R/magangRCUS/penjaringan/penduduk/per.kerjaan")

kerjaan2016 <- read.csv("bykerjaan_2016.csv")
kerjaan2016$jumlah <- as.numeric(kerjaan2016$jumlah)
unique(kerjaan2016$jenis_pekerjaan)

kerjaan2017 <- read.csv("bykerjaan_2017.csv")
kerjaan2017$jumlah <- as.numeric(kerjaan2017$jumlah)
unique(kerjaan2017$jenis_pekerjaan)
