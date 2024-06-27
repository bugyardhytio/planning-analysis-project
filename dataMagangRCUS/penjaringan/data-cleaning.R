library(dplyr)
library(readr)
library(stringr)

### Input aspek dan pembagiannya yang diinginkan (demografi/demografi-ext)
aspek <- c("demografi", "demografi-ext")
variabel.demografi <- c("usia", "warganegara", "kerjaan", "agama")
variabel.demoext <- c("kepadatan", "kk")

if (aspek == "demografi") {
  variabel <- variabel.demografi
} else {
  variabel <- variabel.demoext
}

workdir <- paste("~/R/magangRCUS/penjaringan", "data", aspek[i], variabel[j], sep = "/")

setwd(workdir)

# Import and Cleaning Data ---------------------------------

data2013 <- read_csv("byusia_2013.csv")
data2014 <- read_csv("byusia_2014.csv")
data2015 <- read_csv("byusia_2015.csv")
data2016 <- read_csv("byusia_2016.csv")
data2017 <- read_csv("byusia_2017.csv")

## Matching column name
nama.kolom <- c("tahun", "provinsi", "kabupaten", "kecamatan", "kelurahan", "usia", "jenisKelamin", "jumlah")
datalist <- list(data2013, data2014, data2015, data2016, data2017)

for (i in 1:length(datalist)) {
  colnames(datalist[[i]]) <- nama.kolom
}

## Joining data
df <- bind_rows(datalist)

## Cleaning Data
df <- na.omit(df)
df$provinsi <- str_replace(df$provinsi, "DKI Jakarta", "PROVINSI DKI JAKARTA")
df$usia <- str_replace(df$usia, "9-May", "05-09")
df$usia <- str_replace(df$usia, "14-Oct", "10-14")
df$usia <- str_replace(df$usia, "70-75", "70-74")
df$jenisKelamin <- str_replace(df$jenisKelamin, "Laki laki", "Laki-Laki")
df$jenisKelamin <- str_replace(df$jenisKelamin, "Laki-laki", "Laki-Laki")

any(is.na(df))
str(as_data_frame(df))


# ------------------
n_of_category <- function(dataset) {
  dataset %>%
    summarize(n_tahun = n_distinct(tahun),
              n_provinsi = n_distinct(provinsi),
              n_kabupaten = n_distinct(kabupaten),
              n_kecamatan = n_distinct(kecamatan),
              n_kelurahan = n_distinct(kelurahan),
              n_usia = n_distinct(usia),
              n_jenisKelamin = n_distinct(jenisKelamin))
}

n_of_category(df)
any(is.na(df$jumlah))
