setwd("~/R/magangRCUS/penjaringan/data/demografi/per-agama")
### data source: 

# PRE-ANALYSIS ----
## Import and Cleaning Data ---------------------------------
data2013 <- read_csv("penduduk_2013.csv")
data2014 <- read_csv("penduduk_2014.csv")
data2015 <- read_csv("penduduk_2015.csv")
data2016 <- read_csv("penduduk_2016.csv")
data2017 <- read_csv("penduduk_2017.csv")

## Matching column name ----
nama.kolom <- c("tahun", "provinsi", "kabupaten", "kecamatan", "kelurahan", "agama", "jumlah")
datalist <- list(data2013, data2014, data2015, data2016, data2017)

for (i in 1:length(datalist)) {
  colnames(datalist[[i]]) <- nama.kolom
}

## Joining data ----
df <- bind_rows(datalist)

## Cleaning Data ----
df$agama <- str_replace(df$agama, "Katolik", "Katholik")
df$agama <- str_replace(df$agama, "Katolik", "Katholik")

#EXPLORATORY DATA ANALYSIS ----
## Jumlah penduduk per tahun -----------------
aggPendAgama <- df %>%
  group_by(tahun) %>%
  summarize(jumlah_penduduk = sum(jumlah, na.rm = TRUE)) %>%
  ungroup()


## Jumlah per agama rata-rata per tahun -------------------------
df %>%
  group_by(tahun, agama) %>%
  summarize(jumlah_penduduk = sum(jumlah, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(agama) %>%
  group_by(agama) %>%
  summarize(avg_penduduk = floor(mean(jumlah_penduduk))) %>%
  ungroup() %>%
  arrange(desc(avg_penduduk))