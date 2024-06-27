setwd("~/R/magangRCUS/penjaringan/data/demografi/per-usia")

# PRE-ANALYSIS ----
## Import and Cleaning Data ----
data2013 <- read_csv("penduduk_2013.csv")
data2014 <- read_csv("penduduk_2014.csv")
data2015 <- read_csv("penduduk_2015.csv")
data2016 <- read_csv("penduduk_2016.csv")
data2017 <- read_csv("penduduk_2017.csv")

## Matching Column Name ----
nama.kolom <- c("tahun", "provinsi", "kabupaten", "kecamatan", "kelurahan", "usia", "jenisKelamin", "jumlah")
datalist <- list(data2013, data2014, data2015, data2016, data2017)

for (i in 1:length(datalist)) {
  colnames(datalist[[i]]) <- nama.kolom
}

## Joining data ----
df <- bind_rows(datalist)

## Cleaning Data ----
df <- na.omit(df)
df$provinsi <- str_replace(df$provinsi, "PROVINSI DKI JAKARTA", "DKI Jakarta")
df$usia <- str_replace(df$usia, "9-May", "05-09")
df$usia <- str_replace(df$usia, "14-Oct", "10-14")
df$usia <- str_replace(df$usia, "70-75", "70-74")
df$usia <- str_replace(df$usia, ">75", "75++")
df$jenisKelamin <- str_replace(df$jenisKelamin, "Laki laki", "Pria")
df$jenisKelamin <- str_replace(df$jenisKelamin, "Laki-laki", "Pria")
df$jenisKelamin <- str_replace(df$jenisKelamin, "Laki-Laki", "Pria")
df$jenisKelamin <- str_replace(df$jenisKelamin, "Perempuan", "Wanita")
df$kabupaten <- str_replace(df$kabupaten, " ", ".")
df$kelurahan <- str_replace(df$kelurahan, "P\\. ", "PULAU ")
df$kelurahan <- str_replace(df$kelurahan, "KUSUMAH", "KUSUMA")
df$kelurahan <- str_replace(df$kelurahan, "KERENDANG", "KRENDANG")
df$kelurahan <- str_replace(df$kelurahan, "BALI MESTER", "BALIMESTER")
df$kelurahan <- str_replace(df$kelurahan, "PAL MERIAM", "PALMERIAM")
df$kelurahan <- str_replace(df$kelurahan, "KAMPUNG TENGAH", "TENGAH")

dfKec <- df %>%
  select(kelurahan, kecamatan) %>%
  distinct(kelurahan, kecamatan)
dfKab <- df %>%
  select(kelurahan, kabupaten) %>%
  distinct(kelurahan, kabupaten)
df2017 <- df %>%
  filter(tahun == 2017)


# EXPLORATORY DATA ANALYSIS ----
## Setup Variabel ----
### Filter kecamatan penjaringan aja
kecPenjaringan <- df %>%
  filter(kecamatan == "PENJARINGAN")

### filter kelurahan penjaringan aja
kelPenjaringan <- kecPenjaringan %>%
  filter(kelurahan == "PENJARINGAN")


## Jumlah penduduk ----
### per tahun ----
aggPendUsia <- df %>% 
  group_by(tahun) %>%
  summarize(jumlahPenduduk = sum(jumlah))

aggPendUsia %>%
  ggplot(aes(tahun, jumlahPenduduk)) +
  geom_line() +
  ylim(0, 10400000)


### per tahun per jenis kelamin ----
aggPendUsiaJK <- df %>%
  group_by(tahun, jenisKelamin) %>%
  summarize(jumlahPenduduk = sum(jumlah))

aggPendUsiaJK %>%
  ggplot(aes(tahun, jumlahPenduduk, col = jenisKelamin)) +
  geom_line() +
  ylim(0, 5300000)

### per tahun per kelurahan ----
df %>%
  group_by(tahun, kelurahan) %>%
  summarize(jumlahPenduduk = sum(jumlah)) %>%
  ggplot(aes(tahun, jumlahPenduduk, fill = kabupaten)) +
  geom_col(position = "fill")

df %>%
  group_by(tahun, kabupaten) %>%
  summarize(jumlahPenduduk = sum(jumlah)) %>%
  ggplot(aes(kabupaten, jumlahPenduduk, fill = tahun)) +
  geom_col(position = "fill")

df %>%
  group_by(tahun, kabupaten) %>%
  summarize(jumlahPenduduk = sum(jumlah)) %>%
  spread(kabupaten, jumlahPenduduk)


# Jumlah penduduk per kelurahan per tahun ----
pendudukKel <- df %>%
  group_by(tahun, kelurahan, kecamatan, kabupaten) %>%
  summarize(jumlahPenduduk = sum(jumlah)) 

pendudukKel2017 <- pendudukKel %>%
  filter(tahun == 2017) %>%
  arrange(jumlahPenduduk)


## Rasio ketergantungan ----
### Bagaimana posisi dependency ratio kelurahan penjaringan dibanding kelurahan-kelurahan lain
depRatio <- df %>%
  mutate(usiaProduktif = ifelse(usia %in% c("0-4", "5-9", "10-14", "65-69", "70-74", ">75"), "Tidak", "Ya")) %>%
  group_by(usiaProduktif, tahun) %>%
  summarize(n = sum(jumlah)) %>%
  ungroup() %>%
  spread(usiaProduktif, n) %>%
  mutate(dependencyRat = Tidak / Ya * 100) %>%
  select(tahun, dependencyRat)

depRatioKelurahan <- df %>%
  mutate(usiaProduktif = ifelse(usia %in% c("0-4", "5-9", "10-14", "65-69", "70-74", ">75"), "Tidak", "Ya")) %>%
  group_by(usiaProduktif, kelurahan) %>%
  summarize(n = sum(jumlah)) %>%
  ungroup() %>%
  spread(usiaProduktif, n) %>%
  mutate(dependencyRat = Tidak / Ya * 100) %>%
  select(kelurahan, dependencyRat) %>%
  arrange(dependencyRat)

### histogram
hist(depRatioKelurahan$dependencyRat)


### peta ----
shpDepRatio <- inner_join(kelurahan, depRatioKelurahan, by = "kelurahan")
shpDepRatio <- inner_join(shpDepRatio, dfKec, by = "kelurahan")
shpDepRatio <- inner_join(shpDepRatio, dfKab, by = "kelurahan")
shpDepRatio <- select(shpDepRatio, kelurahan, kecamatan, kabupaten, dependencyRat, geometry)

shpDepRatio %>%
  filter(grepl("JAKARTA", kabupaten)) %>%
  tm_shape() +
  tm_fill(col = "dependencyRat")


## Piramida Penduduk ----
dfPyramid <- df %>%
  filter(jenisKelamin == "Pria") %>%
  mutate(jumlah = -jumlah) %>%
  rbind(subset(df, jenisKelamin == "Wanita"))

ggplot(dfPyramid, aes(x = usia, y = jumlah, fill = jenisKelamin)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

## Sex Ratio ----
df %>%
  spread(jenisKelamin, jumlah) %>%
  group_by(tahun) %>%
  summarize(aggPria = as.numeric(sum(Pria)), aggWanita = as.numeric(sum(Wanita))) %>%
  ungroup() %>%
  mutate(sexRatio = aggPria / aggWanita)


# Jumlah penduduk per kelurahan per tahun
shpPendKel <- inner_join(kelurahan, pendudukKel, by = "kelurahan")
shpPendKel <- select(shpPendKel, tahun, kelurahan, kecamatan, kabupaten, jumlahPenduduk, geometry)

shpPendKel %>%
  filter(grepl("JAKARTA", kabupaten)) %>%
  tm_shape() +
  tm_fill(col = "jumlahPenduduk") +
  tm_facets("tahun")

purple <- list(color = brewer.pal(3, "Purples"))