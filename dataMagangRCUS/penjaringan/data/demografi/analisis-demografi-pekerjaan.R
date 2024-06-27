setwd("~/R/magangRCUS/penjaringan/data/demografi/per-kerjaan")
### data source: 

# PRE-ANALYSIS ----
## Import and Cleaning Data ---------------------------------
data2013 <- read_csv("penduduk_2013.csv")
data2014 <- read_csv("penduduk_2014.csv")
data2015 <- read_csv("penduduk_2015.csv")
data2016 <- read_csv("penduduk_2016.csv")
data2017 <- read_csv("penduduk_2017.csv")

## Matching column name ----
nama.kolom <- c("tahun", "provinsi", "kabupaten", "kecamatan", "kelurahan", "pekerjaan", "jumlah")
datalist <- list(data2013, data2014, data2015, data2016, data2017)

for (i in 1:length(datalist)) {
  colnames(datalist[[i]]) <- nama.kolom
}

## Joining data ----
df <- bind_rows(datalist)

df <- arrange(df, pekerjaan)
unique(df$pekerjaan)

## Cleaning Data ----
df$pekerjaan <- str_replace(df$pekerjaan, "DPR-RI", "DPR RI")
df$pekerjaan <- str_replace(df$pekerjaan, "DPRD Kabupaten Kota", "DPRD Kabupaten/Kota")
df$pekerjaan <- str_replace(df$pekerjaan, "Kabinet Menteri", "Kabinet/Kementerian")
df$pekerjaan <- str_replace(df$pekerjaan, "Kabinet Kementerian", "Kabinet/Kementerian")
df$pekerjaan <- str_replace(df$pekerjaan, " / ", "/")
df$pekerjaan <- str_replace(df$pekerjaan, "Belum/Tidak Bekerja", "Belum Bekerja/Tidak Bekerja")
df$pekerjaan <- str_replace(df$pekerjaan, "Imam Mesjid", "Imam Masjid")
df$pekerjaan <- str_replace(df$pekerjaan, "Buruh Nelayan Perikanan", "Buruh Nelayan/Perikanan")
df$pekerjaan <- str_replace(df$pekerjaan, "Buruh Perternakkan", "Buruh Peternakan")
df$pekerjaan <- str_replace(df$pekerjaan, "Buruh Tani Perkebunan", "Buruh Tani/Perkebunan")
df$pekerjaan <- str_replace(df$pekerjaan, "Penerjermah", "Penerjemah")
df$pekerjaan <- str_replace(df$pekerjaan, "Penterjemah", "Penerjemah")
df$pekerjaan <- str_replace(df$pekerjaan, "Tentara Negara Indonesia", "Tentara Nasional Indonesia")
df$pekerjaan <- str_replace(df$pekerjaan, "ustadz/mubaligh", "Ustadz/Mubaligh")



#EXPLORATORY DATA ANALYSIS ----
## Setup Variabel -------------------------
jumlahPerKerjaan <- df %>%
  group_by(tahun, pekerjaan) %>%
  summarize(jumlah = sum(jumlah, na.rm = TRUE)) %>%
  ungroup() 

singleyear <- jumlahPerKerjaan %>%
  group_by(pekerjaan) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  select(pekerjaan) %>%
  as_vector()

notSummary <- jumlahPerKerjaan %>%
  filter(tahun == 2015) %>%
  distinct(pekerjaan) %>%
  as_vector()

explorePerKerjaan <- jumlahPerKerjaan %>%
  filter(!(pekerjaan %in% singleyear) & !(pekerjaan %in% notSummary)) 


## Bagaimana erkembangan penduduk per jenis pekerjaan per tahun ----------------
explorePerKerjaan %>%
  ggplot(aes(tahun, jumlah)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~pekerjaan)


## Pekerjaan apa yang paling banyak dilakukan?--------------------------
terbanyakPerKerjaan <- explorePerKerjaan %>%
  group_by(pekerjaan) %>%
  summarize(avg_jumlah = floor(mean(jumlah))) %>%
  ungroup()

### Top 10
terbanyakPerKerjaan %>%
  arrange(desc(avg_jumlah)) %>%
  head(10)

### Grafik
terbanyakPerKerjaan %>%
  transform(pekerjaan=reorder(pekerjaan, avg_jumlah)) %>%
  ggplot(aes(pekerjaan, avg_jumlah)) +
  geom_col() +
  # Transform log 10
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Berapa persen Top 10 dibanding jumlah penduduk --------------------------
### jumlah penduduk 
aggPendKerja <- df %>%
  group_by(tahun) %>%
  summarize(jumlah_penduduk = sum(jumlah, na.rm = TRUE))


## Variasi spasial ---------------------------------
### Khusus karyawan swasta (karena paling banyak)
### Setup
swasta <- df %>%
  filter(pekerjaan == "Karyawan Swasta") %>%
  group_by(kabupaten, kecamatan, kelurahan) %>%
  summarize(avg_swasta = mean(jumlah, na.rm = TRUE)) %>%
  ungroup()
swasta$kelurahan <- str_replace(swasta$kelurahan, "P\\.", "PULAU")
swasta$kelurahan <- str_replace(swasta$kelurahan, "KUSUMAH", "KUSUMA")
swasta$kelurahan <- str_replace(swasta$kelurahan, "KERENDANG", "KRENDANG")
swasta$kelurahan <- str_replace(swasta$kelurahan, "BALI MESTER", "BALIMESTER")
swasta$kelurahan <- str_replace(swasta$kelurahan, "PAL MERIAM", "PALMERIAM")
swasta$kelurahan <- str_replace(swasta$kelurahan, "KAMPUNG TENGAH", "TENGAH")

### df
kelurahan
swasta

arrange(kelurahan, kelurahan)
arrange(swasta, kelurahan)
match(kelurahan$kelurahan, swasta$kelurahan)

#kenapa bisa ada 377 kelurahan di shp dan 257 kelurahan di dataset?
test1 <- kelurahan %>%
  anti_join(swasta, by = "kelurahan")
unique(test1$kelurahan)
#gimana cara ngambil vectornya aja?
test2 <- swasta %>%
  anti_join(kelurahan, by = "kelurahan")
unique(test2$kelurahan)

shpSwasta <- kelurahan %>%
  inner_join(swasta, by = "kelurahan")

shpSwasta %>%
  filter(grepl("JAKARTA", kabupaten)) %>%
  tm_shape() +
  tm_fill(col = "avg_swasta")