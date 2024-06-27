lokasi <- geocode("Kelurahan Penjaringan, Kecamatan Penjaringan, Jakarta Utara, DKI Jakarta", source = "dsk")
peta_penjaringan <- get_map(location = lokasi, zoom = "auto", source = "osm")

setwd("~/R/magangRCUS/penjaringan/shapefile/batas-administrasi/")

kecamatan <- st_read("batas_kecamatan.shp")
kelurahan <- st_read("batas_kelurahan.shp")
rw <- st_read("batas_rw.shp")
  
# DATA CLEANING ----
## Cleaning shp kelurahan ----
kelurahan <- select(kelurahan, keluraha_1, geometry)
colnames(kelurahan) <- c("kelurahan", "geometry")
kelurahan$kelurahan <- toupper(kelurahan$kelurahan)
kelurahan$kelurahan <- str_replace(kelurahan$kelurahan, "KALI DERES", "KALIDERES")
kelurahan$kelurahan <- str_replace(kelurahan$kelurahan, "PAL MERAH", "PALMERAH")
kelurahan$kelurahan <- str_replace(kelurahan$kelurahan, "HARAPAN MULYA", "HARAPAN MULIA")
kelurahan$kelurahan <- str_replace(kelurahan$kelurahan, "KALIANYAR", "KALI ANYAR")

## Cleaning shp rw
rw <- select(rw, rw_name, keluraha_1, kecamata_1, kabupate_1, geometry)
colnames(rw) <- c("rw", "kelurahan", "kecamatan", "kabupaten", "geometry")
rw$kelurahan <- toupper(rw$kelurahan)