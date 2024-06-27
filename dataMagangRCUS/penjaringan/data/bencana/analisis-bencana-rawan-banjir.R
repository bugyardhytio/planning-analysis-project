setwd("~/R/magangRCUS/penjaringan/data/bencana/rw-rawan-banjir")

banjir <- read_csv("banjir.csv")
colnames(banjir) <- c("kabupaten", "kecamatan", "kelurahan", "rw")
rwBanjir <- inner_join(rw, banjir, by = c("kelurahan", "rw"))

tm_shape(rw) +
  tm_polygons() +
tm_shape(rwBanjir) +
  tm_fill("blue")