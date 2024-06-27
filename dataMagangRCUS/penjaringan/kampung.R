library(readr)

getwd()

kampung <- read.csv("kampung/lokasiKampung")

count(kampung$Kotamadya)
table(kampung$Kotamadya, kampung$Kecamatan)
table(kampung$Kecamatan, kampung$Kotamadya)
table(kampung$Kecamatan, kampung$Kelurahan)
table(kampung$Kelurahan, kampung$Kecamatan)

levels(kampung$Kelurahan)
