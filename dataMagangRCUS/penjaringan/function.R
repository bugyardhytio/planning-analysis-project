#-------------------------------
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