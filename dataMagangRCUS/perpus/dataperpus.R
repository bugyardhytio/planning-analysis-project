library(dplyr)
setwd("~/R/magangRCUS")

data <- read.csv("catalog-titles-1.csv")

# Rekap yang udah selesai
F2 <- read.csv("done/F2.csv")
is.na(F2$Foto)


# Split per Call Number
column <- c("A", "B", "C", "D", "E", "F", "G", "H")
row <- as.character(1:3)

for (i in column) {
  for (j in row) {
    cell <- paste(i, j, sep="")
    filename <- paste(cell,"csv", sep = ".")

    x <- data[which(data$Call.No==cell), ]
    
    # write.csv(x, filename)
  }
}