suppressPackageStartupMessages(library(tidyverse))

df <- read.table("./int_file.csv", col.names = "reads")

# S SEGMENT
barcodes <- str_match_all(df, "ACCCTCAATACCTGAAGAAG\\s*(.*?)\\s*GATAAGTGGATGTCACAGAA")  %>%
as.data.frame() %>%
  select(2)

barcodes <- barcodes[nchar(barcodes$X2) == 33, ]
barcodes <- as.data.frame(barcodes)
colnames(barcodes) <- NULL

# L SEGMENT
# barcodes <- str_match_all(df, "ATTTAAAATTAATACATCAT\\s*(.*?)\\s*TATAGTAAAAATAATCCTGA") %>%
#   as.data.frame() %>%
#   select(2)
# 
# barcodes <- barcodes[nchar(barcodes$X2) == 24, ]
# barcodes <- as.data.frame(barcodes)
# colnames(barcodes) <- NULL

write.csv(barcodes, "./barcodes.csv", row.names = FALSE, quote = FALSE)
