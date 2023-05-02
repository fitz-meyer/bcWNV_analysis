library(tidyverse)
library(magrittr)

column_classes <- c("c", "n")
setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/Sequencing/all_counts/stocks/true_counts/")
stock_file <- read_delim("estimated_input.txt",
                      col_types = column_classes, col_names = c("barcode", "count"))

setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/Sequencing/all_counts/whole_tissue/true_counts/")
mg_file <- read_delim("aea_mg_12dpi_5_S153_true_barcodes.txt", 
                 col_types = column_classes, col_names = c("barcode", "count"))

stock_total <- sum(stock_file$count)
mg_total <- sum(mg_file$count)

#create df w/ b1 prob (stock - mg)
temp1 <- stock_file %>%
  mutate(b1 = 1-(1-(stock_file$count/stock_total))^mg_total)
prob_df <- temp1 %>%
  mutate(b1 = sprintf(temp1$b1, fmt = '%#.12f')) %>%
  mutate(b1 = as.numeric(b1))

#make NA entries '0'
prob_df[is.na(prob_df)]<-0

setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/whole_tissue/probabilities/est_input/mg_infection_probabilities/")
write_csv(prob_df, "aea_12d_5_mg_infection_prob.csv")

setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/Sequencing/all_counts/whole_tissue/true_counts/")




