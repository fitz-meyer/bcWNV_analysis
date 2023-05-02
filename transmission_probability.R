library(tidyverse)
library(magrittr)

column_classes <- c("c", "n")
setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/Sequencing/all_counts/stocks/true_counts/")
stock_file <- read_delim("estimated_input.txt",
                      col_types = column_classes, col_names = c("barcode", "count"))

setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/Sequencing/all_counts/whole_tissue/true_counts/")
sl_file <- read_delim("aea_sl_12dpi_5_S173_true_barcodes.txt", 
                      col_types = column_classes, col_names = c("barcode", "count"))
sg_file <- read_delim("aea_sg_12dpi_5_S163_true_barcodes.txt", 
                      col_types = column_classes, col_names = c("barcode", "count"))
mg_file <- read_delim("aea_mg_12dpi_5_S153_true_barcodes.txt", 
                 col_types = column_classes, col_names = c("barcode", "count"))

stock_total <- sum(stock_file$count)
mg_total <- sum(mg_file$count)
sg_total <- sum(sg_file$count)
sl_total <- sum(sl_file$count)

#test
#b1 <- 1-(1-(stock_file$count/stock_total))^mg_total
#b2 <- 1-(1-(mg_file$count/mg_total))^sg_total
#b3 <- 1-(1-(sg_file$count/sg_total))^sl_total

#create df w/ b1 prob (stock - mg)
temp1 <- stock_file %>%
  mutate(b1 = 1-(1-(stock_file$count/stock_total))^mg_total)
temp1 <- temp1 %>%
  mutate(b1 = sprintf(temp1$b1, fmt = '%#.12f'))

#create df w/ b2 prob (mg - sg)
temp2 <- mg_file %>%
  mutate(b2 = 1-(1-(mg_file$count/mg_total))^sg_total)
temp2 <- temp2 %>%
  mutate(b2 = sprintf(temp2$b2, fmt = '%#.12f'))

#create df w/ b3 prob (sg - sl)
temp3 <- sg_file %>%
  mutate(b3 = 1-(1-(sg_file$count/sg_total))^sl_total)
temp3 <- temp3 %>%
  mutate(b3 = sprintf(temp3$b3, fmt = '%#.12f'))

#merge all dfs and select for only barcode, b1, b2, and b3 prob values
#retain only stock counts - want for comparing starting freq w/ survival prob later on.

df_list <- list(temp1, temp2, temp3) %>%
  reduce(full_join, by = 'barcode') %>%
  arrange(desc(count.x)) %>%
  select(1, 2, 3, 5, 7) %>%
  mutate(b1 = as.numeric(b1)) %>%
  mutate(b2 = as.numeric(b2)) %>%
  mutate(b3 = as.numeric(b3))

#make NA entries '0'
df_list[is.na(df_list)]<-0

#multiply probabilities:
prob_df <- df_list %>%
  #mutate(b1xb2 = b1*b2) %>% 
  #mutate(b2xb3 = b2*b3) %>%
  mutate(b1xb2xb3 = b1*b2*b3)
prob_df <- prob_df %>%
  mutate(b1xb2xb3 = sprintf(prob_df$b1xb2xb3, fmt = '%#.12f')) 

setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/whole_tissue/probabilities/est_input/transmission_probabilities/")
write_csv(prob_df, "aea_12d_5_transmission_prob.csv")




