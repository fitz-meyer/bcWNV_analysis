library(tidyverse)
library(magrittr)

#read in files:
getwd()
setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/Sequencing/all_counts/stocks/true_counts/")
column_classes <- c("c", "n")

stock50 <- read_delim("bcWNV_P2_50_S309_true_barcodes.txt", 
                      col_types = column_classes, col_names = c("barcode", "count50"))
stock100 <- read_delim("bcWNV_P2_100_S310_true_barcodes.txt", 
                       col_types = column_classes, col_names = c("barcode", "count100"))
stock200 <- read_delim("bcWNV_P2_200_S311_true_barcodes.txt", 
                       col_types = column_classes, col_names = c("barcode", "count200"))
stock_merge <- read_delim("P2_merge_input.txt", 
                          col_types = column_classes, col_names = c("barcode", "count_merge"))

#generate barcode frequencies for each "replicate":

totalumis_s50 <- sum(stock50$count50)
stock50$freq_1_50 <- (stock50$count50)/totalumis_s50

totalumis_s100 <- sum(stock100$count100)
stock100$freq_2_100 <- (stock100$count100)/totalumis_s100

totalumis_s200 <- sum(stock200$count200)
stock200$freq_3_200 <- (stock200$count200)/totalumis_s200

totalumis_merge <- sum(stock_merge$count_merge)
stock_merge$freq_4_merge <- (stock_merge$count_merge)/totalumis_merge

#merge frequency tables (note: merge can only do 2 dfs at a time)

stock100_200 <- merge(stock100, stock200, by = "barcode", all.x = TRUE, all.y = TRUE) 
stock100_200_50 <- merge(stock100_200, stock50, by = "barcode", all.x = TRUE, all.y = TRUE)
stockvols <- merge(stock100_200_50, stock_merge, by = "barcode", all.x = TRUE, all.y = TRUE)

#make NA entries '0'
stockvols[is.na(stockvols)]<-0

#select columns of interest for graph data
graph_data <- stockvols %>%
  select(1, 3, 5, 7, 9) 

#arrange desc by freq_merge (or SL)
graph_data <- graph_data %>%
   arrange(desc(freq_4_merge))

#generate separate dataframes so each element of the X axis gets its own rank
sv50 <- graph_data[c("freq_1_50")]
sv50 <- sv50 %>%
   mutate(volume = 50) %>%
   mutate(barcode_id = 1:nrow(sv50)) %>%
   dplyr::rename("freq" = "freq_1_50")

sv100 <- graph_data[c("freq_2_100")]
sv100 <- sv100 %>%
   mutate(volume = 100) %>%
   mutate(barcode_id = 1:nrow(sv100)) %>%
   dplyr::rename("freq" = "freq_2_100")

sv200 <- graph_data[c("freq_3_200")]
sv200 <- sv200 %>%
   mutate(volume = 200) %>%
   mutate(barcode_id = 1:nrow(sv200)) %>%
   dplyr::rename("freq" = "freq_3_200")

sv_merge <- graph_data[c("freq_4_merge")]
sv_merge <- sv_merge %>%
  mutate(volume = "350") %>%
  mutate(barcode_id = 1:nrow(sv_merge)) %>%
  dplyr::rename("freq" = "freq_4_merge")

#bind these dataframes and arrange desc by barcode_id
frequencies <- rbind(sv50, sv100, sv200, sv_merge) %>%
   arrange(desc(barcode_id)) %>%
   mutate(barcode_id = as.factor(barcode_id)) %>%
   mutate(volume = as.numeric(volume)) %>%
   mutate(freq = as.numeric(freq))

#graph
x<-(rep(c("red","green","blue","yellow","purple","grey","orange","brown","turquoise","pink","cyan","darkgrey"),11000))

breaks <- c("50","100","200","merge")

ggplot(frequencies, aes(x = volume, y = freq, fill = barcode_id)) + 
   geom_area(alpha = 0.6, show.legend = FALSE) +
   theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) +
   theme(panel.grid.major.y = element_line(colour = "light grey"), panel.grid.major.x = element_blank(),
         panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
   theme(axis.line.x = element_blank(), axis.line.y = element_blank()) +
   theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
   theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
   ggtitle("bcWNV Stock Barcode Dynamics") +
   labs(x = "Stock Volume", y = "Barcode Frequency") +
   scale_x_continuous(breaks = c(0,1,2,3), labels = breaks) +
   scale_y_continuous(breaks = seq(0,1, by=0.1), expand = c(0,0)) +
   scale_fill_manual(values = x)


#add expand = c(0,0) to each scale_x/y_continuous function if you want to remove white space from edges of graph.







