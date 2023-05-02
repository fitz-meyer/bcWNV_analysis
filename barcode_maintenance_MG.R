library(tidyverse)
library(magrittr)

#set working directory and read in files:
#change path to location of files.
#sample files
getwd()
setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/Sequencing/all_counts/whole_tissue/true_counts/")
column_classes <- c("c", "n")

#read in all MG, or SG, or SL samples
MG1 <- read_delim("cxt_mg_12dpi_1_S62_true_barcodes.txt", 
                 col_types = column_classes, col_names = c("barcode", "countMG1"))
MG2 <- read_delim("cxt_mg_12dpi_2_S63_true_barcodes.txt", 
                 col_types = column_classes, col_names = c("barcode", "countMG2"))
MG3 <- read_delim("cxt_mg_12dpi_3_S64_true_barcodes.txt", 
                 col_types = column_classes, col_names = c("barcode", "countMG3"))
MG4 <- read_delim("cxt_mg_12dpi_4_S65_true_barcodes.txt", 
                  col_types = column_classes, col_names = c("barcode", "countMG4"))
MG5 <- read_delim("cxt_mg_12dpi_5_S66_true_barcodes.txt", 
                  col_types = column_classes, col_names = c("barcode", "countMG5"))
MG6 <- read_delim("cxt_mg_12dpi_6_S67_true_barcodes.txt", 
                  col_types = column_classes, col_names = c("barcode", "countMG6"))
MG7 <- read_delim("cxt_mg_12dpi_7_S68_true_barcodes.txt", 
                  col_types = column_classes, col_names = c("barcode", "countMG7"))
MG8 <- read_delim("cxt_mg_12dpi_8_S69_true_barcodes.txt", 
                  col_types = column_classes, col_names = c("barcode", "countMG8"))
MG9 <- read_delim("cxt_mg_12dpi_9_S70_true_barcodes.txt", 
                  col_types = column_classes, col_names = c("barcode", "countMG9"))
MG10 <- read_delim("cxt_mg_12dpi_10_S71_true_barcodes.txt", 
                  col_types = column_classes, col_names = c("barcode", "countMG10"))

#input file
setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/Sequencing/all_counts/stocks/true_counts/")
input <- read_delim("P2_merge_input.txt", 
                    col_types = column_classes, col_names = c("barcode", "count_input"))

#generate barcode frequencies for each "replicate" or sample type:
totalumis_MG1 <- sum(MG1$countMG1)
MG1$freq_MG1 <- (MG1$countMG1)/totalumis_MG1

totalumis_MG2 <- sum(MG2$countMG2)
MG2$freq_MG2 <- (MG2$countMG2)/totalumis_MG2

totalumis_MG3 <- sum(MG3$countMG3)
MG3$freq_MG3 <- (MG3$countMG3)/totalumis_MG3

totalumis_MG4 <- sum(MG4$countMG4)
MG4$freq_MG4 <- (MG4$countMG4)/totalumis_MG4

totalumis_MG5 <- sum(MG5$countMG5)
MG5$freq_MG5 <- (MG5$countMG5)/totalumis_MG5

totalumis_MG6 <- sum(MG6$countMG6)
MG6$freq_MG6 <- (MG6$countMG6)/totalumis_MG6

totalumis_MG7 <- sum(MG7$countMG7)
MG7$freq_MG7 <- (MG7$countMG7)/totalumis_MG7

totalumis_MG8 <- sum(MG8$countMG8)
MG8$freq_MG8 <- (MG8$countMG8)/totalumis_MG8

totalumis_MG9 <- sum(MG9$countMG9)
MG9$freq_MG9 <- (MG9$countMG9)/totalumis_MG9

totalumis_MG10 <- sum(MG10$countMG10)
MG10$freq_MG10 <- (MG10$countMG10)/totalumis_MG10

totalumis_input <- sum(input$count_input)
input$freq_input <- (input$count_input)/totalumis_input

#merge frequency tables (note: merge can only do 2 dfs at a time)
MG1_2 <- merge(MG1, MG2, by = "barcode", all.x = TRUE, all.y = TRUE) 
MG12_3 <- merge(MG1_2, MG3, by = "barcode", all.x = TRUE, all.y = TRUE)
MG123_4 <- merge(MG12_3, MG4, by = "barcode", all.x = TRUE, all.y = TRUE)
MG1234_5 <- merge(MG123_4, MG5, by = "barcode", all.x = TRUE, all.y = TRUE)
MG12345_6 <- merge(MG1234_5, MG6, by = "barcode", all.x = TRUE, all.y = TRUE)
MG123456_7 <- merge(MG12345_6, MG7, by = "barcode", all.x = TRUE, all.y = TRUE)
MG1234567_8 <- merge(MG123456_7, MG8, by = "barcode", all.x = TRUE, all.y = TRUE)
MG12345678_9 <- merge(MG1234567_8, MG9, by = "barcode", all.x = TRUE, all.y = TRUE)
MG_all <- merge(MG12345678_9, MG10, by = "barcode", all.x = TRUE, all.y = TRUE)
all_samples <- merge(MG_all, input, by = "barcode", all.x = TRUE, all.y = TRUE)

#make NA entries '0'
all_samples[is.na(all_samples)]<-0

#select columns of interest for graph data
graph_data <- all_samples %>%
  select(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23) 

#choosing to skip percent generating step, ordering by freq. They end up the same. 
#can also skip arranging by lowest diversity sample and assigning rank that way
#just arrange desc by input
graph_data <- graph_data %>%
  arrange(desc(freq_input))

#generate separate dataframes so each element of the X axis gets its own rank
sepMG1 <- graph_data[c("freq_MG1")]
sepMG1 <- sepMG1 %>%
  mutate(sample = 1) %>%
  mutate(barcode_id = 1:nrow(sepMG1)) %>%
  dplyr::rename("freq" = "freq_MG1")

sepMG2 <- graph_data[c("freq_MG2")]
sepMG2 <- sepMG2 %>%
  mutate(sample = 2) %>%
  mutate(barcode_id = 1:nrow(sepMG2)) %>%
  dplyr::rename("freq" = "freq_MG2")

sepMG3 <- graph_data[c("freq_MG3")]
sepMG3 <- sepMG3 %>%
  mutate(sample = 3) %>%
  mutate(barcode_id = 1:nrow(sepMG3)) %>%
  dplyr::rename("freq" = "freq_MG3")

sepMG4 <- graph_data[c("freq_MG4")]
sepMG4 <- sepMG4 %>%
  mutate(sample = 4) %>%
  mutate(barcode_id = 1:nrow(sepMG4)) %>%
  dplyr::rename("freq" = "freq_MG4")

sepMG5 <- graph_data[c("freq_MG5")]
sepMG5 <- sepMG5 %>%
  mutate(sample = 5) %>%
  mutate(barcode_id = 1:nrow(sepMG5)) %>%
  dplyr::rename("freq" = "freq_MG5")

sepMG6 <- graph_data[c("freq_MG6")]
sepMG6 <- sepMG6 %>%
  mutate(sample = 6) %>%
  mutate(barcode_id = 1:nrow(sepMG6)) %>%
  dplyr::rename("freq" = "freq_MG6")

sepMG7 <- graph_data[c("freq_MG7")]
sepMG7 <- sepMG7 %>%
  mutate(sample = 7) %>%
  mutate(barcode_id = 1:nrow(sepMG7)) %>%
  dplyr::rename("freq" = "freq_MG7")

sepMG8 <- graph_data[c("freq_MG8")]
sepMG8 <- sepMG8 %>%
  mutate(sample = 8) %>%
  mutate(barcode_id = 1:nrow(sepMG8)) %>%
  dplyr::rename("freq" = "freq_MG8")

sepMG9 <- graph_data[c("freq_MG9")]
sepMG9 <- sepMG9 %>%
  mutate(sample = 9) %>%
  mutate(barcode_id = 1:nrow(sepMG9)) %>%
  dplyr::rename("freq" = "freq_MG9")

sepMG10 <- graph_data[c("freq_MG10")]
sepMG10 <- sepMG10 %>%
  mutate(sample = 10) %>%
  mutate(barcode_id = 1:nrow(sepMG10)) %>%
  dplyr::rename("freq" = "freq_MG10")

sep_input <- graph_data[c("freq_input")]
sep_input <- sep_input %>%
  mutate(sample = 0) %>%
  mutate(barcode_id = 1:nrow(sep_input)) %>%
  dplyr::rename("freq" = "freq_input")

#bind these dataframes and arrange desc by barcode_id
frequencies <- rbind(sepMG1, sepMG2, sepMG3, sepMG4, sepMG5, sepMG6, sepMG7, sepMG8, 
                     sepMG9, sepMG10, sep_input) %>%
  arrange(desc(barcode_id)) %>%
  mutate(barcode_id = as.factor(barcode_id))

#graph - note if getting error "insufficient values in manual scale" increase the value of the
#number after the color vector. 
x<-(rep(c("red","green","blue","yellow","purple","grey","orange","brown","turquoise","pink",
          "cyan","darkgrey"),10000))

breaks <- c("Input", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")

#have to include 'stat="identity"' when plotting this data with geom_bar
ggplot(frequencies, aes(x = sample, y = freq, fill = barcode_id)) + 
  geom_bar(stat = "identity", alpha = 0.6, show.legend = FALSE) +
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        panel.background = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "light grey"), panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  theme(axis.line.x = element_blank(), axis.line.y = element_blank()) +
  theme(axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18)) +
  theme(axis.ticks.length = unit(0.25, "cm")) +
  theme(axis.title.x = element_text(size = 1), axis.title.y = element_text(size = 1)) +
  theme(plot.margin = unit(c(0.5,0.1,0.1,0.1), "cm")) +
  ggtitle("cxt_12dpi_MG_barcode_maintenance") +
  theme(plot.title = element_text(vjust = 4)) +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10), labels = breaks, expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,1, by=0.2), expand = c(0,0)) +
  scale_fill_manual(values = x)

setwd("/Users/emilyfitzmeyer/Desktop/maintenance_plots")
ggsave("cxt_12dpi_mg.png", plot = last_plot(), device = png(), scale = 1, width = 6.5, height = 5, dpi = 300)


