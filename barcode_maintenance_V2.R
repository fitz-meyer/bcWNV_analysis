library(tidyverse)
library(magrittr)

#set working directory and read in files:
#change path to location of files.
#sample files
getwd()
setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/Sequencing/all_counts/whole_tissue/true_counts/")
column_classes <- c("c", "n")

#read in all MG, or SG, or SL samples - comment out 'MG' dataframes as necessary
MG1 <- read_delim("aea_mg_8dpi_30_S127_true_barcodes.txt", 
                 col_types = column_classes, col_names = c("barcode", "count"))
MG2 <- read_delim("aea_mg_8dpi_31_S128_true_barcodes.txt", 
                 col_types = column_classes, col_names = c("barcode", "count"))
MG3 <- read_delim("aea_mg_8dpi_33_S129_true_barcodes.txt", 
                 col_types = column_classes, col_names = c("barcode", "count"))
MG4 <- read_delim("aea_mg_8dpi_5_S122_true_barcodes.txt", 
                  col_types = column_classes, col_names = c("barcode", "count"))
MG5 <- read_delim("aea_mg_8dpi_6_S123_true_barcodes.txt", 
                  col_types = column_classes, col_names = c("barcode", "count"))
MG6 <- read_delim("aea_mg_8dpi_8_S124_true_barcodes.txt", 
                  col_types = column_classes, col_names = c("barcode", "count"))
MG7 <- read_delim("cxt_mg_4dpi_4_S5_true_barcodes.txt",
                 col_types = column_classes, col_names = c("barcode", "count"))
MG8 <- read_delim("cxt_mg_4dpi_5_S6_true_barcodes.txt",
                 col_types = column_classes, col_names = c("barcode", "count"))
MG9 <- read_delim("cxt_mg_4dpi_8_S7_true_barcodes.txt",
                 col_types = column_classes, col_names = c("barcode", "count"))

#input file
setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/Sequencing/all_counts/stocks/true_counts/")
input <- read_delim("P2_merge_input.txt", 
                    col_types = column_classes, col_names = c("barcode", "count_input"))

names <- c("MG1", "MG2", "MG3", "MG4", "MG5", "MG6",
           "MG7", "MG8", "MG9", "input")

list1 <- list(MG1, MG2, MG3, MG4, MG5, MG6,
              MG7, MG8, MG9, input)

names(list1) <- names

#create function to generate frequency columns for each df and preserve + append df name to numeric columns
barcode_frequency <- function(x, name) {
  totalumis <- sum(x[["count"]])
  x[["freq"]] <- ((x[["count"]])/totalumis)
  names(x) <- paste(names(x), name, sep = "_")
  colnames(x)[1] = "barcode"
  print(x)
}

#map function over list
list2 <- map2(list1, names(list1), barcode_frequency)

#merge dfs by barcode
all_samples <- list2 %>%
  reduce(full_join, by = "barcode")

#make NA entries '0'
all_samples[is.na(all_samples)]<-0

#select frequency columns for graph data - adjust to match sample number
graph_data <- all_samples %>%
  select(1, 3, 5, 7, 9, 11, 13, 15) 

#arrange desc by input
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

sep_input <- graph_data[c("freq_input")]
sep_input <- sep_input %>%
  mutate(sample = 0) %>%
  mutate(barcode_id = 1:nrow(sep_input)) %>%
  dplyr::rename("freq" = "freq_input")

#bind these dataframes and arrange desc by barcode_id
frequencies <- rbind(sepMG1, sepMG2, sepMG3, sepMG4, sepMG5, sepMG6, sepMG9, sep_input) %>%
  arrange(desc(barcode_id)) %>%
  mutate(barcode_id = as.factor(barcode_id))

#graph - note if getting error "insufficient values in manual scale" increase the value of the
#number after the color vector. 
x<-(rep(c("red","green","blue","yellow","purple","grey","orange","brown","turquoise","pink",
          "cyan","darkgrey"),10000))

breaks <- c("Input", "1", "2", "3", "4", "5", "6", "7", "8", "9")

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
  ggtitle("cxt_4dpi_MG_barcode_maintenance") +
  theme(plot.title = element_text(vjust = 4)) +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6), labels = breaks, expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,1, by=0.2), expand = c(0,0)) +
  scale_fill_manual(values = x)

setwd("/Users/emilyfitzmeyer/Desktop/maintenance_plots")
ggsave("aea_8dpi_mg.png", plot = last_plot(), device = png(), scale = 1, width = 6.5, height = 5, dpi = 300)

