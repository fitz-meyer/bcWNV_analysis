library(tidyverse)
library(magrittr)

#set working directory and read in files:
#change path to location of files.
#sample files
getwd()
setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/Sequencing/all_counts/whole_tissue/true_counts/")
column_classes <- c("c", "n")

#make sure you are reading in samples from the same dpi and mosquito!
MG <- read_delim("cxt_mg_12dpi_9_S70_true_barcodes.txt", 
                      col_types = column_classes, col_names = c("barcode", "count"))
SG <- read_delim("cxt_sg_12dpi_9_S80_true_barcodes.txt", 
                       col_types = column_classes, col_names = c("barcode", "count"))
SL <- read_delim("cxt_sl_12dpi_9_S60_true_barcodes.txt", 
                       col_types = column_classes, col_names = c("barcode", "count"))

#input file
setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/Sequencing/all_counts/stocks/true_counts/")
input <- read_delim("P2_merge_input.txt", 
                       col_types = column_classes, col_names = c("barcode", "count"))

names <- c("MG", "SG", "SL", "input")

list1 <- list(MG, SG, SL, input)

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

#select columns of interest for graph data
graph_data <- all_samples %>%
  select(1, 3, 5, 7, 9) 

#arrange by MG or input 
graph_data <- graph_data %>%
  arrange(desc(freq_MG))

#generate separate dataframes so each element of the X axis gets its own rank
sepMG <- graph_data[c("freq_MG")]
sepMG <- sepMG %>%
  mutate(sample = 1) %>%
  mutate(barcode_id = 1:nrow(sepMG)) %>%
  dplyr::rename("freq" = "freq_MG")

sepSG <- graph_data[c("freq_SG")]
sepSG <- sepSG %>%
  mutate(sample = 2) %>%
  mutate(barcode_id = 1:nrow(sepSG)) %>%
  dplyr::rename("freq" = "freq_SG")

sepSL <- graph_data[c("freq_SL")]
sepSL <- sepSL %>%
  mutate(sample = 3) %>%
  mutate(barcode_id = 1:nrow(sepSL)) %>%
  dplyr::rename("freq" = "freq_SL")

sep_input <- graph_data[c("freq_input")]
sep_input <- sep_input %>%
  mutate(sample = 0) %>%
  mutate(barcode_id = 1:nrow(sep_input)) %>%
  dplyr::rename("freq" = "freq_input")

#bind these dataframes and arrange desc by barcode_id
frequencies <- rbind(sepMG, sepSG, sepSL, sep_input) %>%
  arrange(desc(barcode_id)) %>%
  mutate(barcode_id = as.factor(barcode_id)) #%>%
  #mutate(volume = as.factor(sample))

#graph
x<-(rep(c("red","green","blue","yellow","purple","grey","orange","brown","turquoise","pink","cyan","darkgrey"),1500))

breaks <- c("Input", "MG", "SG", "SL")

ggplot(frequencies, aes(x = sample, y = freq, fill = barcode_id)) + 
  geom_area(alpha = 0.6, show.legend = FALSE) +
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "light grey"), panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  theme(axis.line.x = element_blank(), axis.line.y = element_blank()) +
  theme(axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18)) +
  theme(axis.ticks.length = unit(0.25, "cm")) +
  theme(axis.title.x = element_text(size = 1), axis.title.y = element_text(size = 1)) +
  theme(plot.margin = unit(c(0.3,1,0.3,0.3), "cm")) +
  ggtitle("cxt_9_12dpi") +
  theme(plot.title = element_text(vjust = 4)) +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = c(0,1,2,3), labels = stringr::str_wrap(breaks, width = 8), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,1, by=0.2), expand = c(0,0)) +
  scale_fill_manual(values = x)

#setwd("/Users/emilyfitzmeyer/Desktop/prog_plots/Supplemental/")
#ggsave("cxt_9_12dpi.png", plot = last_plot(), device = png(), scale = 1, width = 6.5, height = 5, dpi = 300)

