library(tidyverse)

# set working directory to directory containing the "rarefaction_files" directory:
setwd("/Users/emilyfitzmeyer/Desktop/")

# list all files:
filenames <- list.files(path = "mgi_rare")

# create list of dataframe names from first 15 characters of filename (or relevant length):
#names <- substr(filenames, 1, 15)
# I like this way better bc I hate when only some filenames have trailing underscores:
names <- str_replace(filenames, "_barcodes.txt", "")

# set working directory to "rarefaction_files" directory:
setwd("/Users/emilyfitzmeyer/Desktop/mgi_rare")

# load all files with lapply [if this returns a "no lines available in input" error try 
# switching read.delim to read_delim]
# specify column names to avoid having first row of each dataframe turned into column names:
filelist <- lapply(filenames, read_csv, col_names = c("barcode"), col_types = cols(.default = "c"))

# Assign names to dataframes:
names(filelist) <- names

# behold the shitty workaround - wherein I tried to name every element of the list "barcodes" so I wouldn't
# have to change the input of my function.
#shitty_work_around <- rep(c("barcodes"), times = 231)
#names(filelist) <- shitty_work_around

# currently the input of the function (line 36) and part of the loop (line 46) expect a df called "barcodes" as input 
#test <- read_csv("cxt_mg_12dpi_4_S65_barcodes.txt", col_names = c("barcode"), 
#                     col_types = cols(.default = "c"))

# make function out of subsampling for loop:
bootstrap_rarefaction <- function (x) { 
  
  dataset_size <- nrow(x)
  subsample_offset <- 10000
  subsample_sizes <- seq(from = 0, to = dataset_size, by = subsample_offset)
  
  all_data <- data.frame(subsample_size = integer(),
                         richess = integer())
  
  for (subsample_size in subsample_sizes) {
    subsample <- slice_sample(x, n = subsample_size, replace = T)
    richness <- nrow(subsample %>% 
                       select(barcode) %>% 
                       distinct())
    output <- c(subsample_size, richness)
    all_data <- rbind.data.frame(all_data, output)
  }
all_data
}

#test w/ barcodes
#temp <- bootstrap_rarefaction(test)

# map function over list:
new_filelist <- map(filelist, bootstrap_rarefaction)

setwd("/Users/emilyfitzmeyer/Desktop/")
capture.output(new_filelist, file = "YA_bcWNV_P2_stock.csv") 




