library(tidyverse)
library(QSutils)
library(vegan)
library(fs)

#assign filepath to "data_path":
data_path <- "/Users/emilyfitzmeyer/Desktop/BC_WNV/mgi_bcWNV/mgi_bcWNV_trueBC/"

#txt_files <- dir(data_path, pattern = "*.txt")
txt_files <- fs::dir_ls(data_path)

#assign column classes to "column_classes" (use shortcut values): 
column_classes <- c("c", "n")

# read all files in using map_dfr
# specify column types and column names ('counts' column tends to read in as both 'chr' and 'dbl'when col types aren't specified) 
# mutate source column to cut down file path length:
# change '65' to whatever number of characters in path precedes your filename +1
# use str_replace to remove redundant parts of filename "_barcode_counts.txt"
df <- txt_files %>%
  map_dfr(read_delim, col_types = column_classes, col_names = c("barcodes", "counts"), .id = "source") %>%
  mutate(source = str_sub(source, 65, -1)) %>%
  mutate(source = str_replace(source, "_true_barcodes.txt", ""))

# nest by "source" (the filenames)
# pull the 'counts' columns from the nested dataframe and use Shannon() to determine sample complexity
nested_df <- df %>%
  group_by(source) %>%
  nest() %>%
  mutate(complexity = 
           map(data, ~ .x %>%
                 pull(counts) %>% 
                 QSutils::Shannon()))

# add a column for normalized Shannon values using QSutils::NormShannon
# we don't trust NormShannon yet but everyone likes to be included
nested_df <- nested_df %>%
  mutate(norm_complexity =
           map(data, ~ .x %>%
                 pull(counts) %>%
                 QSutils::NormShannon()))

# repeat with Length() to get # of unique barcodes (note, nrow() and n() do not work here because
# in list format 'counts' is a vector not a column)
nested_df <- nested_df %>%
  mutate(unique_barcodes = 
           map(data, ~ .x %>%
                 pull(counts) %>% 
                 length()))

# sum abundance vectors and make a column called "total_barcodes" with these values.
nested_df <- nested_df %>%
  mutate(total_barcodes = 
           map(data, ~ .x %>%
                 pull(counts) %>% 
                 sum()))

# select columns of interest 
# not selecting for 'Data' column because upon unnesting this will generate an unweildy and confusing dataframe
# if you don't specify 'cols' it works but throws an error about it  
exp_df <- nested_df %>%
  select(1, 3, 4, 5, 6) %>%
  unnest(cols = c(complexity, norm_complexity, unique_barcodes, total_barcodes))

# export new df as a csv file (specify filename after '\') 
# weird thing: using '\' before filename cuts of the first letter of the filename 
# using '\\' before filename includes a '\' in the filename
# NOTE: if you are already in the directory where you want the file to go you do not have to specify a path
# if you specify a path anyway your file will not show up, set appropriate working directory before writing out file 
setwd("/Users/emilyfitzmeyer/Desktop")
write_csv(exp_df, "mgi_sample_diversity.csv")













#EXTRAS


#scaling attempt 2:
#exp_df %>%
#  mutate(scaled_complexity = rescale(complexity)) %>%
#  mutate(scaled_complexity = sprintf(scaled_complexity, fmt = '%#.5f'))

#even when using sprintf to add decimal points to the scaled values the scaling simplifies
#a bunch of unique complexity values 0.5.
#I want to retain the differences in these values!!!!

#setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/Sequencing/ffbcWNV_NextSeq/barcode_counts/stocks")
#test_file <- read_delim("3_P1_200_S349_barcode_counts.txt", col_types = c("c", "n"), col_names = c("barcodes", "counts"))
#iNEXT(test_file$counts, q = c(0,1,2), datatype = "abundance")

#Shannon(test_file$counts)

#scaling attempt 1:

#this is not a great way to do what I was trying to do, it will force high complexity samples in small datasets
#to be presented as "low" complexity. 

#create function that will rescale the diversity index vector from 0 to 1:
#range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# generate a new column 'complexity_01' containing complexity values normalized to a 0 to 1 scale
#exp_df <- exp_df %>%
 # mutate(complexity_01 = range01(complexity))
