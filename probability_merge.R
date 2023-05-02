library(tidyverse)
library(magrittr)

#this script merges the .csv output files of either transmission_probability, sg_infection_probability or mg_infection_probability. 
#change compartment name and species code as needed (where indicated)

#set working directory to directory containing the "X_prob" directory:
#change compartment name
setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/whole_tissue/probabilities/est_input/transmission_probabilities/")

#list all files:
#change species code
filenames <- list.files(path = "cxt")

#create list of dataframe names:
#names <- str_replace(filenames, "_mg_infection_prob.csv", "")
#names <- str_replace(filenames, "_sg_infection_prob.csv", "")
names <- str_replace(filenames, "_transmission_prob.csv", "")

#set working director to directory containing files
#change compartment name and species code
setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/whole_tissue/probabilities/est_input/transmission_probabilities/cxt/")
filelist <- lapply(filenames, read_csv, col_types = cols(.default = "c"))

#assign names to dataframes:
names(filelist) <- names

#remove all but "barcode" "count" and "b1xb2" columns for each element of list:
selected_filelist <- filelist %>%
#for mg
  #map(function(x) {select(x, 1, 2, 3)})
#for sg
  #map(function(x) {select(x, 1, 2, 5)})
#for transmission
  map(function(x) {select(x, 1, 2, 6)})

#merge all dfs in list by barcode 
#select only columns 1, 2 (barcode and stock count) and columns containing "b1xb2"
df_merge <- selected_filelist %>%
  reduce(full_join, by = 'barcode') %>%
#for mg
  #select(1:2, contains("b1")) 
#for sg
  #select(1:2, contains("b1xb2")) 
#for transmission
  select(1:2, contains("b1xb2xb3"))

#coerce columns 2:ncol from chr to num/dbl
df_merge[,2:ncol(df_merge)] <- lapply(df_merge[,2:ncol(df_merge)], as.numeric)

#make NA entries '0' - if needed
df_merge[is.na(df_merge)]<-0

#average all b1xb2 columns, ignoring NAs:
df_merge <- df_merge %>%
  mutate(mean = rowMeans(df_merge[,3:ncol(df_merge)], na.rm = TRUE))
#for some reason piping into the second 'mutate' doesn't work. It sees 'mean' as an undefined column. 
df_merge %>% 
  mutate(mean = sprintf(df_merge$mean, fmt = '%#.12f'))

setwd("/Users/emilyfitzmeyer/Desktop/BC_WNV/whole_tissue/probabilities/est_input/transmission_probabilities/")
write_csv(df_merge, "cxt_merge_transmission_prob.csv")





